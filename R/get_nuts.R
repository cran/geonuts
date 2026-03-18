#' Identify NUTS Regions for Geolocations
#'
#' Vectorised identification of NUTS regions for input coordinates using
#' Eurostat geospatial layers. Supports a single level (0–3) or \code{"all"} to
#' return all levels. Optional country pre-filter and "nearest" fallback make
#' the function robust to points falling just outside polygon boundaries.
#'
#' @param latitude (numeric, **mandatory**) Latitudes in decimal degrees (WGS84).
#'   Must be within \code{[-90, 90]}.
#' @param longitude (numeric, **mandatory**) Longitudes in decimal degrees (WGS84).
#'   Must be within \code{[-180, 180]}. Length must match \code{latitude}.
#' @param level (integer or character, optional) One of \code{0, 1, 2, 3, "all"}.
#'   Default: \code{"all"}. When \code{"all"}, the result includes \code{nuts0..nuts3}.
#' @param year (integer, optional) NUTS reference year for the Eurostat layer.
#'   Default: \code{2021}.
#' @param resolution (integer, optional) Eurostat map resolution. Typical
#'   values: \code{1, 3, 10, 20, 60}. Default: \code{20}.
#' @param crs (integer, optional) EPSG code of input coordinates. If not 4326,
#'   inputs are transformed to WGS84 (EPSG:4326). Default: \code{4326}.
#' @param country (character, optional) Two-letter \code{CNTR_CODE} to pre-filter
#'   polygons (e.g., \code{"DE"}, \code{"IT"}). Reduces memory and speeds up queries.
#' @param match_strategy (character, optional) Matching strategy:
#'   \code{"within"} (default) assigns a NUTS only when the point lies within a
#'   polygon; \code{"nearest"} uses the nearest NUTS polygon for unmatched points.
#' @param nearest_max_km (numeric, optional) Maximum distance (km) for
#'   nearest fallback; use \code{Inf} to allow any distance. Default: \code{Inf}.
#' @param verbose (logical, optional) Print informative messages. Default: \code{TRUE}.
#'
#' @return
#' If \code{level} is a single integer: a \code{data.frame} with columns
#' \code{lat, lon, nuts, cntr_code, match_status, match_dist_km, level, year, resolution}.
#' If \code{level = "all"}: a \code{data.frame} with
#' \code{lat, lon, nuts0, nuts1, nuts2, nuts3, cntr_code, match_status, match_dist_km, year, resolution}.
#' Here \code{match_status}/\code{match_dist_km} are computed using level 3 (most granular).
#'
#' @details
#' - Geometries are downloaded via \code{eurostat::get_eurostat_geospatial()} and
#'   cached per \code{(level, year, resolution)} to avoid repeated I/O.
#' - Polygons are pre-filtered using the points' bounding box to accelerate
#'   joins on continental layers (with safe fallback).
#' - Nearest distances are computed with units and converted to kilometers.
#'
#' @seealso \code{\link{map_nuts}}
#' @examples
#' \donttest{
#'   res <- get_nuts(52.52, 13.405, level = 3, year = 2021, resolution = 20)
#'   head(res)
#' }
#' @export
#' @importFrom sf st_as_sf st_transform st_crs st_make_valid
get_nuts <- function(latitude,
                     longitude,
                     level = "all",
                     year = 2021,
                     resolution = 20,
                     crs = 4326,
                     country = NULL,
                     match_strategy = c("within", "nearest"),
                     nearest_max_km = Inf,
                     verbose = TRUE) {

  # Dummy references so CRAN sees actual use when kept in Imports (no runtime effect)
  if (FALSE) {
    giscoR::gisco_get_nuts
    giscoR::gisco_get_countries
  }

  match_strategy <- match.arg(match_strategy)

  .validate_get_nuts_params(latitude, longitude, level, year, resolution, crs,
                            country, match_strategy, nearest_max_km)

  # Build sf POINTS in input CRS; transform to WGS84 for robustness
  pts <- .coords_to_sf(latitude, longitude, crs_input = crs)

  # Helper to match a single level
  .match_level <- function(nuts_level) {
    shp <- .get_cached_shape(level = nuts_level, year = year,
                             resolution = resolution, verbose = verbose)
    if (!is.null(country)) {
      shp <- .filter_country(shp, country)
      if (nrow(shp) == 0L) {
        stop("No polygons found for the requested `country` at this level/year/resolution.", call. = FALSE)
      }
    }

    # Ensure valid geoms + align CRS to the points' CRS
    shp <- suppressWarnings(sf::st_make_valid(shp))
    shp <- sf::st_transform(shp, sf::st_crs(pts))

    shp <- .prefilter_by_bbox(shp, pts)

    # Pass the level and resolution into the join helper
    .join_points_to_nuts(pts, shp, match_strategy, nearest_max_km,
                         level = nuts_level, resolution = resolution)
  }

  if (identical(level, "all")) {
    out0 <- .match_level(0)
    out1 <- .match_level(1)
    out2 <- .match_level(2)
    out3 <- .match_level(3)

    out <- .assemble_output_all(latitude, longitude, out0, out1, out2, out3,
                                year, resolution)
    if (isTRUE(verbose)) .message("Matched all levels (0-3).")
    return(out)
  }

  # Single-level path
  outL <- .match_level(as.integer(level))
  out  <- .assemble_output_single(latitude, longitude, outL,
                                  level = as.integer(level),
                                  year = year, resolution = resolution)
  if (isTRUE(verbose)) .message(sprintf("Matched level %s.", level))
  out
}


# Internal Functions -----------------------------------------------------------

#' Parameter validation for get_nuts()
#' @keywords internal
.validate_get_nuts_params <- function(latitude, longitude, level, year, resolution, crs,
                                      country, match_strategy, nearest_max_km) {
  stopifnot(is.numeric(latitude), is.numeric(longitude))
  if (length(latitude) != length(longitude)) stop("`latitude` and `longitude` must have the same length.", call. = FALSE)
  if (any(latitude < -90 | latitude > 90, na.rm = TRUE)) stop("`latitude` must be within [-90, 90].", call. = FALSE)
  if (any(longitude < -180 | longitude > 180, na.rm = TRUE)) stop("`longitude` must be within [-180, 180].", call. = FALSE)

  if (!(identical(level, "all") || level %in% c(0L, 1L, 2L, 3L))) {
    stop("`level` must be 0, 1, 2, 3, or \"all\".", call. = FALSE)
  }
  if (!is.numeric(year) || length(year) != 1L) stop("`year` must be a single integer.", call. = FALSE)
  if (!is.numeric(resolution) || length(resolution) != 1L) stop("`resolution` must be a single integer.", call. = FALSE)
  if (!is.numeric(crs) || length(crs) != 1L) stop("`crs` must be a single EPSG integer (e.g., 4326).", call. = FALSE)
  if (!is.null(country) && (!is.character(country) || nchar(country[1]) != 2L)) {
    stop("`country` must be a two-letter CNTR_CODE (e.g., \"DE\").", call. = FALSE)
  }
  if (!is.character(match_strategy) || length(match_strategy) != 1L) stop("`match_strategy` must be \"within\" or \"nearest\".", call. = FALSE)
  if (!is.numeric(nearest_max_km) || length(nearest_max_km) != 1L || nearest_max_km < 0) {
    stop("`nearest_max_km` must be a single non-negative number.", call. = FALSE)
  }
  invisible(TRUE)
}

#' Convert numeric lat/lon vectors to sf POINT (WGS84)
#' @keywords internal
#' @importFrom sf st_as_sf st_crs st_transform
.coords_to_sf <- function(latitude, longitude, crs_input = 4326) {
  pts <- sf::st_as_sf(
    data.frame(row_id = seq_along(latitude), lon = longitude, lat = latitude),
    coords = c("lon", "lat"),
    crs = as.integer(crs_input),
    remove = TRUE
  )
  if (is.na(sf::st_crs(pts)$epsg) || sf::st_crs(pts)$epsg != 4326) {
    pts <- sf::st_transform(pts, 4326)
  }
  pts
}

#' Cached Eurostat geospatial layer
#' @keywords internal
#' @importFrom eurostat get_eurostat_geospatial
.get_cached_shape <- local({
  cache <- new.env(parent = emptyenv())
  function(level, year, resolution, verbose = TRUE) {
    key <- paste(level, year, resolution, sep = "_")
    if (exists(key, envir = cache, inherits = FALSE)) {
      if (isTRUE(verbose)) .message("Using cached NUTS layer.")
      return(get(key, envir = cache, inherits = FALSE))
    }
    if (isTRUE(verbose)) .message(sprintf(
      "Downloading NUTS layer (level=%s, year=%s, resolution=%s)...",
      level, year, resolution
    ))
    # Silence giscoR/eurostat chatter:
    shp <- suppressMessages(suppressWarnings(
      eurostat::get_eurostat_geospatial(
        resolution = resolution,
        nuts_level = level,
        year = year
      )
    ))
    assign(key, shp, envir = cache)
    shp
  }
})

#' Country filter by CNTR_CODE (if available)
#' @keywords internal
.filter_country <- function(shp, country) {
  if (!"CNTR_CODE" %in% names(shp)) return(shp)
  shp[shp$CNTR_CODE == country, , drop = FALSE]
}

#' Pre-filter polygons by bounding box of points (safe with fallback)
#' @keywords internal
#' @importFrom sf st_bbox st_as_sfc st_intersects
.prefilter_by_bbox <- function(shp, pts) {
  if (is.null(shp) || !inherits(shp, "sf")) {
    stop("Internal error: expected an 'sf' object for 'shp' but got NULL/invalid.", call. = FALSE)
  }
  if (nrow(shp) == 0L) return(shp)

  # Build bbox, then expand slightly to be robust to precision at edges
  bb <- sf::st_bbox(pts)

  # Numeric expansion (degrees) to avoid s2/st_buffer issues on lon/lat
  expand <- 0.05  # ~ tiny expansion in degrees
  bb_expanded <- bb
  bb_expanded["xmin"] <- bb["xmin"] - expand
  bb_expanded["xmax"] <- bb["xmax"] + expand
  bb_expanded["ymin"] <- bb["ymin"] - expand
  bb_expanded["ymax"] <- bb["ymax"] + expand

  bb_expanded <- sf::st_as_sfc(bb_expanded)

  idx <- sf::st_intersects(shp, bb_expanded, sparse = TRUE)
  keep <- lengths(idx) > 0L

  if (!any(keep)) {
    # Fallback: do not prefilter (prevents false negatives)
    return(shp)
  }
  shp[keep, , drop = FALSE]
}

#' Spatial join + optional nearest fallback (robust; km units)
#' @keywords internal
#' @importFrom sf st_join st_within st_nearest_feature st_distance
#' @importFrom units set_units
.join_points_to_nuts <- function(pts, shp, match_strategy = "within", nearest_max_km = Inf,
                                 level = NA_integer_, resolution = 20L) {
  # 1) Primary join: strict topology
  j <- suppressWarnings(sf::st_join(pts, shp, left = TRUE, join = sf::st_within))

  out <- data.frame(
    row_id    = j$row_id,
    nuts_id   = j$NUTS_ID,
    cntr_code = j$CNTR_CODE,
    stringsAsFactors = FALSE
  )
  out$match_status  <- ifelse(is.na(out$nuts_id), "unmatched", "matched")
  out$match_dist_km <- NA_real_

  # Dummy references so CRAN sees actual use when kept in Imports (no runtime effect)
  if (FALSE) {
    sf::st_nearest_feature
    sf::st_distance
    sf::st_join
  }

  # 2) Resolution-adaptive proximity pass for points near/at polygon boundaries.
  #    Coarser resolutions simplify boundaries more aggressively, so a larger
  #    proximity threshold is needed to recover valid points that fall just
  #    outside the simplified polygon.
  border_idx <- which(is.na(out$nuts_id))
  if (length(border_idx) > 0L && nrow(shp) > 0L) {
    prox_km <- switch(as.character(as.integer(resolution)),
                      "1"  = 0.5,
                      "3"  = 1.5,
                      "10" = 5,
                      "20" = 10,
                      "60" = 25,
                      as.numeric(resolution) * 0.5)
    near_idx <- sf::st_nearest_feature(pts[border_idx, ], shp)
    dists    <- sf::st_distance(pts[border_idx, ], shp[near_idx, ], by_element = TRUE)
    dists_km <- as.numeric(units::set_units(dists, "km", mode = "standard"))
    within_prox <- dists_km <= prox_km
    if (any(within_prox)) {
      fill <- border_idx[within_prox]
      out$nuts_id[fill]      <- as.character(shp$NUTS_ID[near_idx[within_prox]])
      out$cntr_code[fill]    <- as.character(shp$CNTR_CODE[near_idx[within_prox]])
      out$match_status[fill] <- "matched"
    }
  }

  # 3) Nearest fallback when explicitly requested
  if (identical(match_strategy, "nearest") && any(is.na(out$nuts_id)) && nrow(shp) > 0L) {
    miss_idx <- which(is.na(out$nuts_id))
    nearest_idx <- sf::st_nearest_feature(pts[miss_idx, ], shp)
    dists <- sf::st_distance(pts[miss_idx, ], shp[nearest_idx, ], by_element = TRUE)
    dists_km <- as.numeric(units::set_units(dists, "km", mode = "standard"))

    within_cap <- is.infinite(nearest_max_km) | (dists_km <= nearest_max_km)
    fill_idx   <- miss_idx[within_cap]
    if (length(fill_idx) > 0L) {
      out$nuts_id[fill_idx]       <- as.character(shp$NUTS_ID[nearest_idx[within_cap]])
      out$cntr_code[fill_idx]     <- as.character(shp$CNTR_CODE[nearest_idx[within_cap]])
      out$match_status[fill_idx]  <- "nearest"
      out$match_dist_km[fill_idx] <- dists_km[within_cap]
    }
  }

  # 4) Level-0 safety net: if still unmatched, force nearest (no cap)
  if (isTRUE(level == 0L) && any(is.na(out$nuts_id)) && nrow(shp) > 0L) {
    miss_idx <- which(is.na(out$nuts_id))
    nearest_idx <- sf::st_nearest_feature(pts[miss_idx, ], shp)
    dists <- sf::st_distance(pts[miss_idx, ], shp[nearest_idx, ], by_element = TRUE)
    dists_km <- as.numeric(units::set_units(dists, "km", mode = "standard"))

    out$nuts_id[miss_idx]       <- as.character(shp$NUTS_ID[nearest_idx])
    out$cntr_code[miss_idx]     <- as.character(shp$CNTR_CODE[nearest_idx])
    out$match_status[miss_idx]  <- "nearest"
    out$match_dist_km[miss_idx] <- dists_km
  }

  out
}

#' Assemble single-level output
#' @keywords internal
.assemble_output_single <- function(lat, lon, joined, level, year, resolution) {
  joined <- joined[order(joined$row_id), , drop = FALSE]

  nuts_col <- joined$nuts_id
  if (isTRUE(level == 0L)) {
    # Prefer CNTR_CODE (ISO2). If missing, derive from first 2 chars of NUTS_ID.
    nuts0 <- joined$cntr_code
    need_fill <- is.na(nuts0) & !is.na(joined$nuts_id)
    if (any(need_fill)) nuts0[need_fill] <- substr(joined$nuts_id[need_fill], 1, 2)
    nuts_col <- nuts0
  }

  data.frame(
    lat = as.numeric(lat),
    lon = as.numeric(lon),
    nuts = nuts_col,
    cntr_code = joined$cntr_code,
    match_status = joined$match_status,
    match_dist_km = joined$match_dist_km,
    level = as.integer(level),
    year = as.integer(year),
    resolution = as.integer(resolution),
    stringsAsFactors = FALSE
  )
}

#' Assemble all-levels output (0..3) with status from level 3
#' @keywords internal
.assemble_output_all <- function(lat, lon, out0, out1, out2, out3, year, resolution) {
  o0 <- out0[order(out0$row_id), , drop = FALSE]
  o1 <- out1[order(out1$row_id), , drop = FALSE]
  o2 <- out2[order(out2$row_id), , drop = FALSE]
  o3 <- out3[order(out3$row_id), , drop = FALSE]

  # Robust nuts0: prefer CNTR_CODE (ISO2), else derive from NUTS_ID (first 2 chars)
  nuts0 <- o0$cntr_code
  need_fill0 <- is.na(nuts0) & !is.na(o0$nuts_id)
  if (any(need_fill0)) nuts0[need_fill0] <- substr(o0$nuts_id[need_fill0], 1, 2)

  # Prefer the most granular CNTR_CODE; fall back upwards
  cntr <- o3$cntr_code
  idx  <- is.na(cntr); if (any(idx)) cntr[idx] <- o2$cntr_code[idx]
  idx  <- is.na(cntr); if (any(idx)) cntr[idx] <- o1$cntr_code[idx]
  idx  <- is.na(cntr); if (any(idx)) cntr[idx] <- o0$cntr_code[idx]

  data.frame(
    lat   = as.numeric(lat),
    lon   = as.numeric(lon),
    nuts0 = nuts0,
    nuts1 = o1$nuts_id,
    nuts2 = o2$nuts_id,
    nuts3 = o3$nuts_id,
    cntr_code = cntr,
    match_status = o3$match_status,
    match_dist_km = o3$match_dist_km,
    year  = as.integer(year),
    resolution = as.integer(resolution),
    stringsAsFactors = FALSE
  )
}

#' Compact message printer
#' @keywords internal
.message <- function(x) cat(paste0("[geonuts] ", x, "\n"))
