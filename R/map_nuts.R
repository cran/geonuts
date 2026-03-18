#' Plot NUTS Matches on a Map
#'
#' Visualises the frequency of matched NUTS regions and (optionally) overlays
#' the input points (matched vs. unmatched) for validation. Works with both
#' single-level and multi-level (\code{level = "all"}) outputs from \code{\link{get_nuts}}.
#'
#' @param nuts (data.frame, **mandatory**) Output of \code{\link{get_nuts}}. Must include
#'   \code{lat}, \code{lon}, and either \code{nuts} (single-level) or any of \code{nuts0..nuts3},
#'   plus \code{year}, \code{resolution}. If both single and multi-level columns exist,
#'   \code{map_level} selects which to display.
#' @param map_level (integer, optional) NUTS level to display when multiple
#'   levels are present. One of \code{0, 1, 2, 3}. Default: \code{3}.
#' @param country (character, optional) Two-letter \code{CNTR_CODE} to filter the
#'   map polygons (e.g., \code{"DE"}, \code{"FR"}). If \code{NULL}, uses all available regions.
#'   When supplied, the map is always zoomed to the full extent of the selected country.
#' @param show_points (logical, optional) Overlay input points. Default: \code{TRUE}.
#' @param border_col (character, optional) Polygon border colour. Default: \code{"lightgrey"}.
#' @param low_col (character, optional) Fill colour for lower frequencies. Default: \code{"lightgreen"}.
#' @param high_col (character, optional) Fill colour for higher frequencies. Default: \code{"darkgreen"}.
#' @param id_col (character, optional) Point colour for matched/nearest inputs. Default: \code{"black"}.
#' @param uid_col (character, optional) Point colour for unmatched inputs. Default: \code{"red"}.
#' @param verbose (logical, optional) Print informative messages. Default: \code{TRUE}.
#'
#' @return A \code{ggplot2} object showing a choropleth of NUTS frequencies with
#'   optional point overlays.
#'
#' @seealso \code{\link{get_nuts}}
#' @examples
#' \donttest{
#'   res <- get_nuts(52.52, 13.405, level = 3, year = 2021, resolution = 20)
#'   p <- map_nuts(res, map_level = 3)
#'   print(p)
#' }
#' @export
#' @importFrom ggplot2 labs theme element_text
map_nuts <- function(nuts,
                     map_level = 3,
                     country = NULL,
                     show_points = TRUE,
                     border_col = "lightgrey",
                     low_col = "lightgreen",
                     high_col = "darkgreen",
                     id_col = "black",
                     uid_col = "red",
                     verbose = TRUE) {

  st <- .standardize_nuts_input(nuts, map_level)

  # Fetch and prepare shapes
  shp <- .get_cached_shape(level = st$level, year = st$year,
                           resolution = st$resolution, verbose = verbose)
  if (!is.null(country)) {
    shp <- .filter_country(shp, country)
    if (nrow(shp) == 0L) stop("No polygons found for the requested `country`.", call. = FALSE)
  }

  md <- .build_map_data(shp, st$df, st$nuts_col,
                        country_shp = if (!is.null(country)) shp else NULL)

  # Build plot
  p <- .render_map(md = md,
                   border_col = border_col,
                   low_col = low_col,
                   high_col = high_col,
                   id_col = id_col,
                   uid_col = uid_col,
                   show_points = show_points)

  # Tidy title and ASCII-only subtitle
  p <- p + ggplot2::labs(
    title = "NUTS matches",
    subtitle = paste0(
      "Level ", st$level,
      "  |  Year ", st$year,
      "  |  Resolution ", st$resolution,
      if (!is.null(country)) paste0("  |  Country ", country) else ""
    )
  ) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5, face = "bold", size = 13),
      plot.subtitle = ggplot2::element_text(hjust = 0.5, size = 10)
    )

  if (isTRUE(verbose)) {
    .message(sprintf("Rendered map: level=%s, year=%s, resolution=%s%s.",
                     st$level, st$year, st$resolution,
                     if (!is.null(country)) paste0(", country=", country) else ""))
  }
  p
}


# Internal Functions -----------------------------------------------------------

#' Normalise get_nuts() output for mapping
#' @keywords internal
.standardize_nuts_input <- function(nuts, map_level) {
  required_meta <- c("lat", "lon", "year", "resolution")
  if (!all(required_meta %in% names(nuts))) {
    stop("`nuts` must include columns: lat, lon, year, resolution.", call. = FALSE)
  }

  # Validate map_level domain explicitly
  if (!is.numeric(map_level) || length(map_level) != 1L || !(map_level %in% c(0L, 1L, 2L, 3L))) {
    stop("Selected `map_level` is not present (must be one of 0, 1, 2, 3).", call. = FALSE)
  }

  multi_cols <- c("nuts0", "nuts1", "nuts2", "nuts3")
  has_multi  <- any(multi_cols %in% names(nuts))
  has_single <- "nuts" %in% names(nuts)

  if (has_multi && has_single) {
    nuts_col <- paste0("nuts", as.integer(map_level))
    if (!nuts_col %in% names(nuts)) stop("Selected `map_level` column is not present.", call. = FALSE)
    level <- as.integer(map_level)
  } else if (has_multi) {
    nuts_col <- paste0("nuts", as.integer(map_level))
    if (!nuts_col %in% names(nuts)) stop("Selected `map_level` column is not present.", call. = FALSE)
    level <- as.integer(map_level)
  } else if (has_single) {
    available_level <- if ("level" %in% names(nuts) && !is.na(nuts$level[1])) {
      as.integer(nuts$level[1])
    } else {
      NA_integer_
    }
    if (!is.na(available_level) && map_level != available_level) {
      stop(sprintf("Selected `map_level` is not present in `nuts` (available: %d).",
                   available_level), call. = FALSE)
    }
    nuts_col <- "nuts"
    level <- if (!is.na(available_level)) available_level else as.integer(map_level)
  } else {
    stop("`nuts` must include either `nuts` or any of `nuts0..nuts3`.", call. = FALSE)
  }

  list(
    df = nuts,
    nuts_col = nuts_col,
    level = level,
    year = as.integer(nuts$year[1]),
    resolution = as.integer(nuts$resolution[1])
  )
}

#' Build map data (frequencies, splits, and an auto-zoom extent)
#' @keywords internal
#' @importFrom sf st_bbox
.build_map_data <- function(shp, df, nuts_col, country_shp = NULL) {
  # Frequency per region (exclude NA region ids)
  tab <- table(df[[nuts_col]], useNA = "no")
  freq <- data.frame(geo = names(tab), Frequency = as.numeric(tab), stringsAsFactors = FALSE)

  # Align column types
  shp$NUTS_ID <- as.character(shp$NUTS_ID)
  if (nrow(freq) > 0) freq$geo <- as.character(freq$geo)

  # Merge to attach frequencies to polygons
  mapdf <- merge(shp, freq, by.x = "NUTS_ID", by.y = "geo", all.x = TRUE)

  # Points split by match
  found <- !is.na(df[[nuts_col]])
  coords_found <- data.frame(lon = df$lon[found],  lat = df$lat[found])
  coords_na    <- data.frame(lon = df$lon[!found], lat = df$lat[!found])

  # Auto-zoom: when a country filter is active, always show the full country extent.
  # Without a country filter, focus on matched regions if present, then points, then full layer.
  if (!is.null(country_shp)) {
    bb <- sf::st_bbox(country_shp)
  } else if ("Frequency" %in% names(mapdf) && any(!is.na(mapdf$Frequency) & mapdf$Frequency > 0)) {
    bb <- sf::st_bbox(mapdf[!is.na(mapdf$Frequency) & mapdf$Frequency > 0, , drop = FALSE])
  } else if (nrow(df) > 0 && all(c("lon","lat") %in% names(df))) {
    bb <- c(xmin = min(df$lon, na.rm = TRUE), ymin = min(df$lat, na.rm = TRUE),
            xmax = max(df$lon, na.rm = TRUE), ymax = max(df$lat, na.rm = TRUE))
  } else {
    bb <- sf::st_bbox(shp)
  }
  # Padding (5%)
  dx <- as.numeric(bb["xmax"] - bb["xmin"]); dy <- as.numeric(bb["ymax"] - bb["ymin"])
  pad_x <- ifelse(is.finite(dx) && dx > 0, dx * 0.05, 0.5)
  pad_y <- ifelse(is.finite(dy) && dy > 0, dy * 0.05, 0.5)
  xlim <- c(as.numeric(bb["xmin"]) - pad_x, as.numeric(bb["xmax"]) + pad_x)
  ylim <- c(as.numeric(bb["ymin"]) - pad_y, as.numeric(bb["ymax"]) + pad_y)

  list(
    mapdf = mapdf,
    coords_found = coords_found,
    coords_na = coords_na,
    xlim = xlim,
    ylim = ylim
  )
}

#' Render the map (ggplot2)
#' @keywords internal
#' @importFrom ggplot2 ggplot geom_sf scale_fill_gradient geom_point theme_void theme element_text guides guide_colourbar aes coord_sf
#' @importFrom grid unit
.render_map <- function(md,
                        border_col,
                        low_col,
                        high_col,
                        id_col,
                        uid_col,
                        show_points) {

  freq_vals <- md$mapdf$Frequency
  base <- ggplot2::ggplot(md$mapdf)

  multi_in_region <- any(freq_vals > 1, na.rm = TRUE)
  max_freq <- if (length(freq_vals) == 0L || all(is.na(freq_vals))) 0L else max(freq_vals, na.rm = TRUE)

  if (!is.finite(max_freq) || max_freq < 2) {
    multi_in_region <- FALSE
  }

  if (multi_in_region) {
    breaks <- seq(1L, max_freq, by = max(1L, floor(max_freq / 4)))
    if (breaks[length(breaks)] != max_freq) breaks <- c(breaks, max_freq)

    p <- base +
      ggplot2::geom_sf(
        ggplot2::aes(fill = Frequency),
        color = border_col, linewidth = 0.15, alpha = 0.95
      ) +
      ggplot2::scale_fill_gradient(
        name   = "Count",
        low    = low_col,
        high   = high_col,
        limits = c(1, max_freq),
        breaks = breaks,
        na.value = "grey97"
      ) +
      ggplot2::guides(
        fill = ggplot2::guide_colourbar(
          direction = "vertical",
          barheight = grid::unit(60, "pt"),
          barwidth  = grid::unit(6,  "pt"),
          ticks = TRUE,
          ticks.linewidth = 0.3
        )
      )
  } else {
    p <- base +
      ggplot2::geom_sf(fill = "grey97", color = border_col, linewidth = 0.15) +
      ggplot2::guides(fill = "none")
  }

  if (isTRUE(show_points)) {
    if (nrow(md$coords_found) > 0L) {
      p <- p + ggplot2::geom_point(
        data = md$coords_found,
        ggplot2::aes(x = lon, y = lat),
        inherit.aes = FALSE,
        shape = 21, fill = id_col, color = "white",
        size = 2.2, stroke = 0.3, alpha = 0.9
      )
    }
    if (nrow(md$coords_na) > 0L) {
      p <- p + ggplot2::geom_point(
        data = md$coords_na,
        ggplot2::aes(x = lon, y = lat),
        inherit.aes = FALSE,
        shape = 4, color = uid_col,
        size = 2.2, stroke = 0.9, alpha = 0.9
      )
    }
  }

  p +
    ggplot2::coord_sf(xlim = md$xlim, ylim = md$ylim, expand = FALSE) +
    ggplot2::theme_void() +
    ggplot2::theme(
      legend.title = ggplot2::element_text(size = 9),
      legend.text  = ggplot2::element_text(size = 8),
      legend.position = "right",
      plot.margin = grid::unit(c(6, 6, 6, 6), "pt")
    )
}
