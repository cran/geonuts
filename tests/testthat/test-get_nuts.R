# Tests for get_nuts()
# - Structure & columns
# - Level 0 correctness on capitals (DE/FR/IT)
# - "all" levels shape
# - Nearest fallback behaviour over water
# - Input validation errors

suppressPackageStartupMessages(library(testthat))

# ---- Skips / guards -----------------------------------------------------------

skip_if_not_installed("sf")
skip_if_not_installed("eurostat")

has_internet <- function() {
  con <- url("http://www.r-project.org")
  out <- try(suppressWarnings(open.connection(con, open = "rb", timeout = 2)), silent = TRUE)
  if (!inherits(out, "try-error")) close(con)
  !inherits(out, "try-error")
}

if (!has_internet()) {
  skip("No internet connection available; skipping Eurostat-dependent tests.")
}

# ---- Helper data --------------------------------------------------------------

# Approximate coordinates for city centers
lat <- c(52.5200, 48.8566, 41.9028)    # Berlin, Paris, Rome
lon <- c(13.4050, 2.3522, 12.4964)

# A point clearly in the sea (Bay of Biscay), to trigger "nearest"
lat_sea <- 43.8
lon_sea <- -2.5

# ---- Tests -------------------------------------------------------------------

test_that("get_nuts() returns expected structure for single level", {
  res <- get_nuts(latitude = lat,
                  longitude = lon,
                  level = 0,
                  year = 2021,
                  resolution = 20,
                  verbose = FALSE)

  expect_s3_class(res, "data.frame")
  expect_true(all(c("lat","lon","nuts","cntr_code","match_status",
                    "match_dist_km","level","year","resolution") %in% names(res)))
  expect_equal(nrow(res), length(lat))

  # Known NUTS0 codes for these capitals
  expect_true(all(res$nuts %in% c("DE", "FR", "IT")))
  # Level column should be 0
  expect_true(all(res$level == 0L))
})

test_that("get_nuts(level = 'all') returns all levels and metadata", {
  res <- get_nuts(latitude = lat,
                  longitude = lon,
                  level = "all",
                  year = 2021,
                  resolution = 20,
                  verbose = FALSE)

  expect_s3_class(res, "data.frame")
  expect_true(all(c("lat","lon","nuts0","nuts1","nuts2","nuts3",
                    "cntr_code","match_status","match_dist_km",
                    "year","resolution") %in% names(res)))
  expect_equal(nrow(res), length(lat))

  # Basic sanity: nuts0 must be country codes; none should be NA for capital points
  expect_false(any(is.na(res$nuts0)))
})

test_that("get_nuts(match_strategy = 'nearest') assigns nearest region over water", {
  res <- get_nuts(latitude = c(lat[1], lat_sea),
                  longitude = c(lon[1], lon_sea),
                  level = 3,
                  year = 2021,
                  resolution = 20,
                  match_strategy = "nearest",
                  nearest_max_km = 400,
                  verbose = FALSE)

  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 2L)
  expect_true(all(c("nuts","match_status","match_dist_km") %in% names(res)))

  # First point (Berlin) should be matched within polygon
  expect_equal(res$match_status[1], "matched")

  # Second point (sea) should be filled by nearest
  expect_equal(res$match_status[2], "nearest")
  expect_false(is.na(res$match_dist_km[2]))
  expect_true(res$match_dist_km[2] > 0)
})

test_that("get_nuts() input validation triggers informative errors", {
  # Latitude/longitude length mismatch
  expect_error(
    get_nuts(latitude = c(1,2), longitude = 1, level = 0),
    "same length"
  )

  # Invalid level
  expect_error(
    get_nuts(latitude = 1, longitude = 1, level = 5),
    "must be 0, 1, 2, 3, or \"all\""
  )

  # Invalid CRS
  expect_error(
    get_nuts(latitude = 1, longitude = 1, crs = "EPSG:4326"),
    "EPSG integer"
  )
})
