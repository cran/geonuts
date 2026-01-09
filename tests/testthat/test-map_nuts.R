# Tests for map_nuts()
# - Output structure (ggplot)
# - Basic plotting with single level
# - Basic plotting with all levels
# - Country filtering
# - Point overlay toggling
# - Input validation

suppressPackageStartupMessages(library(testthat))

# ---- Skips / guards -----------------------------------------------------------

skip_if_not_installed("sf")
skip_if_not_installed("ggplot2")
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

lat <- c(52.5200, 48.8566, 41.9028)    # Berlin, Paris, Rome
lon <- c(13.4050, 2.3522, 12.4964)

# ---- Tests -------------------------------------------------------------------

test_that("map_nuts() returns a ggplot object for single level results", {
  res <- get_nuts(latitude = lat,
                  longitude = lon,
                  level = 3,
                  year = 2021,
                  resolution = 20,
                  verbose = FALSE)

  p <- map_nuts(res, map_level = 3, show_points = TRUE, verbose = FALSE)
  expect_s3_class(p, "ggplot")
})

test_that("map_nuts() works with level = 'all' outputs", {
  res <- get_nuts(latitude = lat,
                  longitude = lon,
                  level = "all",
                  year = 2021,
                  resolution = 20,
                  verbose = FALSE)

  p <- map_nuts(res, map_level = 1, show_points = FALSE, verbose = FALSE)
  expect_s3_class(p, "ggplot")
})

test_that("map_nuts() correctly filters by country code", {
  res <- get_nuts(latitude = lat,
                  longitude = lon,
                  level = 3,
                  year = 2021,
                  resolution = 20,
                  verbose = FALSE)

  p_de <- map_nuts(res, map_level = 3, country = "DE", show_points = TRUE, verbose = FALSE)
  expect_s3_class(p_de, "ggplot")
})

test_that("map_nuts() handles missing points gracefully", {
  res <- get_nuts(latitude = lat,
                  longitude = lon,
                  level = 3,
                  year = 2021,
                  resolution = 20,
                  verbose = FALSE)
  res$nuts[2] <- NA  # simulate one unmatched point

  p <- map_nuts(res, map_level = 3, show_points = TRUE, verbose = FALSE)
  expect_s3_class(p, "ggplot")
})

test_that("map_nuts() throws informative errors for malformed input", {
  # Missing required columns
  bad_df <- data.frame(lat = c(1, 2), lon = c(3, 4))
  expect_error(map_nuts(bad_df), "must include columns")

  # Invalid map_level
  res <- get_nuts(latitude = lat,
                  longitude = lon,
                  level = 3,
                  year = 2021,
                  resolution = 20,
                  verbose = FALSE)

  expect_error(map_nuts(res, map_level = 5), "not present")
})
