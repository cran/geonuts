## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 7,
  fig.height = 5,
  message = FALSE,
  warning = FALSE
)

## ----example-getnuts----------------------------------------------------------
library(geonuts)

# Example coordinates for European capitals
lat <- c(52.5200, 48.8566, 41.9028)
lon <- c(13.4050,  2.3522, 12.4964)

# Identify NUTS regions (all levels)
nuts_all <- get_nuts(
  latitude = lat,
  longitude = lon,
  level = "all",
  year = 2021,
  resolution = 20,
  verbose = FALSE
)

nuts_all

## ----example-mapnuts----------------------------------------------------------
# Create a map for the most granular (NUTS3) level
map_nuts(nuts_all, map_level = 3, show_points = TRUE)

## ----example-mapnuts-country--------------------------------------------------
map_nuts(nuts_all, map_level = 3, country = "IT")

## ----example-single-level-----------------------------------------------------
nuts2 <- get_nuts(lat, lon, level = 2, year = 2021, resolution = 20)
head(nuts2)

## ----example-cache-reset, eval = FALSE----------------------------------------
# unlink(tools::R_user_dir("eurostat", "cache"), recursive = TRUE)

