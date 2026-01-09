<!-- README.md is generated from README.Rmd. Please edit README.Rmd -->

> Identify and visualize European **NUTS** regions from geographic
> coordinates using Eurostat geospatial data.

<!-- badges: start -->

[![R-CMD-check](https://github.com/aikatona/geonuts/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/aikatona/geonuts/actions/workflows/R-CMD-check.yaml)
[![License:
GPL-3](https://img.shields.io/badge/License-GPL--3-blue.svg)](https://github.com/aikatona/geonuts/blob/main/LICENSE.md)
<!-- badges: end -->

## Overview

**geonuts** is a lightweight R package that maps latitude/longitude to
EU NUTS regions  
(levels 0–3) and provides quick validation maps.

-   Fast lookup for many points
-   `level = "all"` to return NUTS0–NUTS3 at once
-   Optional nearest‐polygon fallback for points just off the coast
-   Clean maps with auto-zoom and frequency shading
-   Uses official Eurostat layers via **eurostat** (and compatible with
    **giscoR**)

## Installation

    # install.packages("remotes")
    remotes::install_github("aikatona/geonuts")

## Quick start

    library(geonuts)

    # Example coordinates (Berlin, Paris, Rome)
    lat <- c(52.5200, 48.8566, 41.9028)
    lon <- c(13.4050, 2.3522, 12.4964)

    # 1) Identify NUTS regions at all levels
    nuts_all <- get_nuts(
      latitude   = lat,
      longitude  = lon,
      level      = "all",
      year       = 2021,
      resolution = 20,
      verbose    = FALSE
    )
    nuts_all

    ##       lat     lon nuts0 nuts1 nuts2 nuts3 cntr_code match_status match_dist_km year
    ## 1 52.5200 13.4050    DE   DE3  DE30 DE300        DE      matched            NA 2021
    ## 2 48.8566  2.3522    FR   FR1  FR10 FR101        FR      matched            NA 2021
    ## 3 41.9028 12.4964    IT  <NA>  <NA>  <NA>        IT    unmatched            NA 2021
    ##   resolution
    ## 1         20
    ## 2         20
    ## 3         20

    #>   lat    lon nuts0 nuts1   nuts2    nuts3 cntr_code match_status match_dist_km year resolution
    #>  ... (table truncated in README for brevity)

    # 2) Map (NUTS3) with points
    map_nuts(nuts_all, map_level = 3, show_points = TRUE)

    ## [geonuts] Using cached NUTS layer.
    ## [geonuts] Rendered map: level=3, year=2021, resolution=20.

![](README_files/figure-markdown_strict/unnamed-chunk-2-1.png)

    # 3) Restrict map to a country (e.g., Italy)
    map_nuts(nuts_all, map_level = 3, country = "IT")

    ## [geonuts] Using cached NUTS layer.
    ## [geonuts] Rendered map: level=3, year=2021, resolution=20, country=IT.

![](README_files/figure-markdown_strict/unnamed-chunk-2-2.png)

## Nearest fallback (over water)

    # A point in the Bay of Biscay plus Berlin
    lat2 <- c(52.5200, 43.8000)
    lon2 <- c(13.4050, -2.5000)

    res <- get_nuts(
      latitude        = lat2,
      longitude       = lon2,
      level           = 3,
      match_strategy  = "nearest",
      nearest_max_km  = 400,
      year            = 2021,
      resolution      = 20,
      verbose         = FALSE
    )
    res

    ##     lat    lon  nuts cntr_code match_status match_dist_km level year resolution
    ## 1 52.52 13.405 DE300        DE      matched            NA     3 2021         20
    ## 2 43.80 -2.500 FRI13        FR      nearest      82.83546     3 2021         20

## Notes on data and caching

-   Geometries are fetched via `eurostat::get_eurostat_geospatial()` and
    cached.
-   Once downloaded, subsequent calls are much faster and work offline.
-   To clear the Eurostat cache:

<!-- -->

    unlink(tools::R_user_dir("eurostat", "cache"), recursive = TRUE)

## Functions

-   `get_nuts()`: Identify NUTS for coordinates (single level 0/1/2/3 or
    “all”).
-   `map_nuts()`: Visualize matches; auto-zoom; optional frequency
    legend (shown only if any region has count &gt; 1).

## Vignette

A longer walk-through is available as an HTML vignette:

    # Build once locally
    devtools::build_vignettes()
    browseVignettes("geonuts")

## Citation

If you use geonuts in published work, please cite the GitHub repository:

> Katona, A. I., & Kurbucz, M. T. (2025). geonuts: Identify and
> Visualize European NUTS Regions from Geolocations. GitHub:
> aikatona/geonuts.
