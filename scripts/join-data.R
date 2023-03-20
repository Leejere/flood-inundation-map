# This script attempts produce usable dataframes for
# Calgary and Denver
# based on the outputs from ArcGIS Pro

# Please make sure that the working directory this repo (top level)
# so that the file references below work

library(tidyverse)
library(sf)
library(ggplot2)

lintr::use_lintr(type = "tidyverse")

predictors <- c(
  "boundary", # ---------- 1: inside city boundary; 0: outside city boundary
  "dem", # --------------- From DEM; meters
  "slope", # ------------- Percentage rise
  "dist_big_streams", # -- Distance (meters) to streams with drainage > 50 km2
  "dist_huge_streams", # - Distance (meters) to streams with drainage > 100 km2
  "flow_accumulation", # - Flow accumulation (number of cells)
  "impervious" # --------- Impervious surface as percange of area
)

targets <- c(
  "inundation_1pct", # --- Percange area in 100-year (1%) inundated zone
  "inundation_10pct" # --- Percange area in 10-year (10%) inundated zone
)

add_variables_from_csv <- function(city_fishnet, city_name, variable_list) {
  for (variable in variable_list) {
    path <- paste0(
      "data/fishnet-output/tbl_", city_name, "_", variable, ".csv"
    )

    data <- read.csv(path) %>%
      rename(id := OID_, !!variable := value)

    city_fishnet <- city_fishnet %>%
      left_join(data, by = "id")
  }

  city_fishnet <- city_fishnet %>%
    filter(boundary > 0.9) %>%
    dplyr::select(-boundary)

  return(city_fishnet)
}

# Get the predictors

calgary <- st_read("data/fishnet-output/calgary_fishnet.shp") %>%
  dplyr::select(id, geometry) %>%
  add_variables_from_csv("calgary", c(predictors, targets))

denver <- st_read("data/fishnet-output/denver_fishnet.shp") %>%
  dplyr::select(id, geometry) %>%
  add_variables_from_csv("denver", predictors)
