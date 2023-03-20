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

# Get the predictors and target

calgary <- st_read("data/fishnet-output/calgary_fishnet.shp") %>%
  dplyr::select(id, geometry) %>%
  add_variables_from_csv("calgary", c(predictors, targets))

denver <- st_read("data/fishnet-output/denver_fishnet.shp") %>%
  dplyr::select(id, geometry) %>%
  add_variables_from_csv("denver", predictors)

# plot calgary fishnet
ggplot() +
  geom_sf(data=calgary, 
          fill = "dark green", 
          color = "dark green",
          alpha = 0.6) +
  labs(title="Calgary") +
  mapTheme


# plot calgary fishnet
ggplot() +
  geom_sf(data=denver, 
          fill = "dark blue", 
          color = "dark blue",
          alpha = 0.6) +
  labs(title="Denver") +
  mapTheme

# add spatial lags for calagry
install.packages("spdep")
library(spdep)
calculate_spatial_lags <- function(data, targets, id_col = "id", geometry_col = "geometry") {
  
  # Create a neighbors list based on polygon contiguity
  nb <- poly2nb(data, row.names = data[[id_col]])
  
  # Convert neighbors list to spatial weights matrix (row-standardized)
  lw <- nb2listw(nb, style = "W")
  
  # Calculate spatial lags for each target variable
  for (target in targets) {
    lag_var_name <- paste0(target, "_spatial_lag")
    data[[lag_var_name]] <- lag.listw(lw, data[[target]])
  }
  
  return(data)
}

calgary <- calculate_spatial_lags(calgary, targets)

# Correlation matrix

## Convert the 'calgary' dataset to a regular data frame
calgary_df <- as.data.frame(calgary) 

## Remove non-numeric columns, including the 'geometry' column
calgary_numeric <- calgary_df %>% dplyr::select(-id) %>% dplyr::select_if(is.numeric)

## Calculate the correlation matrix
correlation_matrix <- cor(calgary_numeric)

library(corrplot)
corrplot(correlation_matrix, method = "color", type = "lower", tl.col = "black", diag = FALSE)