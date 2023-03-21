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

View(calgary)
View(denver)

# plot Calgary fishnet
ggplot() +
  geom_sf(data=calgary, 
          fill = "dark green", 
          color = "dark green",
          alpha = 0.6) +
  labs(title="Calgary") +
  mapTheme


# plot Denver fishnet
ggplot() +
  geom_sf(data=denver, 
          fill = "dark blue", 
          color = "dark blue",
          alpha = 0.6) +
  labs(title="Denver") +
  mapTheme

# add predictor spatial lags
install.packages("spdep")
library(spdep)

predictors <- c(
  "dem", # --------------- From DEM; meters
  "slope", # ------------- Percentage rise
  "dist_big_streams", # -- Distance (meters) to streams with drainage > 50 km2
  "dist_huge_streams", # - Distance (meters) to streams with drainage > 100 km2
  "flow_accumulation", # - Flow accumulation (number of cells)
  "impervious" # --------- Impervious surface as percange of area
)

calculate_spatial_lags <- function(data, predictors, id_col = "id", geometry_col = "geometry") {
  # Create neighbors list using the 'geometry' column
  nb <- poly2nb(data, row.names = data[[id_col]])
  
  # Create spatial weights matrix
  swm <- nb2listw(nb, style = "W", zero.policy = TRUE)
  
  # Calculate spatial lags for the specified predictor variables
  for (predictor in predictors) {
    spatial_lag_colname <- paste0("lag_", predictor)
    predictor_values <- as.numeric(data[[predictor]])
    data[[spatial_lag_colname]] <- lag.listw(swm, predictor_values, zero.policy = TRUE)
  }
  
  return(data)
}


calgary_with_lags <- calculate_spatial_lags(calgary, predictors)
denver_with_lags <- calculate_spatial_lags(denver, predictors)
View(calgary_with_lags)


# Calculate the correlation matrix
library(corrplot)

plot_correlation_matrix <- function(data, id_col = "id") {
  # Convert the dataset to a regular data frame
  data_df <- as.data.frame(data)
  
  # Remove non-numeric columns, including the 'id' column
  data_numeric <- data_df %>% dplyr::select(-!!sym(id_col)) %>% dplyr::select_if(is.numeric)
  
  # Calculate the correlation matrix
  correlation_matrix <- cor(data_numeric)
  
  # Create the correlation matrix plot
  corrplot(correlation_matrix, method = "color", type = "lower", tl.col = "black", diag = FALSE)
}

plot_correlation_matrix(calgary_with_lags)
plot_correlation_matrix(denver_with_lags)

