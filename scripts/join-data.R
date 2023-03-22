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
  geom_sf(
    data = calgary,
    fill = "dark green",
    color = "dark green",
    alpha = 0.6
  ) +
  labs(title = "Calgary") 


# plot Denver fishnet
ggplot() +
  geom_sf(
    data = denver,
    fill = "dark blue",
    color = "dark blue",
    alpha = 0.6
  ) +
  labs(title = "Denver") +
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

# EDA
##Calculate the correlation matrix
library(corrplot)

plot_correlation_matrix <- function(data, id_col = "id") {
  # Convert the dataset to a regular data frame
  data_df <- as.data.frame(data)

  # Remove non-numeric columns, including the 'id' column
  data_numeric <- data_df %>%
    dplyr::select(-!!sym(id_col)) %>%
    dplyr::select_if(is.numeric)

  # Calculate the correlation matrix
  correlation_matrix <- cor(data_numeric)

  # Create the correlation matrix plot
  corrplot(correlation_matrix, method = "color", type = "lower", tl.col = "black", diag = FALSE)
}

# Call the function
plot_correlation_matrix(calgary_with_lags)
plot_correlation_matrix(denver_with_lags)


## Function to plot histograms of the variables
library(gridExtra)
plot_histograms <- function(data, predictors) {
  
  # Initialize an empty list to store the plots
  plots <- list()
  
  for (predictor in predictors) {
    # Create a histogram for each predictor in the dataset
    p <- ggplot(data, aes_string(predictor)) +
      geom_histogram() + # Removed binwidth parameter
      labs(title = paste("Histogram of", predictor),
           x = predictor,
           y = "Frequency") +
      theme_minimal()
    
    # Add the histograms to the list
    plots <- append(plots, list(p))
  }
  
  # Arrange the plots in a grid with 3 columns and 2 rows
  grid.arrange(grobs = plots, ncol = 3, nrow = 2)
}


# Call the function
plot_histograms(calgary_df, predictors)
plot_histograms(denver_df, predictors)

#pairs(calgary_df_numeric)

# feature engineering

## transform sf_data to df
tranform_df <- function(data, id_col = "id") {
  data_df <- as.data.frame(data)
  data_numeric <- data_df %>% dplyr::select(-!!sym(id_col)) %>% dplyr::select_if(is.numeric)
  return(data_numeric)
}

calgary_df <- tranform_df(calgary)
denver_df <- tranform_df(denver)

## flow_accumulation

calgary_df$log_flow_accumulation <- log(calgary_df$flow_accumulation)
denver_df$log_flow_accumulation <- log(denver_df$log_flow_accumulation)


## distance to streams
### option 1 :categorize distance into bins

library(classInt)

# Function to categorize a column using the Fisher-Jenks method
categorize_by_fisher <- function(data, n_categories, column) {
  # Apply the Fisher-Jenks method
  breaks_fisher <- classIntervals(
    data[[column]],
    n = n_categories,
    style = "fisher"
  )
  
  # Create the column name for the categorized data
  new_column_name <- paste0(column, "_categories")
  
  # Categorize the column based on the calculated breaks
  data[[new_column_name]] <- cut(
    data[[column]],
    breaks = c(breaks_fisher$brks),
    labels = paste0("Category_", seq(1, n_categories)),
    include.lowest = TRUE
  )
  
  # Return the modified data frame with the new categorized column
  return(data)
}

# Call the function
calgary_df <- categorize_by_fisher(data = calgary_df, n_categories = 3, column = "dist_big_streams")
calgary_df <- categorize_by_fisher(data = calgary_df, n_categories = 3, column = "dist_huge_streams")

denver_df <- categorize_by_fisher(data = denver_df, n_categories = 3, column = "dist_big_streams")
denver_df <- categorize_by_fisher(data = denver_df, n_categories = 3, column = "dist_huge_streams")

# check
glimpse((calgary_df))
levels(calgary_df$dist_big_streams_categories)

### option 2 : log transform
calgary_df$log_dist_big_streams <- log(calgary_df$dist_big_streams)
calgary_df$log_dist_huge_streams <- log(calgary_df$dist_huge_streams)

denver_df$log_dist_big_streams <- log(denver_df$dist_big_streams)
denver_df$log_dist_huge_streams <- log(denver_df$dist_huge_streams)


## transform predicted variable

# Function to binarize values in a data frame column based on a threshold
binarize_values <- function(data, column, threshold, include_threshold = FALSE) {
  if (include_threshold) {
    # If the threshold value should be included as 1
    binary_values <- as.integer(data[[column]] >= threshold)
  } else {
    # If the threshold value should be included as 0
    binary_values <- as.integer(data[[column]] > threshold)
  }
  
  # Create the new column name for the binary data
  new_column_name <- paste0(column, "_binary")
  
  # Add the binary values column to the data frame
  data[[new_column_name]] <- binary_values
  
  # Return the modified data frame with the new binary column
  return(data)
}

# Call the function
calgary_df <- binarize_values(calgary_df, "inundation_1pct", threshold = 0.5, include_threshold = TRUE)
calgary_df <- binarize_values(calgary_df, "inundation_10pct", threshold = 0.5, include_threshold = TRUE)

denver_df <- binarize_values(denver_df, "inundation_1pct", threshold = 0.5, include_threshold = TRUE)
denver_df <- binarize_values(denver_df, "inundation_10pct", threshold = 0.5, include_threshold = TRUE)


