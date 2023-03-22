# This script attempts produce usable dataframes for
# Calgary and Denver
# based on the outputs from ArcGIS Pro

# Please make sure that the working directory this repo (top level)
# so that the file references below work

library(tidyverse)
library(sf)
library(ggplot2)
library(spdep)
library(caTools)
library(plotROC)
library(caret)

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

# Function to engineer features
engineer_features <- function(city_fishnet) {
  city_fishnet <- city_fishnet %>%
    mutate(
      log_dist_big_streams = log(dist_big_streams),
      log_dist_huge_streams = log(dist_huge_streams),
      log_flow_accumulation = log(flow_accumulation),
      inundated = ifelse(inundation_1pct > 0.5, 1, 0)
    )
  return(city_fishnet)
}

# Get the predictors and target

calgary <- st_read("data/fishnet-output/calgary_fishnet.shp") %>%
  dplyr::select(id, geometry) %>%
  add_variables_from_csv("calgary", c(predictors, targets)) %>%
  engineer_features()

denver <- st_read("data/fishnet-output/denver_fishnet.shp") %>%
  dplyr::select(id, geometry) %>%
  add_variables_from_csv("denver", predictors) %>%
  engineer_features()

predictors_used <- c(
  "dem", # --------------- From DEM; meters
  "slope", # ------------- Percentage rise
  "log_dist_big_streams",
  "log_dist_huge_streams",
  "log_flow_accumulation",
  "impervious" # --
)

lag_predictors_used <- predictors_used %>%
  sapply(., function(x) {
    paste0("lag_", x)
  }) %>%
  unname()


all_predictors_used <- c(predictors_used, lag_predictors_used)

target_used <- c("inundated")

calgary_used <- calgary %>%
  dplyr::select(all_of(c("id", predictors_used, target_used)))

# add predictor spatial lags

calculate_spatial_lags <-
  function(fishnet, predictors, id_col = "id", geometry_col = "geometry") {
    # Create neighbors list using the 'geometry' column
    nb <- poly2nb(fishnet, row.names = fishnet[[id_col]])

    # Create spatial weights matrix
    swm <- nb2listw(nb, style = "W", zero.policy = TRUE)

    # Calculate spatial lags for the specified predictor variables
    for (predictor in predictors) {
      spatial_lag_colname <- paste0("lag_", predictor)
      predictor_values <- as.numeric(fishnet[[predictor]])
      fishnet[[spatial_lag_colname]] <- lag.listw(swm, predictor_values, zero.policy = TRUE)
    }

    return(fishnet)
  }

calgary_used <- calgary_used %>%
  calculate_spatial_lags(predictors_used)


# Start to build a model
for_model <- calgary_used %>%
  dplyr::select(all_of(c(all_predictors_used, target_used)), id)

train_ratio <- 0.75
sample <- sample.split(for_model$inundated, SplitRatio = train_ratio)
train <- subset(for_model, sample == TRUE)
test <- subset(for_model, sample == FALSE)

# The regression model
model <- glm(
  inundated ~ ., train %>% dplyr::select(-id) %>% st_drop_geometry(),
  family = "binomial"(link = "logit")
)

test$predicted_probs <- predict(model, test, type = "response")

# Plot two probability density plots
ggplot(test, aes(x = predicted_probs, fill = as.factor(inundated))) +
  geom_density() +
  facet_grid(inundated ~ ., scales = "free") +
  xlim(-0.1, 1) +
  labs(
    x = "Predicted Probability of Inundation",
    y = "Probability Density",
    title = "Distribution of predicted probabilities by observed outcome"
  )

ggplot(test) +
  geom_roc(aes(d = as.numeric(test$inundated), m = test$predicted_probs)) +
  style_roc(theme = theme_grey) +
  geom_abline(slope = 1, intercept = 0, size = 1.5, color = "grey") +
  labs(
    title = "ROC Curve",
    subtitle = paste(
      "Area Under Curve (AUC):",
      pROC::auc(test$inundated, test$predicted_probs, digits = 4)
    )
  )

# Cross-validation
