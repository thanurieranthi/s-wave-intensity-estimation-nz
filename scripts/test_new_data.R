# Function to test model_ris_1 on new data from after 2023
test_model_ris_1_new_data <- function(fitted_model, new_data_path, training_data) {

  # Load new data
  new_data <- read.csv(new_data_path, check.names = FALSE)

  # Filter data for years after 2023
  new_data_filtered <- new_data[new_data$year > 2023, ]

  # Check if we have data after 2023
  if (nrow(new_data_filtered) == 0) {
    print("No data available after 2023")
    return(NULL)
  }

  print(paste("Initial number of records after 2023:", nrow(new_data_filtered)))

  # Rename column to match training data naming convention
  names(new_data_filtered)[names(new_data_filtered) == "station_name"] <- "StationName"

  # Get list of stations from training data
  training_stations <- unique(training_data$StationName)

  # Filter new data to only include stations that exist in training data
  new_data_filtered <- new_data_filtered[new_data_filtered$StationName %in% training_stations, ]

  # Check if we have data after filtering
  if (nrow(new_data_filtered) == 0) {
    print("No data available after 2023 for stations in training data")
    return(NULL)
  }

  print(paste("Number of records after filtering for existing stations:", nrow(new_data_filtered)))
  print(paste("Years in filtered data:", paste(unique(new_data_filtered$year), collapse = ", ")))
  print(paste("Number of unique stations:", length(unique(new_data_filtered$StationName))))

  # Convert necessary columns to appropriate types
  new_data_filtered$StationName <- as.factor(new_data_filtered$StationName)
  new_data_filtered$year <- as.factor(new_data_filtered$year)

  # Create Year_Index as a factor (mapping years to indices)
  year_values <- unique(sort(as.numeric(new_data_filtered$year)))
  year_index_mapping <- data.frame(
    year = year_values,
    Year_Index = as.factor(seq_along(year_values))
  )

  # Add Year_Index to the dataset
  new_data_filtered <- merge(new_data_filtered, year_index_mapping, by = "year", all.x = TRUE)

  # Generate random EarthquakeID values (since we don't have real-time access)
  set.seed(123)
  new_data_filtered$EarthquakeID <- as.factor(sample(1:nrow(new_data_filtered), nrow(new_data_filtered), replace = TRUE))

  # Make predictions using the fitted model
  # allow.new.levels = FALSE for stations (only use existing stations)
  # allow.new.levels = TRUE for EarthquakeID and Year_Index (these are randomly generated)
  predictions <- predict(fitted_model,
                        newdata = new_data_filtered,
                        allow.new.levels = TRUE)

  # Handle any NA predictions
  valid_idx <- !is.na(predictions) & !is.na(new_data_filtered$log_PGV)
  predictions_valid <- predictions[valid_idx]
  actual_values <- new_data_filtered$log_PGV[valid_idx]

  if (length(predictions_valid) == 0) {
    print("No valid predictions generated")
    return(NULL)
  }

  print(paste("Number of valid predictions:", length(predictions_valid)))

  # Calculate performance metrics
  # R-squared (coefficient of determination)
  ss_res <- sum((actual_values - predictions_valid)^2)
  ss_tot <- sum((actual_values - mean(actual_values))^2)
  r_squared <- 1 - (ss_res / ss_tot)

  # RMSE (Root Mean Squared Error)
  rmse <- sqrt(mean((actual_values - predictions_valid)^2))

  # MSE (Mean Squared Error)
  mse <- mean((actual_values - predictions_valid)^2)

  # MAPE (Mean Absolute Percentage Error)
  mape <- mean(abs((actual_values - predictions_valid) / actual_values)) * 100

  # Print performance metrics
  print("=== MODEL PERFORMANCE METRICS ===")
  print(paste("R-squared:", round(r_squared, 4)))
  print(paste("RMSE:", round(rmse, 4)))
  print(paste("MSE:", round(mse, 4)))
  print(paste("MAPE:", round(mape, 2), "%"))
  print("================================")

  # Create residuals
  residuals <- actual_values - predictions_valid

  # Create a comprehensive residual plot panel
  par(mfrow = c(2, 2))

  # 1. Residuals vs Fitted Values
  plot(predictions_valid, residuals,
       main = "Residuals vs Fitted Values",
       xlab = "Fitted Values (Predicted log_PGV)",
       ylab = "Residuals",
       pch = 16,
       col = "steelblue",
       cex = 0.7)
  abline(h = 0, col = "red", lwd = 2)

  # 2. Q-Q Plot (Normality of residuals)
  qqnorm(residuals,
         main = "Q-Q Plot of Residuals",
         pch = 16,
         col = "steelblue")
  qqline(residuals, col = "red", lwd = 2)

  # 3. Histogram of Residuals
  hist(residuals,
       main = "Histogram of Residuals",
       xlab = "Residuals",
       ylab = "Frequency",
       col = "steelblue",
       border = "black",
       breaks = 20)

  # 4. Actual vs Predicted Values
  plot(actual_values, predictions_valid,
       main = "Actual vs Predicted log_PGV",
       xlab = "Actual log_PGV",
       ylab = "Predicted log_PGV",
       pch = 16,
       col = "steelblue",
       cex = 0.7)
  # Add perfect prediction line
  min_val <- min(actual_values, predictions_valid)
  max_val <- max(actual_values, predictions_valid)
  abline(a = 0, b = 1, col = "red", lwd = 2)

  par(mfrow = c(1, 1))

  # Create additional diagnostic plots
  par(mfrow = c(1, 2))

  # 5. Scale-Location Plot (Square root of standardized residuals vs fitted)
  standardized_residuals <- residuals / sd(residuals)
  plot(predictions_valid, sqrt(abs(standardized_residuals)),
       main = "Scale-Location Plot",
       xlab = "Fitted Values",
       ylab = "√|Standardized Residuals|",
       pch = 16,
       col = "steelblue",
       cex = 0.7)

  # 6. Residuals by Station (if there are enough data points per station)
  station_data <- new_data_filtered[valid_idx, ]
  if (length(unique(station_data$StationName)) > 1) {
    boxplot(residuals ~ station_data$StationName,
            main = "Residuals by Station",
            xlab = "Station Name",
            ylab = "Residuals",
            las = 2)
    abline(h = 0, col = "red", lwd = 1, lty = 2)
  }

  par(mfrow = c(1, 1))

  # Return results as a list
  results <- list(
    r_squared = r_squared,
    rmse = rmse,
    mse = mse,
    mape = mape,
    residuals = residuals,
    predictions = predictions_valid,
    actual = actual_values,
    n_predictions = length(predictions_valid),
    years_tested = unique(sort(new_data_filtered$year[valid_idx]))
  )

  return(invisible(results))
}
