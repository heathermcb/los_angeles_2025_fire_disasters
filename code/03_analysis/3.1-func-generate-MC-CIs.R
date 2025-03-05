# -------------------------------------------------------------------------------
#-------------Los Angeles Wildfires- ITS analysis------------------------------#   
#-------------------------R code-----------------------------------------------#
#-----------------Last update:2/28/25------------------------------------------#

# Code adapted from the following project:

# @project: Two-stage interrupted time series design
# @author: Arnab K. Dey (arnabxdey@gmail.com), Yiqun Ma
# @organization: Scripps Institution of Oceanography, UC San Diego
# @description: This script contains functions to generate confidence intervals for time series forecasts
# @date: Dec 20, 2024
# -------------------------------------------------------------------------------

#' @description
#' This function performs repeated forecasting to generate prediction intervals using 
#' the modeltime framework. It runs multiple iterations of model fitting and prediction, 
#' aggregating results to produce final estimates with uncertainty bounds.
#'
#' @param model_spec A modeltime table containing the configured model(s) to be used
#'                  for forecasting
#' @param training_data A dataframe containing the training data used for model 
#'                     calibration and refitting
#' @param forecast_horizon_data A dataframe containing all time periods for which 
#'                            forecasts should be generated
#' @param n_iterations Integer specifying the number of forecast iterations to perform
#'                    (default: 1000)
#'
#' @return A dataframe containing:
#'   \itemize{
#'     \item .index: Dates for each forecast point
#'     \item .value: Mean predicted values across successful iterations
#'     \item .pred_lo: Lower 95% prediction bound (2.4th percentile)
#'     \item .pred_hi: Upper 95% prediction bound (97.5th percentile)
#'   }
#'   Additionally, the return object contains attributes:
#'   \itemize{
#'     \item n_successful: Number of successful forecast iterations
#'     \item n_attempted: Total number of attempted iterations
#'   }
#'
#' @details
#' The function performs the following steps:
#' 1. Initializes storage for predictions and progress tracking
#' 2. For each iteration:
#'    - Sets a random seed for reproducibility
#'    - Calibrates the model using training data
#'    - Refits the model and generates forecasts
#'    - Stores predictions in a matrix
#' 3. Handles failed iterations gracefully
#' 4. Calculates summary statistics across all successful iterations
#'
#' @note
#' The function uses tryCatch to handle potential errors during individual
#' iterations, ensuring the overall process continues even if some
#' iterations fail.
#'
#' @examples
#' \dontrun{
#' results <- generate_forecast_intervals(
#'   model_spec = my_models,
#'   training_data = training_data,
#'   forecast_horizon_data = full_dataset,
#'   n_iterations = 1000
#' )
#' }

#---------------Function to generate forecast intervals-------------------------------#
generate_forecast_intervals <- function(
    model_spec,             
    training_data,    
    forecast_horizon_data,          
    n_iterations = 1000    
) {
    forecast_predictions <- list()
    
    pb <- txtProgressBar(min = 0, max = n_iterations, style = 3)
    
    forecast_dates <- unique(forecast_horizon_data$date)
    n_dates <- length(forecast_dates)
    
    forecast_matrix <- matrix(NA, nrow = n_dates, ncol = n_iterations)
    
    for(i in 1:n_iterations) {
        iter_seed <- sample.int(.Machine$integer.max, 1)
        set.seed(iter_seed)
        
        tryCatch({
            calibrated_models <- model_spec |>
                modeltime_calibrate(
                    new_data = training_data,
                    quiet = TRUE
                )
            
            final_models <- calibrated_models |>
                modeltime_refit(data = training_data)
            
            predictions <- final_models |>
                modeltime_forecast(
                    new_data = forecast_horizon_data,
                    actual_data = training_data,
                    conf_interval = 0.95,
                    conf_method = "conformal_default",
                    set.seed = iter_seed,
                    bootstrap_time = 100,
                    allow_parallel = FALSE
                )
            
            pred_df <- predictions |>
                filter(.model_desc != "ACTUAL")
            forecast_matrix[, i] <- pred_df$.value
            
        }, error = function(e) {
            warning(sprintf("Iteration %d failed: %s", i, e$message))
            forecast_matrix[, i] <- NA
        })
        
        setTxtProgressBar(pb, i)
    }
    
    close(pb)
    
    forecast_matrix <- forecast_matrix[, colSums(!is.na(forecast_matrix)) > 0]
    
    results <- data.frame(
        .index = forecast_dates,
        .value = rowMeans(forecast_matrix, na.rm = TRUE),
        .pred_lo = apply(forecast_matrix, 1, quantile, probs = 0.025, na.rm = TRUE),
        .pred_hi = apply(forecast_matrix, 1, quantile, probs = 0.975, na.rm = TRUE)
    )
    
    attr(results, "n_successful") <- ncol(forecast_matrix)
    attr(results, "n_attempted") <- n_iterations
    
    return(results)
}

#-----------Function to generate forecast values (no eCIs)-----------------------#


generate_forecast_values <- function(
    model_spec,             
    training_data,    
    forecast_horizon_data,          
    n_iterations = 1000    
) {
  forecast_predictions <- list()
  
  pb <- txtProgressBar(min = 0, max = n_iterations, style = 3)
  
  forecast_dates <- unique(forecast_horizon_data$date)
  n_dates <- length(forecast_dates)
  
  forecast_matrix <- matrix(NA, nrow = n_dates, ncol = n_iterations)
  
  for(i in 1:n_iterations) {
    iter_seed <- sample.int(.Machine$integer.max, 1)
    set.seed(iter_seed)
    
    tryCatch({
      calibrated_models <- model_spec |>
        modeltime_calibrate(
          new_data = training_data,
          quiet = TRUE
        )
      
      final_models <- calibrated_models |>
        modeltime_refit(data = training_data)
      
      predictions <- final_models |>
        modeltime_forecast(
          new_data = forecast_horizon_data,
          actual_data = training_data,
          conf_interval = NULL,  # No confidence intervals
          allow_parallel = FALSE
        )
      
      pred_df <- predictions |>
        filter(.model_desc != "ACTUAL")
      forecast_matrix[, i] <- pred_df$.value
      
    }, error = function(e) {
      warning(sprintf("Iteration %d failed: %s", i, e$message))
      forecast_matrix[, i] <- NA
    })
    
    setTxtProgressBar(pb, i)
  }
  
  close(pb)
  
  forecast_matrix <- forecast_matrix[, colSums(!is.na(forecast_matrix)) > 0]
  
  results <- data.frame(
    .index = forecast_dates,
    .value = rowMeans(forecast_matrix, na.rm = TRUE)  # Only returning the mean forecast values
  )
  
  attr(results, "n_successful") <- ncol(forecast_matrix)
  attr(results, "n_attempted") <- n_iterations
  
  return(results)
}

