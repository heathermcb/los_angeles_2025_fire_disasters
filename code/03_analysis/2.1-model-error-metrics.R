# -----------------------------------------------------------------------------#
#-------------Los Angeles Wildfires- ITS analysis------------------------------#   
#-------------------------R code-----------------------------------------------#
#-----------------Last update:6/27/25------------------------------------------#


# Code adapted from the following project:

# @project: Two-stage interrupted time series design
# @author: Arnab K. Dey, Yiqun Ma
# @organization: Scripps Institution of Oceanography, UC San Diego
# @description: This script loads the modeltime table with best models and calculates training and testing error metrics
# @date: Dec 16, 2024

# load libraries ----------------------------------------------------------------
rm(list = ls())
set.seed(0112358)
pacman::p_load(here, tidymodels, tidyverse, modeltime, metrics)

# set paths
source("paths.R")

# ensure consistent numeric precision ----------------------------------------------
options(digits = 7)
options(scipen = 999)

# load data ---------------------------------------------------
# List of dataset names
#datasets<- c( "df_Virtual_high", "df_Virtual_moderate", "df_Virtual_least")
datasets<- c("df_Virtual_high", "df_Virtual_moderate", "df_OP_high", "df_OP_moderate") 

# List of encounter types to loop through
#encounter_types <- c("num_enc_resp")
encounter_types <- c( "num_enc",  "num_enc_cardio",  "num_enc_neuro", "num_enc_injury")

# Initialize an empty list to store results
results_list <- list()

# Loop through each dataset and load the models and process results
for (dataset_name in datasets) {

  # Loop through each encounter type 
  for (encounter_type in encounter_types) {
 
      # load data ---------------------------------------------------
    preintervention_filename <- here(mod, paste0("df-train-test_sf_", dataset_name, ".csv") )
    df_preintervention <- read.csv(here(preintervention_filename)) %>%
      mutate(date = as.Date(date)) %>%
      select(date, all_of(encounter_type), pr, tmmx, tmmn, rmin, rmax, vs, srad, postjan7, time_period, influenza.a, influenza.b, rsv, sars.cov2) %>%
      mutate(influenza.a = influenza.a * 10000000,
             influenza.b = influenza.b * 10000000,
             rsv = rsv * 10000000,
             sars.cov2 = sars.cov2*10000000) %>%
      mutate(across(where(is.numeric), as.integer)) %>%
      #filter(date>= "2023-01-01") %>% # for respiratory only
      arrange(date)
    
    all_cases_filename <-  here(mod, paste0("df-predict-sf_", dataset_name, ".csv")) 
    df_all_cases <- read.csv(here(all_cases_filename)) %>%
      mutate(date = as.Date(date)) %>%
      select(date, all_of(encounter_type), pr, tmmx, tmmn, rmin, rmax, vs, srad, postjan7, time_period, influenza.a, influenza.b, rsv, sars.cov2) %>%
      mutate(influenza.a = influenza.a * 10000000,
             influenza.b = influenza.b * 10000000,
             rsv = rsv * 10000000,
             sars.cov2 = sars.cov2*10000000) %>%
      mutate(across(where(is.numeric), as.integer)) %>%
      #filter(date>= "2023-01-01") %>% # for respiratory only
      arrange(date)
    
# -----------------------------------------------
 #  # Load Prophet-XGBoost model
    # Create a temporary environment
    temp_env <- new.env()
  
  #Prophet + XGBoost
   set.seed(0112358)
  
  phxgb_filename <- here(outp, paste0( "1.1-model-tune-phxgb-final_", dataset_name,"_", encounter_type,  ".RData"))
  load(here(phxgb_filename), envir = temp_env)
 print(paste("Loaded Prophet-XGBoost model for",encounter_type,  dataset_name))
 splits<-temp_env$splits
 #  -----------------------------------------------
  # Tune model
 wflw_phxgb_tune <- temp_env$wflw_phxgb_tune
 tune_results_phxgb <- temp_env$tune_results_phxgb
 splits <- temp_env$splits  
 
 wflw_fit <- wflw_phxgb_tune |>
   finalize_workflow(select_best(tune_results_phxgb, metric = "rmse")) |>
   fit(training(splits))
 
 # generate modeltime table ---------------------------------------------------
 model_tbl <- modeltime_table(wflw_fit)
 
 # Training error metrics ---------------------------------------------------
 training_preds <- model_tbl %>%
   modeltime_calibrate(new_data = training(splits)) %>%
   select(.model_desc, .calibration_data) %>%
   unnest(cols = c(.calibration_data)) %>%
   mutate(.model_desc = "PROPHETXGB")
 
 df_training_metrics <- training_preds %>%
   group_by(.model_desc) %>%
   summarise(
     mdae = Metrics::mdae(.actual, .prediction),
     mae = Metrics::mae(.actual, .prediction),
     rmse = Metrics::rmse(.actual, .prediction),
     mape = Metrics::mape(.actual, .prediction),
     rse = Metrics::rse(.actual, .prediction),
     smape = Metrics::smape(.actual, .prediction),
     r2 = round(1 - sum((.actual - .prediction)^2) / sum((.actual - mean(.actual))^2), 2)
   ) %>%
   mutate(
     dataset = dataset_name,
     encounter_type = encounter_type,
     data_type = "training"
   )
 
 # Testing error metrics ----------------------------------------------------
 test_preds <- model_tbl %>%
   modeltime_calibrate(new_data = testing(splits)) %>%
   select(.model_desc, .calibration_data) %>%
   unnest(cols = c(.calibration_data)) %>%
   mutate(.model_desc = "PROPHETXGB")
 
 df_testing_metrics <- test_preds %>%
   group_by(.model_desc) %>%
   summarise(
     mdae = Metrics::mdae(.actual, .prediction),
     mae = Metrics::mae(.actual, .prediction),
     rmse = Metrics::rmse(.actual, .prediction),
     mape = Metrics::mape(.actual, .prediction),
     rse = Metrics::rse(.actual, .prediction),
     smape = Metrics::smape(.actual, .prediction),
     r2 = round(1 - sum((.actual - .prediction)^2) / sum((.actual - mean(.actual))^2), 2)
   ) %>%
   mutate(
     dataset = dataset_name,
     encounter_type = encounter_type,
     data_type = "testing"
   )
 
 print(df_training_metrics)
 print(df_testing_metrics)
 
 # Append results
 results_list[[length(results_list) + 1]] <- df_training_metrics
 results_list[[length(results_list) + 1]] <- df_testing_metrics
 
 # Save individual metrics
 saveRDS(df_training_metrics, here(outp, paste0("2.1-model-training-errors_", dataset_name, "_", encounter_type, ".rds")))
 saveRDS(df_testing_metrics, here(outp, paste0("2.1-model-test-errors_", dataset_name, "_", encounter_type, ".rds")))
  }
}

# Combine all metrics into one table ------------------------------------------
results_train_test_metrics <- bind_rows(results_list)

# Save final performance metrics
write.csv(results_train_test_metrics, here(outp, "performance_metrics.csv"), row.names = FALSE)