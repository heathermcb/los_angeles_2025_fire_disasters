# -----------------------------------------------------------------------------#
#-------------Los Angeles Wildfires- ITS analysis------------------------------#   
#-------------------------R code-----------------------------------------------#
#-----------------Last update:3/4/25------------------------------------------#


# Code adapted from the following project:

# @project: Two-stage interrupted time series design
# @author: Arnab K. Dey (arnabxdey@gmail.com), Yiqun Ma
# @organization: Scripps Institution of Oceanography, UC San Diego
# @description: This script loads the modeltime table with best models and calculates training and testing error metrics
# @date: Dec 16, 2024

# load libraries ----------------------------------------------------------------
rm(list = ls())
set.seed(0112358)
pacman::p_load(here, tidymodels, tidyverse, modeltime, metrics)

# nina directories
# inp <- "~/Desktop/projects/casey cohort/LA-wildfires/data/raw-data/"
# outp <- "~/Desktop/projects/casey cohort/LA-wildfires/data/processed-data/"
# mod <- "~/Desktop/projects/casey cohort/LA-wildfires/data/model-output/"

# Server directories
inp <- "D:/Lara/los_angeles_2025_fires_rapid_response/los_angeles_2025_fire_disasters_exp/data/01_raw/"
mod <- "D:/Lara/los_angeles_2025_fires_rapid_response/los_angeles_2025_fire_disasters_exp/Outputs/"
outp <- "D:/Lara/los_angeles_2025_fires_rapid_response/los_angeles_2025_fire_disasters_exp/Outputs/final_results/"

# ensure consistent numeric precision ----------------------------------------------
options(digits = 7)
options(scipen = 999)

# load data ---------------------------------------------------
# List of dataset names
datasets<- c( "df_Virtual_high")
#datasets<- c("df_Virtual_high", "df_Virtual_moderate", "df_OP_high", "df_OP_moderate") 



# List of encounter types to loop through
encounter_types <- c("num_enc")
#encounter_types <- c( "num_enc", "num_enc_resp", "num_enc_cardio",  "num_enc_neuro", "num_enc_injury")

# Initialize an empty list to store results
results_list <- list()

# Loop through each dataset and load the models and process results
for (dataset_name in datasets) {

  # Loop through each encounter type 
  for (encounter_type in encounter_types) {
 
      # load data ---------------------------------------------------
    preintervention_filename <- paste0(mod,"df-train-test_sf_", dataset_name, ".csv") 
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
    
    all_cases_filename <-  paste0(mod,"df-predict-sf_", dataset_name, ".csv") 
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
      # select(date, encounter_type, pr, tmmx, tmmn, rmin, rmax, vs, srad, postjan7, time_period) %>%      

    
    
#load tuned models ---------------------------------------------------
# # Load ARIMA model
# arima_filename <- paste0("Outputs/", "1.1-model-tune-arima-final_", dataset_name, "_", encounter_type, ".RData")
# load(here(arima_filename))
# #print(paste("Loaded ARIMA model for",encounter_type,  dataset_name))

  # -----------------------------------------------
  # # Load NNETAR model
  # rm(list = ls(pattern = "num_enc_neuro"))
  # nnetar_filename <- paste0("Outputs/", "1.2-model-tune-nnetar-final_", dataset_name, "_", encounter_type, ".RData")
  # load(here(nnetar_filename))
  # #print(paste("Loaded NNETAR model for",encounter_type,  dataset_name))

  # -----------------------------------------------
 #  # Load Prophet-XGBoost model
   rm(list = ls(pattern = encounter_type))
 # phxgb_filename <- paste0("Outputs/", "1.3-model-tune-phxgb-final_", dataset_name,"_", encounter_type,  ".RData") #lara will toggle on
  phxgb_filename <- paste0(mod, "1.3-model-tune-phxgb-final_", dataset_name,"_", encounter_type,  ".RData")
  load(here(phxgb_filename))
 #  #print(paste("Loaded Prophet-XGBoost model for",encounter_type,  dataset_name))
 # #  # -----------------------------------------------
  # Load model table with best models
  # Construct file path
  model_tbl_filename <- paste0(mod, "2.1-model-select-best_", dataset_name, "_", encounter_type, ".rds")
  # load model table with best models ---------------------------------------------------
  model_tbl_best_all <- readRDS(here(model_tbl_filename))

# -----------------------------------------------
 #  # Load Prophet-XGBoost model
    # Create a temporary environment
    temp_env <- new.env()
  
  #Prophet + XGBoost
   set.seed(0112358)
    
  phxgb_filename <- paste0(outp, "1.1-model-tune-phxgb-final_", dataset_name,"_", encounter_type,  ".RData")
  load(here(phxgb_filename), envir = temp_env)
 print(paste("Loaded Prophet-XGBoost model for",encounter_type,  dataset_name))
 splits<-temp_env$splits
 #  -----------------------------------------------
  # Tune model
 wflw_phxgb_tune <- temp_env$wflw_phxgb_tune
 tune_results_phxgb <- temp_env$tune_results_phxgb
 splits <- temp_env$splits  
 
 wflw_fit_phxgb_tuned <- wflw_phxgb_tune |>
   finalize_workflow(select_best(tune_results_phxgb, metric = "rmse")) |>
   fit(training(splits))
 
 
 # step-2: generate modeltime table ---------------------------------------------------
 set.seed(0112358)
 model_tbl_best_all <- modeltime_table(
   wflw_fit_phxgb_tuned
 )
 
# step-1: generate training error rates -----------------------------------------------
## calibrate best models on training data and format model descriptions
training_preds <- model_tbl_best_all |>
  modeltime_calibrate(
    new_data = training(splits),  #> # Create month_day variable based on date
    quiet = FALSE
  ) |>
  select(.model_desc, .calibration_data) |>
  unnest(cols = c(.calibration_data)) |>
  mutate(.model_desc = case_when(
    stringr::str_detect(.model_desc, "REGRESSION WITH ARIMA") ~ "ARIMA",
    stringr::str_detect(.model_desc, "ARIMA") & stringr::str_detect(.model_desc, "XGBOOST") ~ "ARIMAXGB",
    stringr::str_detect(.model_desc, "PROPHET") ~ "PROPHETXGB",
    stringr::str_detect(.model_desc, "NNAR") ~ "NNETAR",
    TRUE ~ .model_desc
  ))

## generate training error metrics
df_training_metrics <- training_preds |>
  group_by(.model_desc) |>
  summarise(
    mdae = Metrics::mdae(.actual, .prediction),
    mae = Metrics::mae(.actual, .prediction),
    rmse = Metrics::rmse(.actual, .prediction),
    mape = Metrics::mape(.actual, .prediction),
    rse = Metrics::rse(.actual, .prediction),
    smape = Metrics::smape(.actual, .prediction),
    r2 = round(1 - sum((.actual - .prediction)^2) / sum((.actual - mean(.actual))^2), 2)
  ) %>%
  # Add dataset and encounter type information
  mutate(
    dataset = dataset_name,
    encounter_type = encounter_type,
    data_type = "training"
  )

# Save training predictions

training_errors_filename <- paste0(outp, "2.1-model-training-errors_", dataset_name, "_", encounter_type, ".rds")

df_training_metrics |> saveRDS(here(training_errors_filename))
print(paste("Saved training error metrics for", encounter_type, dataset_name))

# step-2: generate test error rates ------------------------------------------
## calibrate best models on test data and format model descriptions
test_preds <- model_tbl_best_all |>
  modeltime_calibrate(
    new_data = testing(splits),
    # id = "county",
    quiet = FALSE
  ) |>
  select(.model_desc, .calibration_data) |>
  unnest(cols = c(.calibration_data)) |>
  mutate(.model_desc = case_when(
    stringr::str_detect(.model_desc, "REGRESSION WITH ARIMA") ~ "ARIMA",
    stringr::str_detect(.model_desc, "ARIMA") & stringr::str_detect(.model_desc, "XGBOOST") ~ "ARIMAXGB",
    stringr::str_detect(.model_desc, "PROPHET") ~ "PROPHETXGB",
    stringr::str_detect(.model_desc, "NNAR") ~ "NNETAR",
    TRUE ~ .model_desc
  ))

## generate test error metrics
df_testing_metrics <- test_preds |>
  group_by(.model_desc) |>
  summarise(
    mdae = Metrics::mdae(.actual, .prediction),
    mae = Metrics::mae(.actual, .prediction),
    rmse = Metrics::rmse(.actual, .prediction),
    mape = Metrics::mape(.actual, .prediction),
    rse = Metrics::rse(.actual, .prediction),
    smape = Metrics::smape(.actual, .prediction),
    r2 = round(1 - sum((.actual - .prediction)^2) / sum((.actual - mean(.actual))^2), 2)
  ) %>%
# Add dataset and encounter type information
mutate(
  dataset = dataset_name,
  encounter_type = encounter_type,
  data_type = "testing"
)

print(df_training_metrics)
print(df_testing_metrics)

# Combine training and testing results
results_list[[length(results_list) + 1]] <- df_training_metrics
results_list[[length(results_list) + 1]] <- df_testing_metrics

# Save test predictions
test_errors_filename <- paste0(outp, "2.1-model-test-errors_", dataset_name, "_", encounter_type, ".rds")

df_testing_metrics |> saveRDS(here(test_errors_filename))
print(paste("Saved test error metrics for", encounter_type, dataset_name))
  }
}

# Combine all results into a single dataframe
results_train_test_metrics <- bind_rows(results_list)

# Save the final model performance metrics in a csv
final_results_filename <- paste0(outp, "model_performance_metrics.csv")
write.csv(results_train_test_metrics, here(final_results_filename))