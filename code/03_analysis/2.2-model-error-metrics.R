# -------------------------------------------------------------------------------
# #-------------Los Angeles Wildfires- ITS analysis------------------------------#   
#-------------------------R code-----------------------------------------------#
#-------------------------Date:2/11/25------------------------------------------#

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

# Lara directories
inp <- "/Users/larasch/Documents/UCB_postdoc/Research/los_angeles_2025_fires_rapid_response/los_angeles_2025_fire_disasters_exp/data/01_raw/"
outp <- "/Users/larasch/Documents/UCB_postdoc/Research/los_angeles_2025_fires_rapid_response/los_angeles_2025_fire_disasters_exp/Outputs/"
mod <- "/Users/larasch/Documents/UCB_postdoc/Research/los_angeles_2025_fires_rapid_response/los_angeles_2025_fire_disasters_exp/Outputs/"


# ensure consistent numeric precision ----------------------------------------------
options(digits = 7)
options(scipen = 999)

# load data ---------------------------------------------------

# List of dataset names
datasets<- c("df_Virtual_high") , "df_OP_high", "df_ED_high", "df_IP_high")
dataset_name<-"df_Virtual_high"

# datasets <- c(
#   #"df_2022_2023_ED_high",
#   "df_2023_2024_ED_high",
#   "df_2024_2025_ED_high",
#   "df_2022_2023_OP_high",
#   "df_2023_2024_OP_high",
#   "df_2024_2025_OP_high",
#   "df_2022_2023_IP_high",
#   "df_2023_2024_IP_high",
#   "df_2024_2025_IP_high",
#   "df_2022_2023_Virtual_high",
#   "df_2023_2024_Virtual_high",
#   "df_2024_2025_Virtual_high"
# )

# List of encounter types to loop through
#encounter_types <- c("num_enc")
encounter_types <- c("num_enc", "num_enc_resp", "num_enc_cardio", "num_enc_resp", "num_enc_neuro", "num_enc_injury")


# Initialize an empty list to store results
results_list <- list()

# Loop through each dataset and load the models and process results
for (dataset_name in datasets) {

  # Loop through each encounter type 
  for (encounter_type in encounter_types) {
    
      # load data ---------------------------------------------------
    #preintervention_filename <- paste0("Outputs/df-train-test_sf_", dataset_name, ".csv") # lara will toggle on
    preintervention_filename <- paste0(outp,"df-train-test_sf_", dataset_name, ".csv") 
    df_preintervention <- read.csv(here(preintervention_filename)) %>%
      mutate(date = as.Date(date)) %>%
      select(date, encounter_type, pr, tmmx, tmmn, rmin, rmax, vs, srad, postjan7, time_period) %>%
      mutate(across(where(is.numeric), as.integer)) %>%
      arrange(date)
    
    # all_cases_filename <- paste0("Outputs/df-predict-sf_", dataset_name, ".csv") # lara will toggle on
    all_cases_filename <-  paste0(outp,"df-predict-sf_", dataset_name, ".csv") 
    df_all_cases <- read.csv(here(all_cases_filename)) %>%
      mutate(date = as.Date(date)) %>%
      select(date, encounter_type, pr, tmmx, tmmn, rmin, rmax, vs, srad, postjan7, time_period) %>%
      mutate(across(where(is.numeric), as.integer)) %>%
      arrange(date)
    
    
# load tuned models ---------------------------------------------------
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
  # Load Prophet-XGBoost model
  rm(list = ls(pattern = "num_enc_neuro"))
  phxgb_filename <- paste0("Outputs/", "1.3-model-tune-phxgb-final_", dataset_name,"_", encounter_type,  ".RData") #lara will toggle on
 # phxgb_filename <- paste0(mod, "1.3-model-tune-phxgb-final_", dataset_name,"_", encounter_type,  ".RData")
  load(here(phxgb_filename))
  #print(paste("Loaded Prophet-XGBoost model for",encounter_type,  dataset_name))
  # -----------------------------------------------
  # Load model table with best models
  # Construct file path
  model_tbl_filename <- paste0(mod, "2.1-model-select-best_", dataset_name, "_", encounter_type, ".rds")
  
  # Check if the file exists before reading
  if (!file.exists(here(model_tbl_filename))) {
    warning(paste("File not found, skipping:", model_tbl_filename))
    next  # Skip to the next iteration
  }
  
  # Try to load the model table, if it fails, print a warning and continue
  model_tbl_best_all <- tryCatch({
    readRDS(here(model_tbl_filename))
  }, error = function(e) {
    warning(paste("Skipping missing file:", model_tbl_filename))
    return(NULL)  # Return NULL so that processing can continue
  })
  
  # Skip this iteration if the file was not found
  if (is.null(model_tbl_best_all)) next
 # encounter_types <- c("num_enc")
  
# step-1: generate training error rates -----------------------------------------------
## calibrate best models on training data and format model descriptions
training_preds <- model_tbl_best_all |>
  modeltime_calibrate(
    # Question- what date to use here?
    new_data = training(splits),  #> # Create month_day variable based on date
     # mutate(month_day = format(date, "%m-%d")) |>
      # Filter based on month_day
     # filter(month_day > "11-01"),
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

## generate training error metrics
df_training_metrics <- training_preds |>
  # Question- what date to use here?
  # Create month_day variable based on date
  mutate(month_day = format(date, "%m-%d")) |>
  # Filter based on month_day
 # filter(month_day > "12-01") |>
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
training_errors_filename <- paste0(mod, "2.2-model-training-errors_", dataset_name, "_", encounter_type, ".rds")

df_training_metrics |> saveRDS(here(training_errors_filename))
#print(paste("Saved training error metrics for", encounter_type, dataset_name))

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
  # Create month_day variable based on date
  mutate(month_day = format(date, "%m-%d")) |>
  # Filter based on month_day
  #filter(month_day > "12-01") |>
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
#test_errors_filename <- paste0("Outputs/", "2.2-model-test-errors_", dataset_name, "_", encounter_type, ".rds") #lara will toggle on
test_errors_filename <- paste0(mod, "2.2-model-test-errors_", dataset_name, "_", encounter_type, ".rds")

#df_testing_metrics |> saveRDS(here(test_errors_filename))
#print(paste("Saved test error metrics for", encounter_type, dataset_name))
  }
}

# Combine all results into a single dataframe
results_train_test_metrics <- bind_rows(results_list)

# Save the final results dataset
final_results_filename <- paste0("Outputs/", "model_performance_metrics.rds")
saveRDS(results_train_test_metrics, here(final_results_filename))

# Print final results
print(results_train_test_metrics)