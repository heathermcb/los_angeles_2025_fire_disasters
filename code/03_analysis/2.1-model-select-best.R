# -------------------------------------------------------------------------------
#-------------Los Angeles Wildfires- ITS analysis------------------------------#   
#-------------------------R code-----------------------------------------------#
#-------------------------Date:2/20/25------------------------------------------#

# Code adapted from the following project:


# @project: Two-stage interrupted time series design
# @author: Arnab K. Dey (arnabxdey@gmail.com), Yiqun Ma
# @organization: Scripps Institution of Oceanography, UC San Diego
# @description: This script selects the best models based on RMSE and creates a modeltime table for comparison
# @date: Dec 16, 2024

# load libraries ----------------------------------------------------------------
rm(list = ls())
set.seed(0112358)
pacman::p_load(here, tidymodels, tidyverse, modeltime, dplyr)

## nina directories
#inp <- "~/Desktop/projects/casey cohort/LA-wildfires/data/raw-data/"
#outp <- "~/Desktop/projects/casey cohort/LA-wildfires/data/processed-data/"
#mod <- "~/Desktop/projects/casey cohort/LA-wildfires/data/model-output/"

# Lara directories
#inp <- "/Users/larasch/Documents/UCB_postdoc/Research/los_angeles_2025_fires_rapid_response/los_angeles_2025_fire_disasters_exp/data/01_raw/"
#outp <- "/Users/larasch/Documents/UCB_postdoc/Research/los_angeles_2025_fires_rapid_response/los_angeles_2025_fire_disasters_exp/Outputs/"
#mod <- "/Users/larasch/Documents/UCB_postdoc/Research/los_angeles_2025_fires_rapid_response/los_angeles_2025_fire_disasters_exp/Outputs/"

# Server directories
# inp <- "D:/Lara/los_angeles_2025_fires_rapid_response/los_angeles_2025_fire_disasters_exp/data/01_raw/"
# mod <- "D:/Lara/los_angeles_2025_fires_rapid_response/los_angeles_2025_fire_disasters_exp/Outputs/"
# #outp <- "D:/Lara/los_angeles_2025_fires_rapid_response/los_angeles_2025_fire_disasters_exp/Outputs/final_results/"
# outp <- "D:/Lara/los_angeles_2025_fires_rapid_response/los_angeles_2025_fire_disasters_exp/Outputs/testing/"

# Server directories
inp <- "D:/Lara/los_angeles_2025_fires_rapid_response/los_angeles_2025_fire_disasters_exp/data/01_raw/"
mod <- "D:/Lara/los_angeles_2025_fires_rapid_response/los_angeles_2025_fire_disasters_exp/Outputs/"
outp <- "D:/Lara/los_angeles_2025_fires_rapid_response/los_angeles_2025_fire_disasters_exp/Outputs/final_results/"
#outp <- "D:/Lara/los_angeles_2025_fires_rapid_response/los_angeles_2025_fire_disasters_exp/Outputs/testing/"

# List of dataset names
datasets<- c( "df_OP_high", "df_Virtual_high", "df_OP_moderate", "df_Virtual_moderate") 
#datasets<- ("df_Virtual_moderate")

# List of encounter types to loop through
#encounter_types <- c("num_enc_resp") #test
encounter_types <- c( "num_enc", "num_enc_resp", "num_enc_cardio",  "num_enc_neuro", "num_enc_injury")

# Loop through each dataset and load the models and process results
for (dataset_name in datasets) {
  # Loop through each encounter type and create a recipe
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
    arrange(date)

# load tuned models ---------------------------------------------------
# # Load ARIMA model
# arima_filename <- paste0("Outputs/", "1.1-model-tune-arima-final_", dataset_name, "_", encounter_type, ".RData")
# load(here(arima_filename))
# print(paste("Loaded ARIMA model for",encounter_type,  dataset_name))

# # # Load NNETAR model
# #rm(list = ls(pattern = "num_enc_neuro"))
# nnetar_filename <- paste0("Outputs/", "1.2-model-tune-nnetar-final_", dataset_name, "_", encounter_type,".RData")
# load(here(nnetar_filename))
# print(paste("Loaded NNETAR model for", encounter_type, dataset_name))

##Load Prophet-XGBoost model
phxgb_filename <- paste0(outp, "1.3-model-tune-phxgb-final_", dataset_name,"_", encounter_type,  ".RData")

# Create a temporary environment
temp_env <- new.env()

load(here(phxgb_filename), envir = temp_env)
print(paste("Loaded Prophet-XGBoost model for", encounter_type, dataset_name))

# step-1: select best models ---------------------------------------------------
#set.seed(0112358)
## ARIMA
# wflw_fit_arima_tuned <- wflw_arima_tune |>
#   finalize_workflow(
#     select_best(tune_results_arima, metric = "rmse")
#   ) |>
#   fit(training(splits))

# # ## NNETAR
# # set.seed(0112358)
# wflw_fit_nnetar_tuned <- wflw_nnetar_tune |>
#   finalize_workflow(
#     select_best(tune_results_nnetar, metric = "rmse")
#   ) |>
#   fit(training(splits))

#Prophet + XGBoost - run this
set.seed(0112358)

# Then finalize the workflow and fit it from the temporary environment
# Access objects explicitly
wflw_phxgb_tune <- temp_env$wflw_phxgb_tune
tune_results_phxgb <- temp_env$tune_results_phxgb
splits <- temp_env$splits  

  wflw_fit_phxgb_tuned <- wflw_phxgb_tune |>
    finalize_workflow(select_best(tune_results_phxgb, metric = "rmse")) |>
    fit(training(splits))


# step-2: generate modeltime table ---------------------------------------------------
set.seed(0112358)
model_tbl_best_all <- modeltime_table(
    #wflw_fit_arima_tuned #,
   #wflw_fit_nnetar_tuned,
   wflw_fit_phxgb_tuned
)


output_filename <- paste0("2.1-model-select-best_", dataset_name,"_", encounter_type, ".rds")

# Save the model_tbl_best_all to an RDS file for the current dataset
model_tbl_best_all |> saveRDS(here(outp, output_filename)) 


}
}