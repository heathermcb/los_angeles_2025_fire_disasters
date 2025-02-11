# -------------------------------------------------------------------------------
#-------------Los Angeles Wildfires- ITS analysis------------------------------#   
#-------------------------R code-----------------------------------------------#
#-------------------------Date:2/11/25------------------------------------------#

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
inp <- "/Users/larasch/Documents/UCB_postdoc/Research/los_angeles_2025_fires_rapid_response/los_angeles_2025_fire_disasters_exp/data/01_raw/"
outp <- "/Users/larasch/Documents/UCB_postdoc/Research/los_angeles_2025_fires_rapid_response/los_angeles_2025_fire_disasters_exp/Outputs/"
mod <- "/Users/larasch/Documents/UCB_postdoc/Research/los_angeles_2025_fires_rapid_response/los_angeles_2025_fire_disasters_exp/Outputs/"

# List of dataset names
datasets<- c("df_Virtual_high", "df_OP_high", "df_ED_high", "df_IP_high")

#datasets <- c("df_2022_2023_Virtual_high", "df_2023_2024_Virtual_high", "df_2024_2025_Virtual_high", "df_2022_2023_OP_high", "df_2023_2024_OP_high", "df_2024_2025_OP_high")

#,"df_2023_2024_Virtual_high",  "df_2023_2024_OP_high", "df_2024_2025_OP_high")

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
encounter_types <- c( "num_enc")
#encounter_types <- c("num_enc", "num_enc_resp", "num_enc_cardio", "num_enc_resp", "num_enc_neuro", "num_enc_injury")

#""num_enc_cardio", ", "num_enc_neuro",   "num_enc_injury"
# Loop through each dataset and load the models and process results
for (dataset_name in datasets) {
  # Loop through each encounter type and create a recipe
  for (encounter_type in encounter_types) {
    

#dataset<-get(dataset_name)

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
# # # Load ARIMA model
arima_filename <- paste0("Outputs/", "1.1-model-tune-arima-final_", dataset_name, "_", encounter_type, ".RData")
load(here(arima_filename))
print(paste("Loaded ARIMA model for",encounter_type,  dataset_name))

# # # Load NNETAR model
# #rm(list = ls(pattern = "num_enc_neuro"))
# nnetar_filename <- paste0("Outputs/", "1.2-model-tune-nnetar-final_", dataset_name, "_", encounter_type,".RData")
# load(here(nnetar_filename))
# print(paste("Loaded NNETAR model for", encounter_type, dataset_name))

# Load Prophet-XGBoost model
# #rm(list = ls(pattern = "num_enc_neuro"))
#  phxgb_filename <- paste0(mod, "1.3-model-tune-phxgb-final_", dataset_name,"_", encounter_type,  ".RData")
#   
#  load(here(phxgb_filename))
# print(paste("Loaded Prophet-XGBoost model for", encounter_type, dataset_name))
# encounter_types <- c("num_enc")

# step-1: select best models ---------------------------------------------------
set.seed(0112358)
## ARIMA
wflw_fit_arima_tuned <- wflw_arima_tune |>
  finalize_workflow(
    select_best(tune_results_arima, metric = "rmse")
  ) |>
  fit(training(splits))

# # ## NNETAR
# # set.seed(0112358)
# wflw_fit_nnetar_tuned <- wflw_nnetar_tune |>
#   finalize_workflow(
#     select_best(tune_results_nnetar, metric = "rmse")
#   ) |>
#   fit(training(splits))

## Prophet + XGBoost - run this
# set.seed(0112358)
# 
# 
#   wflw_fit_phxgb_tuned <- wflw_phxgb_tune |>
#     finalize_workflow(select_best(tune_results_phxgb, metric = "rmse")) |>
#     fit(training(splits))
# 

# step-2: generate modeltime table ---------------------------------------------------
set.seed(0112358)
model_tbl_best_all <- modeltime_table(
    wflw_fit_arima_tuned #,
   #wflw_fit_nnetar_tuned,
  # wflw_fit_phxgb_tuned
)


output_filename <- paste0("2.1-model-select-best_", dataset_name,"_", encounter_type, ".rds")

# Save the model_tbl_best_all to an RDS file for the current dataset
#model_tbl_best_all |> saveRDS(here("Outputs", output_filename)) #lara will toggle on
model_tbl_best_all |> saveRDS(here(mod, output_filename)) 


}
}