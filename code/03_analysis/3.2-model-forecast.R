# -------------------------------------------------------------------------------
#-------------Los Angeles Wildfires- ITS analysis------------------------------#   
#-------------------------R code-----------------------------------------------#
#-------------------------Date:2/10/25------------------------------------------#

# Code adapted from the following project:

# @project: Two-stage interrupted time series design
# @author: Arnab K. Dey (arnabxdey@gmail.com), Yiqun Ma
# @organization: Scripps Institution of Oceanography, UC San Diego
# @description: This script fits the selected models to training data and generates forecasts for the entire dataset with bootstrapped confidence intervals
# @date: Dec 16, 2024

# load libraries ----------------------------------------------------------------
rm(list = ls())
set.seed(0112358)
pacman::p_load(here, tidymodels, tidyverse, modeltime)

# Lara directories
inp <- "/Users/larasch/Documents/UCB_postdoc/Research/los_angeles_2025_fires_rapid_response/los_angeles_2025_fire_disasters_exp/data/01_raw/"
outp <- "/Users/larasch/Documents/UCB_postdoc/Research/los_angeles_2025_fires_rapid_response/los_angeles_2025_fire_disasters_exp/Outputs/"
mod <- "/Users/larasch/Documents/UCB_postdoc/Research/los_angeles_2025_fires_rapid_response/los_angeles_2025_fire_disasters_exp/Outputs/"


# List of datasets to iterate over
datasets<- c("df_Virtual_high", "df_OP_high", "df_ED_high", "df_IP_high")

# List of encounter types to loop through
#encounter_types <- c("num_enc")
encounter_types <- c("num_enc", "num_enc_resp", "num_enc_cardio", "num_enc_resp", "num_enc_neuro", "num_enc_injury")

# Loop through each dataset and load the models and process results
for (dataset_name in datasets) {
  
  # Loop through each encounter type and create a recipe
  for (encounter_type in encounter_types) {
    
  # # Create filename for the test error metrics
  # test_errors_filename <- paste0("Outputs/", "2.2-model-test-errors_", dataset_name, "_", encounter_type, ".rds")
  # 
  # # Load the test error metrics for the dataset
  # df_testing_metrics <- readRDS(test_errors_filename)
  # 
  # # Find the row number of the model with the lowest RMSE
  # best_model_row <- which.min(df_testing_metrics$rmse)  # Get the index of the lowest RMSE
  # 
  # # Dynamically create the filename for model_tbl_best_all for this dataset
  # model_tbl_best_filename <- paste0( mod, "2.1-model-select-best_", dataset_name, "_", encounter_type,".rds")
  # 
  # # Load the model_tbl_best_all from disk for this iteration
  # model_tbl_best_all <- readRDS(model_tbl_best_filename)
  # 
  # # Use the row number to select the corresponding row from model_tbl_best_all
  # model_tbl_best <- model_tbl_best_all[best_model_row, , drop = FALSE]  # Extract the best model row
  # 
  # # Extract the .model_desc from the best model
  # best_model_desc <- model_tbl_best$.model_desc
# load model table with best models ---------------------------------------------------
model_tbl_best_all <- readRDS(paste0(mod, "2.1-model-select-best_" , dataset_name, "_", encounter_type, ".rds"))
# Question- how to identify best model?
model_tbl_best <- model_tbl_best_all #|> filter(str_detect(.model_desc, best_model_desc)) # Prophet was identified as the best model in script 2.2

# source script for bootstrap ---------------------------------------------------
source(here("code/03_analysis/", "3.1-func-generate-MC-CIs.R"))

# ensure consistent numeric precision ----------------------------------------------
options(digits = 7)
options(scipen = 999)

# load data ---------------------------------------------------
#preintervention_filename <- paste0("Outputs/df-train-test_sf_", dataset_name, ".csv") # lara will toggle on
preintervention_filename <- paste0(outp,"df-train-test_sf_", dataset_name, ".csv") 
df_preintervention <- read.csv(here(preintervention_filename)) %>%
  mutate(date = as.Date(date)) %>%
  mutate(encounter_type=as.integer(encounter_type))%>%
  select(date, encounter_type, pr, tmmx, tmmn, rmin, rmax, vs, srad, postjan7)  # Dynamically select the encounter type column

# all_cases_filename <- paste0("Outputs/df-predict-sf_", dataset_name, ".csv") # lara will toggle on
all_cases_filename <-  paste0(outp,"df-predict-sf_", dataset_name, ".csv") 
df_all_cases <- read.csv(here(all_cases_filename)) %>%
  mutate(date = as.Date(date)) %>%
  mutate(encounter_type=as.integer(encounter_type))%>%
  select(date, encounter_type, pr, tmmx, tmmn, rmin, rmax, vs, srad, postjan7)  # Dynamically select the encounter type column

#forecast on all cases with bootstrapped CIs ------------------------------------------
#forecast_cis <- generate_forecast_intervals(
forecast_cis <- generate_forecast_values(
  model_spec = model_tbl_best,
  training_data = df_preintervention,
  forecast_horizon_data = df_all_cases #,
 # n_iterations = 1000
)
forecast_cis<-forecast_cis[,1:2]
colnames(forecast_cis) <- c("date", "num_pred")
#, "conf_lo", "conf_hi")

# Merge with actuals ---------------------------------------------------
df_forecast <- df_all_cases |>
    rename(num_cases = encounter_type) |>
    select(date, num_cases)  |>
    left_join(forecast_cis, by = "date")

# Create the plot using ggplot2
p <- ggplot(df_forecast, aes(x = date)) +
  # Line for num_cases
  geom_line(aes(y = num_cases, color = "Actual Cases"), size = 1) +
  # Line for num_pred
  geom_line(aes(y = num_pred, color = "Predicted Cases"), size = 1, linetype = "dashed") +
  labs(
    title = paste("Forecast for", encounter_type, "in", dataset_name),    x = "Date",
    y = "Number of Cases"
  ) +
  scale_color_manual(values = c("Actual Cases" = "blue", "Predicted Cases" = "red")) +
  theme_minimal()

# Save the plot to a file
file_name <- paste0(outp, "forecast_", unique(df_forecast$encounter_type), "_", deparse(substitute(dataset)), ".png")
ggsave(file_name, plot = p)

# Add the .model_desc to the final forecast dataset
#df_forecast$model_desc <- best_model_desc

# save final predictions ---------------------------------------------------
final_pred_filename <- paste0("Outputs/", "3.2-final-preds_", dataset_name, "_", encounter_type, ".rds")
df_forecast |> saveRDS(here(final_pred_filename))
  }  # End of encounter type loop
}  # End of dataset loop
