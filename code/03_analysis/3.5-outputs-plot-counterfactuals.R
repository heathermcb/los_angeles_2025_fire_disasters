# -------------------------------------------------------------------------------
#-------------Los Angeles Wildfires- ITS analysis------------------------------#   
#-------------------------R code-----------------------------------------------#
#-------------------------Date:2/25/25------------------------------------------#

# Code adapted from the following project:

# -------------------------------------------------------------------------------
# @project: Two-stage interrupted time series design
# @author: Arnab K. Dey (arnabxdey@gmail.com), Yiqun Ma
# @organization: Scripps Institution of Oceanography, UC San Diego
# @description: This script generates counterfactual plots comparing actual vs predicted respiratory cases
# @date: Dec 20, 2024

# load libraries ----------------------------------------------------------------
rm(list = ls())
pacman::p_load(tidyverse, janitor, data.table, fst, openxlsx, here)

# Server directories
inp <- "D:/Lara/los_angeles_2025_fires_rapid_response/los_angeles_2025_fire_disasters_exp/data/01_raw/"
mod <- "D:/Lara/los_angeles_2025_fires_rapid_response/los_angeles_2025_fire_disasters_exp/Outputs/"
outp <- "D:/Lara/los_angeles_2025_fires_rapid_response/los_angeles_2025_fire_disasters_exp/Outputs/final_results/"


# List of datasets to iterate over
#datasets<- c(  "df_OP_moderate")
datasets<- c("df_Virtual_high", "df_OP_high", "df_OP_moderate", "df_Virtual_moderate") #, "df_OP_moderate")

# List of encounter types to loop through
#encounter_types <- c("num_enc")
encounter_types <- c("num_enc", "num_enc_cardio",  "num_enc_neuro", "num_enc_injury", "num_enc_resp")
# set paths ---------------------------------------------------
#source(here("paths-mac.R"))

# Loop through each dataset and load the models and process results
for (dataset_name in datasets) {
  
  # Loop through each encounter type and create a recipe
  for (encounter_type in encounter_types) {
    
# load original data and forecasted predictions -------------------------------------
    forecast_filename <- paste0(outp, "3.2-final-preds_", dataset_name, "_", encounter_type, ".rds")
    
    df_forecast <- readRDS(here(forecast_filename))

# source function to generate counterfactual plots -------------------------------------
source(here("code/03_analysis/", "3.4-func-plot-counterfactuals.R"))

# generate counterfactual plots ---------------------------------------------------
plot_phxgb <- func_plot_counterfactual_boot(df_forecast, model_name = "PROPHETXGB", start_time=1,
                                            intervention_time = 191,  encounter_type = encounter_type, #intervention_time= 251 for non-resp outcomes
                                            dataset_name = dataset_name)

# save the plot ---------------------------------------------------
    plot_filename <- paste0(outp, "3.5-plot-counterfac-phxgb-boot", dataset_name, "_", encounter_type, ".png")
    
    ggsave(here(plot_filename),
        plot_phxgb,
        width = 10,
        height = 6,
        units = "in",
        dpi = 300,
        bg = "white")
}}