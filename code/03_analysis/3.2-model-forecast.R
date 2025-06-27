# -------------------------------------------------------------------------------
#-------------Los Angeles Wildfires- ITS analysis------------------------------#   
#-------------------------R code-----------------------------------------------#
#-----------------Last update:6/27/25------------------------------------------#

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

# set paths
source("paths.R")

# load data ---------------------------------------------------
# List of dataset names
#datasets<- c( "df_Virtual_high", "df_Virtual_moderate", "df_Virtual_least")
datasets<- c("df_Virtual_high", "df_Virtual_moderate", "df_OP_high", "df_OP_moderate") 


# List of encounter types to loop through
#encounter_types <- c("num_enc_resp")
encounter_types <- c( "num_enc", "num_enc_cardio",  "num_enc_neuro", "num_enc_injury")

# Loop through each dataset and load the models and process results
for (dataset_name in datasets) {
  
  # Loop through each encounter type and create a recipe
  for (encounter_type in encounter_types) {
    
    # load model table with best models ---------------------------------------------------
    
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
    
    wflw_fit_phxgb_tuned <- wflw_phxgb_tune |>
      finalize_workflow(select_best(tune_results_phxgb, metric = "rmse")) |>
      fit(training(splits))
    
    
    # step-2: generate modeltime table ---------------------------------------------------
    set.seed(0112358)
    model_tbl_best <- modeltime_table(
      wflw_fit_phxgb_tuned
    )
    
    # source script for bootstrap ---------------------------------------------------
    source(here("code/03_analysis/", "3.1-func-generate-MC-CIs.R"))
    
    # ensure consistent numeric precision ----------------------------------------------
    options(digits = 7)
    options(scipen = 999)
    
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
      #filter(date>= "2023-01-01") %>% # for resp Virtual
      arrange(date)
    
    all_cases_filename <-  here(mod, paste0("df-predict-sf_", dataset_name, ".csv") )
    df_all_cases <- read.csv(here(all_cases_filename)) %>%
      mutate(date = as.Date(date)) %>%
      select(date, all_of(encounter_type), sars.cov2, pr, tmmx, tmmn, rmin, rmax, vs, srad, postjan7, time_period, influenza.a, influenza.b, rsv) %>%
      mutate(influenza.a = influenza.a * 10000000,
             influenza.b = influenza.b * 10000000,
             rsv = rsv * 10000000,
             sars.cov2 = sars.cov2*10000000) %>%     
      mutate(across(where(is.numeric), as.integer)) %>%
       #filter(date>= "2023-01-01") %>% # for resp Virtual
      arrange(date)
    
    workflow_best <- model_tbl_best$.model[[1]]  # Extract the first model
    
    trained_recipe <- workflow_best |> extract_recipe()
    
    ## Bake the variables to the full dataset
    new_vars <- bake(trained_recipe, df_all_cases)  %>% #bakes in model and adds prediction variables
      select(-any_of(names(df_all_cases)))  # Removes columns that already exist
    
    df_all_cases <- bind_cols(df_all_cases, new_vars) #combines baked variables with original 
    
    #forecast on all cases with bootstrapped CIs ------------------------------------------
    forecast_cis <- generate_forecast_intervals(
      model_spec = model_tbl_best,
      training_data = df_preintervention,
      forecast_horizon_data = df_all_cases, 
      n_iterations = 1000
    )
    
    colnames(forecast_cis) <- c("date", "num_pred", "conf_lo", "conf_hi")
    
    # Merge with actuals ---------------------------------------------------
    df_forecast <- df_all_cases |>
      rename(num_cases = encounter_type) |>
      select(date, num_cases)  |>
      left_join(forecast_cis, by = "date") |>
      arrange(date) |>
      mutate(time=row_number(),
             residuals=(num_pred-num_cases))
    
    
    # save final predictions ---------------------------------------------------
    final_pred_filename <- here(outp, paste0( "3.2-final-preds_", dataset_name, "_", encounter_type, ".rds"))
    df_forecast |> saveRDS(here(final_pred_filename))
  }  # End of encounter type loop
}  # End of dataset loop
