# -------------------------------------------------------------------------------
#-------------Los Angeles Wildfires- ITS analysis------------------------------#   
#-------------------------R code-----------------------------------------------#
#-------------------------Date:2/25/25------------------------------------------#

# Code adapted from the following project:

# @project: Two-stage interrupted time series design
# @author: Arnab K. Dey (arnabxdey@gmail.com), Yiqun Ma
# @organization: Scripps Institution of Oceanography, UC San Diego
# @description: This script configures, tunes, and fits a Prophet-XGBoost model to the aggregated data
# @date: Dec 16, 2024

# load libraries ----------------------------------------------------------------
rm(list = ls())
set.seed(0112358)
pacman::p_load(here, tidymodels, tidyverse, modeltime, timetk, tictoc)

## nina directories
#inp <- "~/Desktop/projects/casey cohort/LA-wildfires/data/raw-data/"
#outp <- "~/Desktop/projects/casey cohort/LA-wildfires/data/processed-data/"
#mod <- "~/Desktop/projects/casey cohort/LA-wildfires/data/model-output/"

# Lara directories
#inp <- "/Users/larasch/Documents/UCB_postdoc/Research/los_angeles_2025_fires_rapid_response/los_angeles_2025_fire_disasters_exp/data/01_raw/"
#outp <- "/Users/larasch/Documents/UCB_postdoc/Research/los_angeles_2025_fires_rapid_response/los_angeles_2025_fire_disasters_exp/Outputs/"
#mod <- "/Users/larasch/Documents/UCB_postdoc/Research/los_angeles_2025_fires_rapid_response/los_angeles_2025_fire_disasters_exp/Outputs/"

# Server directories
inp <- "D:/Lara/los_angeles_2025_fires_rapid_response/los_angeles_2025_fire_disasters_exp/data/01_raw/"
mod <- "D:/Lara/los_angeles_2025_fires_rapid_response/los_angeles_2025_fire_disasters_exp/Outputs/"
#outp <- "D:/Lara/los_angeles_2025_fires_rapid_response/los_angeles_2025_fire_disasters_exp/Outputs/final_results/"
outp <- "D:/Lara/los_angeles_2025_fires_rapid_response/los_angeles_2025_fire_disasters_exp/Outputs/testing/"

# ensure consistent numeric precision ----------------------------------------------
options(digits = 7)
options(scipen = 999)

# Loop through datasets --------------------------------------------------------

# List of dataset names
datasets<- c(  "df_Virtual_high", "df_OP_moderate", "df_Virtual_moderate") 
#datasets<- ("df_Virtual_moderate")"df_OP_high",

# List of encounter types to loop through
#encounter_types <- c("num_enc_resp") #test
encounter_types <- c( "num_enc", "num_enc_resp", "num_enc_cardio",  "num_enc_neuro", "num_enc_injury")

# Iterate over each dataset
for (dataset_name in datasets) {
  # Iterate over each dataset
  # Loop through each encounter type and create a recipe
  for (encounter_type in encounter_types) {
    
  df_train_test <-  paste0(mod,"df-train-test_sf_", dataset_name, ".csv")
  
  df_train_test <- read.csv(here(df_train_test)) %>%
    mutate(date = as.Date(date))

  
    
    # Subset the dataset to include only date and the current encounter type variable
  df_train_test_encounter <- df_train_test %>%
    select(date, all_of(encounter_type), pr, tmmx, tmmn, rmin, rmax, vs, srad, postjan7, time_period, influenza.a, influenza.b, rsv, sars.cov2) %>%
    mutate(influenza.a = influenza.a * 10000000,
           influenza.b = influenza.b * 10000000,
           rsv = rsv * 10000000,
           sars.cov2 = sars.cov2*10000000) %>%
    mutate(across(where(is.numeric), as.integer)) %>%
    arrange(date)
    
    # split data into training and test sets -------------------------------------
    set.seed(0112358)
    splits <- df_train_test_encounter |>
      time_series_split(
        #assess = "75 days", 
        assess = "75 days", 
        cumulative = TRUE,
        date_var = date
      )
    
    ## resample data ----
    set.seed(0112358)
    resamples_kfold <- training(splits) |> 
      time_series_cv(
        assess = "40 days",     
        slice_limit = 8,        
        cumulative = TRUE       
      )
    
    # recipe for modeling ---------------------------------------------------
    formula <- as.formula(paste(encounter_type, "~ ."))
    print(formula)
    
    rec_obj_phxgb <- recipe(formula, training(splits)) |>
      # Time series features 
      step_timeseries_signature(date) |>
      step_holiday(date, holidays = timeDate::listHolidays("US")) |>
      # Minimal seasonal components
      step_fourier(date, period = 7, K = 2) |>  # Weekly seasonality with minimal terms
      step_mutate(weekend = factor(if_else(wday(date, week_start = 1) %in% c(6, 7), "weekend", "weekday"))) |>
      step_dummy(weekend) |> 
      # cleaning steps
      step_rm(matches("(.iso$)|(.xts$)|(.minute)|(.second)|(.hour)|(.am.pm)")) |>
      step_zv() |>
      #step_corr(all_numeric_predictors(), threshold = 0.9) |> # Remove highly correlated features, changed to 0.7 for respiratory Virtual visits
      #step_filter(year(date)>2022) |> #only for respiratory Virtual visits
      step_normalize(all_numeric_predictors()) |>
      step_dummy(all_nominal())
    
    # specify models ---------------------------------------------------
    model_phxgb_tune <- prophet_boost(
      mode = "regression",
      growth = "linear",
      changepoint_num = 1,  # Limited number of changepoints
      prior_scale_holidays = tune(),
      seasonality_yearly = FALSE,  # Disable yearly seasonality since we have partial year
      prior_scale_changepoints = tune(),
      prior_scale_seasonality = tune(),
      # xgboost parameters
      mtry = tune(),
      min_n = tune(),
      tree_depth = tune(),
      learn_rate = tune(),
      loss_reduction = tune(),
      stop_iter = tune()
    ) |>
      set_engine("prophet_xgboost",
                 set.seed = 0112358,
                 early_stop = TRUE,
                 validation = 0.2)  # Add early stopping #
    
    # generate grid for tuning ---------------------------------------------------
    grid_phxgb_tune <- grid_space_filling(
      extract_parameter_set_dials(model_phxgb_tune) |>
        update(
          # Prophet parameters
          prior_scale_changepoints = prior_scale_changepoints(
            range = c(0.01, 0.1),  # Reduced range to limit changepoint impact
            trans = NULL
          ),
          prior_scale_seasonality = prior_scale_seasonality(
            range = c(0.1, 1.0),
            trans = NULL
          ),
          prior_scale_holidays = prior_scale_holidays(
            range = c(-1, 1),
            trans = log10_trans()
          ),
          
          # XGBoost parameters - conservative ranges
          mtry = mtry(range = c(3, 10)),
          min_n = min_n(range = c(15L, 30L)),
          tree_depth = tree_depth(range = c(3, 8)),
          learn_rate = learn_rate(range = c(0.01, 0.1)),
          loss_reduction = loss_reduction(range = c(-5, 1), trans = log10_trans()),
          stop_iter = stop_iter(range = c(10L, 50L))
        ),
      size = 100
    )
    
    # workflow for tuning ---------------------------------------------------
    wflw_phxgb_tune <- workflow() |>
      add_model(model_phxgb_tune) |>
      add_recipe(rec_obj_phxgb)
    
    # model tuning ---------------------------------------------------
    tic(quite = FALSE)
    set.seed(0112358)
    suppressWarnings({
      tune_results_phxgb <- wflw_phxgb_tune |>
        tune_grid(
          resamples = resamples_kfold,
          grid = grid_phxgb_tune,
          control = control_grid(
            verbose = TRUE,
            allow_par = TRUE,
            save_pred = TRUE,
            save_workflow = TRUE,
            parallel_over = "resamples",
            event_level = "first",
            pkgs = c("tidymodels", "modeltime", "timetk")
          ),
          metrics = metric_set(rmse, rsq)
        )
    })
toc() 

# Added
best_params <- tune_results_phxgb |> select_best(metric = "rmse")
print(best_params)
# save the results ---------------------------------------------------
#rm(df_train_test)
save.image(file = here(outp, paste0("1.3-model-tune-phxgb-final_", dataset_name,"_", encounter_type, ".RData")))
}  # End of encounter type loop
}  # End of dataset loop
