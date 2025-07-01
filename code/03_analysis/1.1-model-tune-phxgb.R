# -----------------------------------------------------------------------------#
#-------------Los Angeles Wildfires- ITS analysis------------------------------#   
#-------------------------R code-----------------------------------------------#
#-----------------Last update:6/27/25------------------------------------------#

# Code adapted from the following project:

# @project: Two-stage interrupted time series design
# @author: Arnab K. Dey, Yiqun Ma
# @organization: Scripps Institution of Oceanography, UC San Diego
# @description: This script configures, tunes, and fits a Prophet-XGBoost model to the aggregated data
# @date: Dec 16, 2024

# load libraries ----------------------------------------------------------------
rm(list = ls())
set.seed(0112358)
pacman::p_load(here, tidymodels, tidyverse, modeltime, timetk, tictoc, here)

# set paths
source("paths.R")

# ensure consistent numeric precision ----------------------------------------------
options(digits = 7)
options(scipen = 999)

# Loop through datasets --------------------------------------------------------

# List of dataset names
#datasets<- c( "df_Virtual_high", "df_Virtual_moderate", "df_Virtual_least")
datasets<- c("df_Virtual_high", "df_Virtual_moderate", "df_OP_high", "df_OP_moderate") 


# List of encounter types to loop through
#encounter_types <- c("num_enc_resp")
encounter_types <- c( "num_enc", "num_enc_resp", "num_enc_cardio",  "num_enc_neuro", "num_enc_injury")

# Iterate over each dataset
for (dataset_name in datasets) {
  # Iterate over each dataset
  # Loop through each encounter type and create a recipe
  for (encounter_type in encounter_types) {
    
    df_train_test <- here(mod, paste0("df-train-test_sf_", dataset_name, ".csv"))
    
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
       filter(date>= "2023-01-01") %>% # for respiratory Virtual only
      arrange(date)
    
    ## split data into training and test sets -------------------------------------
    set.seed(0112358)
    splits <- df_train_test_encounter |>
      time_series_split(
        #assess = "57 days", # for Virtual/Resp 
        assess = "75 days", 
        cumulative = TRUE,
        #skip = "57 days",      ## for Virtual/Resp- same as the assess period to avoid overlapping
        skip = "75 days",    # same as the assess period to avoid overlapping
        date_var = date
      )
    
    ## resample data ----
    set.seed(0112358)
    resamples_kfold <- training(splits) |> 
      time_series_cv(
        #assess = "25 days",     # for Virtual/Resp 
        assess = "40 days",     
        slice_limit = 8,        
        cumulative = TRUE       
      )
    
    ## recipe for modeling ---------------------------------------------------
    formula <- as.formula(paste(encounter_type, "~ ."))
    print(formula)
    
    rec_obj_phxgb <- recipe(formula, training(splits)) |>
      # Time series features 
      step_holiday(date, holidays = timeDate::listHolidays("US")) |>
      step_rm(matches("date_USInaugurationDay", "date_USCPulaskisBirthday", "date_USJuneteenthNationalIndependenceDay",
                      "date_USDecorationMemorialDay")) |> # Remove irrelevant holiday data
      # Minimal seasonal components
      step_mutate(
        month=factor(month(date)),
        year=factor(year(date)),
        is_weekend = factor(if_else(wday(date, week_start = 1) %in% c(6, 7), "weekend", "weekday")),
        day_of_week = factor(wday(date, label = TRUE)),
        month_day = format(date, "%m-%d"),  # Create month_day in "MM-DD" format
        business_closed_holiday = factor(if_else(
          month_day %in% c(
            "01-01",  # New Year's Day
            "01-20",  # Martin Luther King Jr. Day
            "07-04",  # Independence Day
            "12-25"   # Christmas Day
          ), "business_closed", "business_open"
        ))
      ) |>
      step_rm(month_day) |>
      step_novel(all_nominal()) |> 
      # cleaning steps
      step_rm(matches("(.iso$)|(.xts$)|(.minute)|(.second)|(.hour)|(.am.pm)")) |>
      step_zv() |>
      step_normalize(all_numeric_predictors())
    
    rec_obj_phxgb |> prep() |> juice() |> colnames()
    
    ## specify models ---------------------------------------------------
    model_phxgb_tune <- prophet_boost(
      mode = "regression",
      growth = "linear",
      seasonality_yearly = FALSE,  # Disable yearly seasonality since we have partial year
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
                 validation = 0.2)
                #validation = 0.3) # for Virtual resp
    
    # generate grid for tuning ---------------------------------------------------
    
    grid_phxgb_tune <- grid_space_filling(
      extract_parameter_set_dials(model_phxgb_tune) |>
        update(
          # XGBoost parameters 
          mtry = mtry(range = c(3, 10)),
          #mtry = mtry(range = c(2, 6)), #resp Virtual only
          
          min_n = min_n(range = c(15L, 30L)),
          tree_depth = tree_depth(range = c(3, 8)),
          learn_rate = learn_rate(range = c(0.01, 0.1)),
          #learn_rate = learn_rate(range = c(0.001, 0.1)), #resp Virtual only
          
          loss_reduction = loss_reduction(range = c(-5, 1), trans = log10_trans()),
          stop_iter = stop_iter(range = c(10L, 50L))
        ),
      size = 100
    )
    
    ## workflow for tuning ---------------------------------------------------
    wflw_phxgb_tune <- workflow() |>
      add_model(model_phxgb_tune) |>
      add_recipe(rec_obj_phxgb)
    
    ## model tuning ---------------------------------------------------
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
    
    # Look at parameters of model
    best_params <- tune_results_phxgb |> select_best(metric = "rmse")
    print(best_params)
    
    # save the results ---------------------------------------------------
    save.image(file = here(outp, paste0("1.1-model-tune-phxgb-final_", dataset_name,"_", encounter_type, ".RData")))
  }  # End of encounter type loop
}  # End of dataset loop
