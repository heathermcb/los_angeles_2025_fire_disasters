# -------------------------------------------------------------------------------
#-------------Los Angeles Wildfires- ITS analysis------------------------------#   
#-------------------------R code-----------------------------------------------#
#-------------------------Date:2/11/25------------------------------------------#

# Code adapted from the following project:

# @project: Two-stage interrupted time series design
# @author: Arnab K. Dey (arnabxdey@gmail.com), Yiqun Ma
# @organization: Scripps Institution of Oceanography, UC San Diego
# @description: This script configures, tunes, and fits a Prophet-XGBoost model to the aggregated data
# @date: Dec 16, 2024

# Question- daily seasonality ends up being disabled- should I override this? 
# load libraries ----------------------------------------------------------------
rm(list = ls())
set.seed(0112358)
pacman::p_load(here, tidymodels, tidyverse, modeltime, timetk, tictoc)

## nina directories
#inp <- "~/Desktop/projects/casey cohort/LA-wildfires/data/raw-data/"
#outp <- "~/Desktop/projects/casey cohort/LA-wildfires/data/processed-data/"
#mod <- "~/Desktop/projects/casey cohort/LA-wildfires/data/model-output/"

# Lara directories
inp <- "/Users/larasch/Documents/UCB_postdoc/Research/los_angeles_2025_fires_rapid_response/los_angeles_2025_fire_disasters_exp/data/01_raw/"
outp <- "/Users/larasch/Documents/UCB_postdoc/Research/los_angeles_2025_fires_rapid_response/los_angeles_2025_fire_disasters_exp/Outputs/"
mod <- "/Users/larasch/Documents/UCB_postdoc/Research/los_angeles_2025_fires_rapid_response/los_angeles_2025_fire_disasters_exp/Outputs/"


# ensure consistent numeric precision ----------------------------------------------
options(digits = 7)
options(scipen = 999)

# Loop through datasets --------------------------------------------------------

# List of dataset names
datasets<-c("df_Virtual_high")
#datasets<- c("df_Virtual_high", "df_OP_high", "df_ED_high", "df_IP_high")

# # datasets <- c(
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
encounter_types <- c("num_enc")
#encounter_types <- c("num_enc", "num_enc_cardio", "num_enc_resp", "num_enc_neuro", "num_enc_injury")
# Iterate over each dataset
for (dataset_name in datasets) {
  # Iterate over each dataset
  #df_train_test <- paste0("Outputs/df-train-test_sf_", dataset_name, ".csv") #lara will toggle on 
  
  df_train_test <-  paste0(outp,"df-train-test_sf_", dataset_name, ".csv")
  
  df_train_test <- read.csv(here(df_train_test)) %>%
    mutate(date = as.Date(date))
  
  # Loop through each encounter type and create a recipe
  for (encounter_type in encounter_types) {
    
    # Subset the dataset to include only date and the current encounter type variable
    df_train_test_encounter <- df_train_test %>%
      select(date, all_of(encounter_type), pr, tmmx, tmmn, rmin, rmax, vs, srad, postjan7, time_period) %>%
      mutate(across(where(is.numeric), as.integer)) %>%
      arrange(date)
    
# split data into training and test sets -------------------------------------
set.seed(0112358)
splits <- df_train_test_encounter |>
  time_series_split(
    assess = "75 days", 
    cumulative = TRUE,
    date_var = date
  )

## resample data ----
set.seed(0112358)
resamples_kfold <- training(splits) |> 
    time_series_cv(
    assess = "30 days",     # Length of each assessment period # this was 4
   # initial = "5 years",     # Initial training period
    slice_limit = 8,        # Number of slices to create #this was 3 # we probably want this larger, maybe at least 10
    cumulative = TRUE       # Use expanding window
  )


  # recipe for modeling ---------------------------------------------------
  # Construct formula dynamically
  formula <- as.formula(paste(encounter_type, "~ ."))
  print(formula)
  
# recipe for modeling ---------------------------------------------------
rec_obj_phxgb <- recipe(formula, training(splits)) |>
    # Time series features 
    step_timeseries_signature(date) |>
   step_holiday(date, holidays = timeDate::listHolidays("US")) |>
    # Lags
    #step_lag(tmmx, lag = 1:3) |>

    #Basic seasonal components
    #step_fourier(date, period = 7, K = 2) |>

    # cleaning steps
    step_rm(matches("(.iso$)|(.xts$)")) |>
    #remove year from model since there's no variation
    #step_zv(date_year) |>
    # step_rm(matches("county")) |>
    step_normalize(matches("(index.num$)|(_year$)")) |>
    step_dummy(all_nominal())

## review the recipe
rec_obj_phxgb |> prep() |> juice() |> colnames()

# specify models ---------------------------------------------------
model_phxgb_tune <- prophet_boost(
                      mode = "regression",
                      #growth = tune(),
                      growth = "linear",
                      
                      #changepoint_range = tune(),
                     #  changepoint_num=0,
                     #  #prior_scale_holidays=tune(),
                     #  seasonality_yearly = tune(), 
                     #  prior_scale_changepoints = tune(),
                     #  prior_scale_seasonality = tune(), # this was commented out
                     #  #xgboost  
                     #  mtry = tune(),
                     #  min_n = tune(),
                     #  tree_depth = tune(),
                     #  learn_rate = tune(),
                     #  loss_reduction = tune(),
                       stop_iter = tune()
                      ) |>
                set_engine("prophet_xgboost",
                set.seed = 0112358)
                #nrounds=5) # this doesn't help error


# generate grid for tuning ---------------------------------------------------
grid_phxgb_tune <- grid_space_filling(
  extract_parameter_set_dials(model_phxgb_tune) |>
    update(
      # Prophet parameters
     # growth = growth(values = c("linear")),
  #changepoint_range = changepoint_range(range = c(0.001, 0.5), trans = NULL), # Wider range changed the range
 # seasonality_yearly = seasonality_yearly(values = c(TRUE)), 
 # prior_scale_changepoints = prior_scale_changepoints(
 #   range = c(0.01, 0.5),
 #   trans = NULL
 # ),
 #    prior_scale_seasonality = prior_scale_seasonality(
 #      range = c(0.001, 5.0),
 #      trans = NULL
 #    ),
 #    # prior_scale_holidays= prior_scale_holidays(range = c(-3, 2), trans = log10_trans()),
 #    
 #      # XGBoost parameters
 #      mtry = mtry(range = c(1, 50), trans = NULL),
 #      min_n = min_n(range = c(1L, 70L), trans = NULL),
 #      tree_depth = tree_depth(range = c(1, 70), trans = NULL),
 #      learn_rate = learn_rate(range = c(0.001, 0.7), trans = NULL),
 #      loss_reduction = loss_reduction(range = c(-50, 5), trans = log10_trans()),
       stop_iter = stop_iter(range = c(5L, 100L), trans = NULL)
    ),
  size = 100 # this was at 30 but should probably be larger 
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
    # control parameters
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
#save.image(here("Outputs", "1.3-model-tune-phxgb-final.RData"))
#save.image(file = here("Outputs", paste0("1.3-model-tune-phxgb-final_", dataset_name,"_", encounter_type, ".RData"))) # lara will toggle on
save.image(file = here(mod, paste0("1.3-model-tune-phxgb-final_", dataset_name,"_", encounter_type, ".RData")))
}  # End of encounter type loop
}  # End of dataset loop
