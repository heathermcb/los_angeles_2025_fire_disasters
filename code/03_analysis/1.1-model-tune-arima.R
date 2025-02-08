#-------------Los Angeles Wildfires- ITS analysis------------------------------#   
#-------------------------R code-----------------------------------------------#
#-------------------------Date:2/6/25------------------------------------------#

# Code adapted from the following project:

# @project: Two-stage interrupted time series design
# @author: Arnab K. Dey (arnabxdey@gmail.com), Yiqun Ma
# @organization: Scripps Institution of Oceanography, UC San Diego
# @description: This script configures, tunes, and fits an ARIMA model to the aggregated data
# @date: Dec 16, 2024

# load libraries ----------------------------------------------------------------
#rm(list = ls())
set.seed(0112358)
pacman::p_load(here, tidymodels, tidyverse, modeltime, timetk, tictoc)

# List of dataset names
# used for testing
datasets <- c("df_2022_2023_OP_high", "df_2023_2024_OP_high", "df_2024_2025_OP_high")

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
encounter_types <- c("num_enc", "num_enc_cardio", "num_enc_resp", "num_enc_neuro", "num_enc_injury")

# Loop through each dataset and load the models and process results
for (dataset_name in datasets) {
  
df_train_test <- paste0("Outputs/df-train-test_sf_", dataset_name, ".csv")
df_train_test <- read.csv(here(df_train_test)) %>%
  mutate(date = as.Date(date))

# Loop through each encounter type
for (encounter_type in encounter_types) {

# Subset the dataset to include only the date and the current encounter type variable
df_train_test_encounter <- df_train_test %>%
  select(date, encounter_type)  
  
# split data into training and test sets -------------------------------------
set.seed(0112358)
splits <- df_train_test_encounter |>
  time_series_split(
    assess = "15 days",
    cumulative = TRUE,
    date_var = date
  )
min(testing(splits))
## Question - I commented out the inital command, is that ok? 
## resample data ----
set.seed(0112358)
resamples_kfold <- training(splits) |>
  # resamples_kfold_resp <- training(splits) |> 
  # vfold_cv(v = 10, strata = date)
  # NOTE: replace function with simpler version
    time_series_cv(
    assess = "10 days",     # Length of each assessment period
    #initial = "5 days",     # Initial training period
    slice_limit = 10,        # Number of slices to create
    cumulative = TRUE       # Use expanding window
  )

# recipe for resampling ---------------------------------------------------
# Construct formula dynamically
formula <- as.formula(paste(encounter_type, "~ ."))

rec_obj_arima <- recipe(formula, training(splits)) %>%
  
  # add Fourier terms for yearly seasonality
  step_fourier(date, period = 7, K = 2) %>% # NOTE: change period to 7 for weekly, 30 for monthly, K=2?
  
  # clean up and normalize
  step_rm(matches("(.iso$)|(.xts$)")) %>%
  step_normalize(matches("(index.num$)|(_year$)")) %>%
  step_dummy(all_nominal())

### Review the recipe 
rec_obj_arima %>% prep() %>% juice() %>% colnames()

# specify models ---------------------------------------------------
model_arima_tune <- arima_reg(
        non_seasonal_ar = tune(),
        non_seasonal_ma = tune(),
        non_seasonal_differences = tune(),
        #seasonal_ar = tune(),
        #seasonal_ma = tune(),
        #seasonal_differences = tune()
        ) |>
  set_engine("auto_arima")

# generate grid for tuning ---------------------------------------------------
grid_arima_tune <- grid_space_filling(
 extract_parameter_set_dials(model_arima_tune) |>
   update(
     # Non-seasonal parameters
     non_seasonal_ar = non_seasonal_ar(range = c(1L, 3L), trans = NULL),
     non_seasonal_ma = non_seasonal_ma(range = c(1L, 3L), trans = NULL),
     non_seasonal_differences = non_seasonal_differences(range = c(0L, 2L)),
     
     # Seasonal parameters
    seasonal_ar = seasonal_ar(range = c(0L, 3L)),
    seasonal_ma = seasonal_ma(range = c(0L, 3L)),
    seasonal_differences = seasonal_differences(range = c(0L, 1L))
   ),
 size = 100  
)

# workflow for tuning ---------------------------------------------------
wflw_arima_tune <- workflow() |>
                    add_model(model_arima_tune) |>
                    add_recipe(rec_obj_arima)

# model tuning ---------------------------------------------------
tic()
set.seed(0112358)
tune_results_arima <- wflw_arima_tune |>
  tune_grid(
    resamples = resamples_kfold,
    grid = grid_arima_tune,
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
    # Add metrics
    metrics = metric_set(rmse, rsq)
  )
toc() # takes ~2 mins to run

# save the results ---------------------------------------------------
save.image(file = here("Outputs", paste0("1.1-model-tune-arima-final_", dataset_name, "_", encounter_type, ".RData"))) 

}  # End loop through encounter types
}  # End loop through datasets  
