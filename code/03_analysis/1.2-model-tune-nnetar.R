#-------------Los Angeles Wildfires- ITS analysis------------------------------#   
#-------------------------R code-----------------------------------------------#
#-------------------------Date:2/10/25------------------------------------------#

# Code adapted from the following project:

# @project: Two-stage interrupted time series design
# @author: Arnab K. Dey (arnabxdey@gmail.com), Yiqun Ma
# @organization: Scripps Institution of Oceanography, UC San Diego
# @description: This script configures, tunes, and fits a NNTEAR model to the aggregated data
# @date: Dec 16, 2024

# load libraries ----------------------------------------------------------------
rm(list = ls())
set.seed(0112358)
pacman::p_load(here, tidymodels, tidyverse, modeltime, timetk, tictoc)

# Lara directories
inp <- "/Users/larasch/Documents/UCB_postdoc/Research/los_angeles_2025_fires_rapid_response/los_angeles_2025_fire_disasters_exp/data/01_raw/"
outp <- "/Users/larasch/Documents/UCB_postdoc/Research/los_angeles_2025_fires_rapid_response/los_angeles_2025_fire_disasters_exp/Outputs/"
mod <- "/Users/larasch/Documents/UCB_postdoc/Research/los_angeles_2025_fires_rapid_response/los_angeles_2025_fire_disasters_exp/Outputs/"

# List of dataset names
# used for testing
datasets <- c("df_2022_2023_OP_high", "df_2023_2024_OP_high", "df_2024_2025_OP_high", "df_2022_2023_Virtual_high",
              "df_2023_2024_Virtual_high",   "df_2024_2025_Virtual_high")
# datasets <- c("df_2022_2023_OP_high")
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
# encounter_types <- c("num_enc")
encounter_types <- c("num_enc", "num_enc_cardio", "num_enc_resp", "num_enc_neuro", "num_enc_injury")
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
      select(date, all_of(encounter_type), pr, tmmx, tmmn, rmin, rmax, vs, srad) %>%
      mutate(across(all_of(encounter_type), as.numeric))
    
# split data into training and test sets -------------------------------------
set.seed(0112358)
splits <- df_train_test |>
  time_series_split(
    assess = "15 days",
    cumulative = TRUE,
    date_var = date
  )

min(training(splits)$date)
max(training(splits)$date)
min(testing(splits)$date)
max(testing(splits)$date)

## resample data ----
set.seed(0112358)
resamples_kfold <- training(splits) |> 
  time_series_cv(
    assess = "15 days",     # Length of each assessment period
    #initial = "5 years",     # Initial training period
    slice_limit = 3,        # Number of slices to create
    cumulative = TRUE       # Use expanding window
  )


 
# recipe for modeling ---------------------------------------------------
  # Construct formula dynamically
  formula <- as.formula(paste(encounter_type, "~ ."))
  
  
  rec_obj_nnetar <- recipe(formula, training(splits)) |>
    step_timeseries_signature(date) |>
    step_zv(date_year) |>
    # cleaning steps
    step_rm(matches("(.iso$)|(.xts$)")) |>
    # step_rm(matches("county")) |>
    step_normalize(matches("(index.num$)|(_year$)")) |>
    step_dummy(all_nominal())

### Review the recipe ----
rec_obj_nnetar |> prep() |> juice() |> colnames()

# specify models ---------------------------------------------------
model_nnetar_tune <- nnetar_reg(
      non_seasonal_ar = tune(),
      seasonal_ar   = tune(),
      hidden_units = tune(),
      num_networks = tune(),
      penalty = tune(),
      epochs = tune()
  ) |>
    set_engine("nnetar",
      set.seed = 0112358)


# generate grid for tuning ---------------------------------------------------
grid_nnetar_tune <- grid_space_filling(
  extract_parameter_set_dials(model_nnetar_tune) |>
    update(
      hidden_units = hidden_units(range = c(8, 20), trans = NULL),
      num_networks = num_networks(range = c(40, 100), trans = NULL),
      penalty = penalty(range = c(0.01, 0.1), trans = NULL), 

      # Time series parameters
      seasonal_ar = seasonal_ar(range = c(1, 4), trans = NULL),
      non_seasonal_ar = non_seasonal_ar(range = c(1, 8), trans = NULL),
      epochs = epochs(range = c(50L, 200L), trans = NULL)
    ),
  size = 75
)

# workflow for tuning ---------------------------------------------------
wflw_nnetar_tune <- workflow() |>
                    add_model(model_nnetar_tune) |>
                    add_recipe(rec_obj_nnetar)

# model tuning ---------------------------------------------------
tic()
set.seed(0112358)
tune_results_nnetar <- wflw_nnetar_tune |>
  tune_grid(
    resamples = resamples_kfold,
    grid = grid_nnetar_tune,
    # control parameters
    control = control_grid(
      verbose = TRUE,
      allow_par = TRUE,
      save_pred = TRUE,
      save_workflow = TRUE,
      parallel_over = "resamples",
      event_level = "first",
      pkgs = c("modeltime", "timetk", "tidymodels")
    ),
    # Add metrics
    metrics = metric_set(rmse, rsq)
  )
toc() # takes ~ 20 mins to run
# Added
best_params <- tune_results_nnetar |> select_best(metric = "rmse")
print(best_params)

# Save the results ---------------------------------------------------
#save.image(file = here("Outputs", paste0("1.2-model-tune-nnetar-final_", dataset_name, "_", encounter_type, ".RData")))
save.image(file = here(mod, paste0("1.2-model-tune-nnetar-final_", dataset_name,"_", encounter_type, ".RData")))

}  # End of encounter type loop
}  # End of dataset loop
