# -------------------------------------------------------------------------------
#-------------Los Angeles Wildfires- ITS analysis------------------------------#   
#-------------------------R code-----------------------------------------------#
#-------------------------Date:2/7/25------------------------------------------#

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

# ensure consistent numeric precision ----------------------------------------------
options(digits = 7)
options(scipen = 999)

# Loop through datasets --------------------------------------------------------
# List of dataset names
datasets <- c("df_2022_2023_OP_high")
#, "df_2023_2024_OP_high", "df_2024_2025_OP_high")

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

# Iterate over each dataset
for (dataset_name in datasets) {
# Iterate over each dataset
df_train_test <- paste0("Outputs/df-train-test_sf_", dataset_name, ".csv")
df_train_test <- read.csv(here(df_train_test)) %>%
  mutate(date = as.Date(date))

# Loop through each encounter type and create a recipe
for (encounter_type in encounter_types) {
  
  # Subset the dataset to include only date and the current encounter type variable
  df_train_test_encounter <- df_train_test %>%
    select(date, encounter_type)  # Dynamically select the encounter type column
  
# split data into training and test sets -------------------------------------
set.seed(0112358)
splits <- df_train_test_encounter |>
  time_series_split(
    assess = "10 days",
    cumulative = TRUE,
    date_var = date
  )

## resample data ----
set.seed(0112358)
resamples_kfold <- training(splits) |> 
    time_series_cv(
    assess = "4 days",     # Length of each assessment period # this was 4
   # initial = "5 years",     # Initial training period
    slice_limit = 3,        # Number of slices to create #this was 3 
    cumulative = TRUE       # Use expanding window
  )


  # recipe for modeling ---------------------------------------------------
  # Construct formula dynamically
  formula <- as.formula(paste(encounter_type, "~ ."))
  
# recipe for modeling ---------------------------------------------------
rec_obj_phxgb <- recipe(formula, training(splits)) |>
    # Time series features 
    step_timeseries_signature(date) |>
    # Lags
    #step_lag(pm25_diff, lag = 1:14) |>

    # Basic seasonal components
  # step_fourier(date, period = 7, K = 2) |>   
    
    # cleaning steps
    step_rm(matches("(.iso$)|(.xts$)")) |>
    #remove year from model since there's no variation
    step_zv(date_year) |>
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
                      changepoint_num=0,
                     
                      #seasonality_yearly = tune(), removed
                      # prior_scale_changepoints = tune(),
                      # prior_scale_seasonality = tune(), # this was commented out
                      #xgboost  
                      mtry = tune(),
                      min_n = tune(),
                      tree_depth = tune(),
                      learn_rate = tune(),
                      loss_reduction = tune(),
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
    #changepoint_range = changepoint_range(range = c(0.01, 0.1), trans = NULL), # Wider range changed the range
      #seasonality_yearly = seasonality_yearly(values = c(TRUE)), removed
      # prior_scale_changepoints = prior_scale_changepoints(
      #   range = c(0.01, 0.5),
      #   trans = NULL
      # ),
      # prior_scale_seasonality = prior_scale_seasonality( 
      #   range = c(0.001, 5.0),
      #   trans = NULL
      # ),

      # XGBoost parameters
      mtry = mtry(range = c(1, 50), trans = NULL),
      min_n = min_n(range = c(1L, 70L), trans = NULL),
      tree_depth = tree_depth(range = c(1, 70), trans = NULL),
      learn_rate = learn_rate(range = c(0.001, 0.7), trans = NULL),
      loss_reduction = loss_reduction(range = c(-50, 5), trans = log10_trans()),
      stop_iter = stop_iter(range = c(5L, 100L), trans = NULL)
    ),
  size = 30
)

# # Claud's code
# # Model specification
# model_phxgb_tune <- prophet_boost(
#   mode = "regression",
#   growth = "linear",
#   changepoint_num = 0,
#   
#   # XGBoost parameters to tune
#   mtry = tune(),
#   min_n = tune(),
#   tree_depth = tune(),
#   learn_rate = tune(),
#   loss_reduction = tune(),
#   stop_iter = tune(),
#   sample_size = tune(),
#   trees = tune()
# ) |>
#   set_engine("prophet_xgboost", set.seed = 0112358)
# 
# # Define much more conservative tuning grid
# grid_phxgb_tune <- grid_space_filling(
#   extract_parameter_set_dials(model_phxgb_tune) |>
#     update(
#       # Significantly reduced tree complexity
#       tree_depth = tree_depth(range = c(2, 6), trans = NULL),  # Much shallower trees
#       
#       # Increased minimum observations needed
#       min_n = min_n(range = c(10, 50), trans = NULL),  # Require more observations per leaf
#       
#       # More conservative learning parameters
#       learn_rate = learn_rate(range = c(0.001, 0.01), trans = NULL),  # Much slower learning
#       loss_reduction = loss_reduction(range = c(1, 10), trans = log10_trans()),  # More aggressive pruning
#       
#       # Sampling strategy for diversity
#       sample_size = sample_prop(range = c(0.5, 0.8)),  # More aggressive subsampling
#       mtry = mtry(range = c(2, 5), trans = NULL),  # More feature sampling
#       
#       # Fewer trees with earlier stopping
#       trees = trees(range = c(50, 500)),  # Reduced maximum trees
#       stop_iter = stop_iter(range = c(5L, 20L), trans = NULL)  # Earlier stopping
#     ),
#   size = 30  # Reduced grid size for faster tuning
# )
# 
# # Stricter control parameters
# control_params <- list(
#   early_stopping_rounds = 5,  # Stop earlier if no improvement
#   validation = 0.25  # Increased validation set
# )


#Nina's code 

# grid_phxgb_tune <- grid_space_filling(
#   extract_parameter_set_dials(model_phxgb_tune) %>%
#     update(
#       growth             = growth(values = c("linear")),
#       changepoint_range  = changepoint_range(range = c(0.5, 0.8), trans = NULL),
#       #seasonality_yearly = seasonality_yearly(values = c(TRUE, FALSE)),
#       prior_scale_changepoints = prior_scale_changepoints(range = c(0.01, 0.5)),
#       prior_scale_seasonality  = prior_scale_seasonality(range = c(0.08, 3.5)),
#       mtry               = mtry(range = c(6, 12)),
#       min_n              = min_n(range = c(2L, 15L)),
#       tree_depth         = tree_depth(range = c(7, 15)),
#       learn_rate         = learn_rate(range = c(0.1, 0.5)),
#       loss_reduction     = loss_reduction(range = c(-10, 1.5), trans = log10_trans()),
#       stop_iter          = stop_iter(range = c(5L, 15L))
#     ),
#   size = 20
# )
# workflow for tuning ---------------------------------------------------
wflw_phxgb_tune <- workflow() |>
                    add_model(model_phxgb_tune) |>
                    add_recipe(rec_obj_phxgb)

# model tuning ---------------------------------------------------
tic(quite = FALSE)
set.seed(0112358)
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
toc() 

# Added
best_params <- tune_results_phxgb |> select_best(metric = "rmse")
print(best_params)
# save the results ---------------------------------------------------
#rm(df_train_test)
#save.image(here("Outputs", "1.3-model-tune-phxgb-final.RData"))
save.image(file = here("Outputs", paste0("1.3-model-tune-phxgb-final_", dataset_name,"_", encounter_type, ".RData")))
}  # End of encounter type loop
}  # End of dataset loop
