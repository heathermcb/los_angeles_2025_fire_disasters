#------------------------------------------------------------------------------#
#-------------Los Angeles Wildfires- ITS analysis------------------------------#   
#-------------------------R code-----------------------------------------------#
#-------------------------Date:2/6/25------------------------------------------#
#-------------------------Lara Schwarz-----------------------------------------#
#------------------------------------------------------------------------------#

# load packages
if (!requireNamespace('pacman', quietly = TRUE)) {install.packages('pacman')}
pacman::p_load(readr, dplyr, tidyr, purrr, lubridate)

# directories
#inp <- "~/Desktop/projects/casey cohort/LA-wildfires/data/raw-data/"
#outp <- "~/Desktop/projects/casey cohort/LA-wildfires/data/processed-data/"


# upload dataset
df <- read_csv("data/01_raw/ENC_EXP_DAILY_01302025.csv") lara will toggle on
#df <- read_csv(paste0(inp,"ENC_EXP_DAILY_01302025.csv"))

# Define exposure levels based on 'exp_pov'
df <- df %>%
  mutate(
    exp_level = case_when(
      exp_pov %in% c(0, 1) ~ "least",
      exp_pov %in% c(2, 3) ~ "moderate",
      exp_pov %in% c(4, 5) ~ "high",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(exp_level))  # Remove rows with NA (which corresponds to "Other")

df <- df %>%
  select(-exp_pov) %>%  # Remove the 'exp_pov' variable
  group_by(exp_level, enc_type, encounter_dt) %>%
  summarise(across(everything(), sum, na.rm = TRUE))

df <- df %>%
  mutate(encounter_dt = mdy(encounter_dt))

# add meterological covariates
cov <- read_csv(paste0(outp,"gridmet_cov_exp_level.csv")) %>%
  mutate(encounter_dt = date) %>%
  select(-date)

df <- full_join(df, cov) %>%
  drop_na(enc_type)

# Define time periods
time_periods <- list(
  "2022_2023" = list(start = ymd("2022-11-01"), end = ymd("2023-01-21")),
  "2023_2024" = list(start = ymd("2023-11-01"), end = ymd("2024-01-21")),
  "2024_2025" = list(start = ymd("2024-11-01"), end = ymd("2025-01-21"))
)

# Ensure date column is in Date format
df <- df %>% 
  mutate(date = as.Date(encounter_dt, format = "%m/%d/%Y"))

# Prepare time period splits
time_period_datasets <- list()

for (period_name in names(time_periods)) {
  period <- time_periods[[period_name]]
  
  # Filter data for the specific time period
  period_data <- df %>%
    filter(date >= period$start & date <= period$end)
  
  # Split by encounter type and exposure level for each time period
  period_split_data <- period_data %>%
    select(enc_type, exp_level, encounter_dt, num_enc, 
           num_enc_cardio, num_enc_resp, num_enc_neuro, num_enc_injury,
           pr, tmmx, tmmn, rmin, rmax, vs, srad) %>%
    group_by(enc_type, exp_level) %>%
    nest() %>%
    mutate(dataset_name = paste0("df_", period_name, "_", enc_type, "_", exp_level))
  
  # Create named list of datasets for this time period
  period_dataset_list <- setNames(
    period_split_data %>% pull(data),
    period_split_data$dataset_name
  )
  
  # Store in time period datasets
  time_period_datasets[[period_name]] <- period_dataset_list
  
  # Create global environment datasets for this time period
  for (dataset_name in names(period_dataset_list)) {
    dataset <- period_dataset_list[[dataset_name]]
    
    # Create df_train_test dataset ----------------
    df_train_test <- dataset %>%
      mutate(
        date = as.Date(encounter_dt, format = "%m/%d/%Y"),
        month_day = format(date, "%m-%d"),
        year = year(date)
      ) %>%
      arrange(date) %>%
      # Apply the filtering condition for train-test split
      filter(month_day < "01-07" | month_day > "01-31") %>%
      select(num_enc, num_enc_cardio, num_enc_resp, num_enc_neuro, num_enc_injury, date,
             pr, tmmx, tmmn, rmin, rmax, vs, srad)
    
    # Dynamically generate the file name to save the df_train_test CSV
 #   output_filename_train_test <- paste0("Outputs/df-train-test_sf_", dataset_name, ".csv") # lara will toggle on
    output_filename_train_test <- paste0(outp,"df-train-test_sf_", dataset_name, ".csv")
    
    # Save the df_train_test dataset as a CSV file
    write.csv(df_train_test, output_filename_train_test, row.names = FALSE) 
    
    # Create df_all_cases dataset ----------------
    df_all_cases <- dataset %>%
      mutate(
        date = as.Date(encounter_dt, format = "%m/%d/%Y"),
        month_day = format(date, "%m-%d"),
        year = year(date)
      ) %>%
      arrange(date) %>%
      filter(!(month_day >= "01-27" & month_day <= "02-01")) %>%
      # You could uncomment the next line if you want a different filter condition
      # filter(month_day < "01-07" | month_day > "01-31") %>%
      select(num_enc, num_enc_cardio, num_enc_resp, num_enc_neuro, num_enc_injury, date,
             pr, tmmx, tmmn, rmin, rmax, vs, srad)
    
    # Dynamically generate the file name to save the df_all_cases CSV
    output_filename_all_cases <- paste0("Outputs/df-predict-sf_", dataset_name, ".csv") # lara will toggle on
  #  output_filename_all_cases <- paste0(outp,"df-predict-sf_", dataset_name, ".csv") 
    
    # Save the df_all_cases dataset as a CSV file
    write.csv(df_all_cases, output_filename_all_cases, row.names = FALSE)
  }
}


# Clean up
rm(list = ls())