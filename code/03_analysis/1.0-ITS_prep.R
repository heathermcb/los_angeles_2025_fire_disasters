#------------------------------------------------------------------------------#
#-------------Los Angeles Wildfires- ITS analysis------------------------------#   
#-------------------------R code-----------------------------------------------#
#-------------------------Date:2/12/25------------------------------------------#
#------------------------------------------------------------------------------#

# load packages
if (!requireNamespace('pacman', quietly = TRUE)) {install.packages('pacman')}
pacman::p_load(readr, dplyr, tidyr, purrr, lubridate, MMWRweek)

# directories
#inp <- "~/Desktop/projects/casey cohort/LA-wildfires/data/raw-data/"
#outp <- "~/Desktop/projects/casey cohort/LA-wildfires/data/processed-data/"

# Lara directories
#inp <- "/Users/larasch/Documents/UCB_postdoc/Research/los_angeles_2025_fires_rapid_response/los_angeles_2025_fire_disasters_exp/data/01_raw/"
#outp <- "/Users/larasch/Documents/UCB_postdoc/Research/los_angeles_2025_fires_rapid_response/los_angeles_2025_fire_disasters_exp/Outputs/"
#mod <- "/Users/larasch/Documents/UCB_postdoc/Research/los_angeles_2025_fires_rapid_response/los_angeles_2025_fire_disasters_exp/Outputs/"

# Server directories
inp <- "D:/Lara/los_angeles_2025_fires_rapid_response/los_angeles_2025_fire_disasters_exp/data/01_raw/"
outp <- "D:/Lara/los_angeles_2025_fires_rapid_response/los_angeles_2025_fire_disasters_exp/Outputs/"
mod <- "D:/Lara/los_angeles_2025_fires_rapid_response/los_angeles_2025_fire_disasters_exp/Outputs/"


# upload dataset
#df <- read_csv("paste0(inp, ENC_EXP_DAILY_01302025.csv")  #lara will toggle on
df <- read_csv(paste0(inp,"ENC_EXP_DAILY_02102025_updated.csv"))
resp_virus<- read_csv(paste0(inp,"wastewater_resp_illness_data/resp-virus-dat_all.csv"))

## adding respiratory viruses
resp_virus_long <- resp_virus %>%
  pivot_longer(cols = `2022-2023`:`2024-2025`, 
               names_to = "time_period", 
               values_to = "value")%>%
  pivot_wider(names_from = resp_virus, values_from = value) %>%
  mutate(year = case_when(
    week < 40 ~ as.numeric(sub(".*-(\\d{4})", "\\1", time_period)),  # Extract second part
    week >= 40 ~ as.numeric(sub("(\\d{4})-.*", "\\1", time_period))  # Extract first part
  ))


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
  summarise(across(everything(), sum, na.rm = TRUE)) %>%
  mutate(encounter_dt = mdy(encounter_dt),
         mmwr_week=MMWRweek(encounter_dt)$MMWRweek,
        year=year(encounter_dt)) # create MMWR week variable
       

# add in flu/rsv data
# Merge both datasets based on week and year
df <- df %>%
  left_join(resp_virus_long, by = c("mmwr_week" = "week", "year")) %>%
  select(-c(time_period))

# add meterological covariates
cov <- read_csv("data/01_raw/gridmet_cov_exp_level.csv") %>%
  mutate(encounter_dt = date) %>%
  select(-date)

df <- full_join(df, cov) %>%
  drop_na(enc_type)


# Ensure date column is in Date format
df <- df %>% 
  mutate(date = as.Date(encounter_dt, format = "%m/%d/%Y")) %>%
# Define time periods
  mutate(
    time_period = case_when(
      date >= ymd("2022-11-01") & date <= ymd("2023-01-31") ~ 1,
      date >= ymd("2023-11-01") & date <= ymd("2024-01-31") ~ 2,
      date >= ymd("2024-11-01") & date <= ymd("2025-01-21") ~ 3,
      TRUE ~ NA_real_
    )
  ) %>%
  drop_na(time_period)  # Remove rows outside defined time periods

# Create datasets split by encounter type and exposure level
out_enc_data <- df %>%
  select(enc_type, exp_level, encounter_dt, num_enc, 
         num_enc_cardio, num_enc_resp, num_enc_neuro, num_enc_injury,
         pr, tmmx, tmmn, rmin, rmax, vs, srad, time_period, `influenza-a`, `influenza-b`, rsv, `sars-cov2`) %>%
  group_by(enc_type, exp_level) %>%
  nest() %>%
  mutate(dataset_name = paste0("df_", enc_type, "_", exp_level))

# Convert to named list
outcome_enc_datasets <- setNames(out_enc_data$data, out_enc_data$dataset_name)

# Iterate through datasets and save as CSV
for (dataset_name in names(outcome_enc_datasets)) {
  dataset <- outcome_enc_datasets[[dataset_name]]
  
  # Create df_train_test dataset
  df_train_test <- dataset %>%
    mutate(
      date = as.Date(encounter_dt),
      month_day = format(date, "%m-%d"),
      year = year(date),
      postjan7 = ifelse(month_day < "01-07" | month_day > "01-31", 0, 1)
    ) %>%
    filter(!(month_day > "01-06" & year == 2025)) %>%
    select(num_enc, num_enc_cardio, num_enc_resp, num_enc_neuro, num_enc_injury, date,
           pr, tmmx, tmmn, rmin, rmax, vs, srad, postjan7, time_period, `influenza-a`, `influenza-b`, rsv, `sars-cov2`)
  
  write.csv(df_train_test, paste0(outp, "df-train-test_sf_", dataset_name, ".csv"), row.names = FALSE)
  
  # Create df_all_cases dataset
  df_all_cases <- dataset %>%
    mutate(
      date = as.Date(encounter_dt),
      month_day = format(date, "%m-%d"),
      year = year(date),
      postjan7 = ifelse(month_day < "01-07" | month_day > "01-31", 0, 1)
    ) %>%
   # filter(!(month_day >= "01-27" & month_day <= "02-01")) %>%
    select(num_enc, num_enc_cardio, num_enc_resp, num_enc_neuro, num_enc_injury, date,
           pr, tmmx, tmmn, rmin, rmax, vs, srad, postjan7, time_period, `influenza-a`, `influenza-b`, rsv, `sars-cov2`)
  
  write.csv(df_all_cases, paste0("Outputs/df-predict-sf_", dataset_name, ".csv"), row.names = FALSE)
}

# # Prepare time period splits
# #time_period_datasets <- list()
# outcome_enc_datasets <- list()
# 
# # for (period_name in names(time_periods)) {
# #   period <- time_periods[[period_name]]
# #   
# #   # Filter data for the specific time period
# #   period_data <- df %>%
# #     filter(date >= period$start & date <= period$end)
# #   
#   # Split by encounter type and exposure level for each time period
# out_enc_data <- df %>%
#   
#  # period_split_data <- df %>%
#     select(enc_type, exp_level, encounter_dt, num_enc, 
#            num_enc_cardio, num_enc_resp, num_enc_neuro, num_enc_injury,
#            pr, tmmx, tmmn, rmin, rmax, vs, srad) %>%
#     group_by(enc_type, exp_level) %>%
#     nest() %>%
#     mutate(dataset_name = paste0("df_", "_", enc_type, "_", exp_level))
#  # mutate(dataset_name = paste0("df_", period_name, "_", enc_type, "_", exp_level))
#   
#   # Create named list of datasets for this time period
# outcome_enc_datasets <- setNames(
#     out_enc_data %>% pull(data),
#     out_enc_data$dataset_name
#   )
# 
#   # Store in time period datasets
#   outcome_enc_datasets[[dataset_name]] <- out_enc_data
#   
#   # Create global environment datasets for this time period
#   #for (dataset_name in names(period_dataset_list)) {
#     #for (dataset_name in names(outcome_enc_datasets)) {
# for (dataset_name in datasets) {
#     #dataset <- period_dataset_list[[dataset_name]]
#     
#     # Create df_train_test dataset ----------------
#     df_train_test <- dataset %>%
#       mutate(
#         date = as.Date(encounter_dt, format = "%m/%d/%Y"),
#         month_day = format(date, "%m-%d"),
#         year = year(date)
#       ) %>%
#       arrange(date) %>%
#       mutate(jan7=
#       # Apply the filtering condition for train-test split
#      ifelse((month_day < "01-07" | month_day > "01-31"), 0, 1) ) %>%
#       filter(!(month_day >= "01-06" & year==2025)) %>%
#       select(num_enc, num_enc_cardio, num_enc_resp, num_enc_neuro, num_enc_injury, date,
#              pr, tmmx, tmmn, rmin, rmax, vs, srad)
#     
#     # Dynamically generate the file name to save the df_train_test CSV
#     output_filename_train_test <- paste0(outp,"df-train-test_sf_", dataset_name, ".csv")
#     
#     # Save the df_train_test dataset as a CSV file
#     write.csv(df_train_test, output_filename_train_test, row.names = FALSE) 
#     
#     # Create df_all_cases dataset ----------------
#     df_all_cases <- dataset %>%
#       mutate(
#         date = as.Date(encounter_dt, format = "%m/%d/%Y"),
#         month_day = format(date, "%m-%d"),
#         year = year(date)
#       ) %>%
#       arrange(date) %>%
#       filter(!(month_day >= "01-27" & month_day <= "02-01")) %>%
#       # You could uncomment the next line if you want a different filter condition
#       # filter(month_day < "01-07" | month_day > "01-31") %>%
#       select(num_enc, num_enc_cardio, num_enc_resp, num_enc_neuro, num_enc_injury, date,
#              pr, tmmx, tmmn, rmin, rmax, vs, srad)
#     
#     # Dynamically generate the file name to save the df_all_cases CSV
#     output_filename_all_cases <- paste0("Outputs/df-predict-sf_", dataset_name, ".csv") # lara will toggle on
#   #  output_filename_all_cases <- paste0(outp,"df-predict-sf_", dataset_name, ".csv") 
#     
#     # Save the df_all_cases dataset as a CSV file
#     write.csv(df_all_cases, output_filename_all_cases, row.names = FALSE)
#   }
# #}


# Clean up
rm(list = ls())