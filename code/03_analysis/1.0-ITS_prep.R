#------------------------------------------------------------------------------#
#-------------Los Angeles Wildfires- ITS analysis------------------------------#   
#-------------------------R code-----------------------------------------------#
#-----------------Last update:6/27/25------------------------------------------#

## Purpose: Prepping data for ITS analysis
#---------------Load packages -------------------------------------------------#
# load packages
if (!requireNamespace('pacman', quietly = TRUE)) {install.packages('pacman')}
pacman::p_load(readr, dplyr, tidyr, purrr, lubridate, MMWRweek, here)

# set paths
source("paths.R")

#---------------------- Data upload and prep-----------------------------------#
# upload dataset
df <- read_csv(here(inp,"ENC_EXP_DAILY_02232025.csv"))
resp_virus<- read_csv(here(inp,"wastewater_resp_illness_data/resp-virus-dat_all.csv"))

# add meterological covariates
cov <- read_csv(here(inp, "gridmet_cov_exp_level.csv")) %>%
  mutate(encounter_dt = date) %>%
  select(-date)

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

#---------------------- Merging in Covariates----------------------------------#

## Restructuring datasets to be merged in
resp_virus_long <- resp_virus %>%
  pivot_longer(cols = `2022-2023`:`2024-2025`, 
               names_to = "time_period", 
               values_to = "value")%>%
  pivot_wider(names_from = resp_virus, values_from = value) %>%
  mutate(year = case_when(
    week < 40 ~ as.numeric(sub(".*-(\\d{4})", "\\1", time_period)),  # Extract second part
    week >= 40 ~ as.numeric(sub("(\\d{4})-.*", "\\1", time_period))  # Extract first part
  ))


df <- df %>%
  select(-exp_pov) %>%  # Remove the 'exp_pov' variable
  group_by(exp_level, enc_type, encounter_dt) %>%
  summarise(across(everything(), sum, na.rm = TRUE)) %>%
  mutate(encounter_dt = mdy(encounter_dt),
         mmwr_week=MMWRweek(encounter_dt)$MMWRweek,
         year=year(encounter_dt)) # create MMWR week variable

# add in flu/rsv data- merge both datasets based on week and year
df <- df %>%
  left_join(resp_virus_long, by = c("mmwr_week" = "week", "year")) %>%
  select(-c(time_period))

# Merge in environmental covaraites
df <- left_join(df, cov) 

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
         pr, tmmx, tmmn, rmin, rmax, vs, srad, time_period, `influenza-a`, `influenza-b`, rsv, `sars-cov2`, denom) %>%
  group_by(enc_type, exp_level) %>%
  nest() %>%
  mutate(dataset_name = paste0("df_", enc_type, "_", exp_level),
         denom = map_dbl(data, ~mean(.x$denom, na.rm = TRUE)),  # Compute average- is same across groups
         data = map2(data, dataset_name, ~mutate(.x, dataset_name = .y)))  # Add dataset_name column inside each dataset)

# Convert to named list
outcome_enc_datasets <- setNames(out_enc_data$data, out_enc_data$dataset_name)

#--------------- create csvs of data for analysis----------------------------#

# Iterate through datasets and save as CSV
for (dataset_name in names(outcome_enc_datasets)) {
  dataset <- outcome_enc_datasets[[dataset_name]]
  
  # Create df_train_test dataset
  df_train_test <- dataset %>%
    mutate(
      date = as.Date(encounter_dt),
      month_day = format(date, "%m-%d"),
      year = year(date),
      postjan7 = ifelse(month_day < "01-07" | month_day > "01-21", 0, 1)
    ) %>%
    filter(!(month_day > "01-06" & year == 2025)) %>%
    select(num_enc, num_enc_cardio, num_enc_resp, num_enc_neuro, num_enc_injury, date,
           pr, tmmx, tmmn, rmin, rmax, vs, srad, postjan7, time_period, `influenza-a`, `influenza-b`, rsv, `sars-cov2`)
  
  write.csv(df_train_test, here(mod, paste0( "df-train-test_sf_", dataset_name, ".csv")), row.names = FALSE)
  
  # Create df_all_cases dataset
  df_all_cases <- dataset %>%
    mutate(
      date = as.Date(encounter_dt),
      month_day = format(date, "%m-%d"),
      year = year(date),
      postjan7 = ifelse(month_day < "01-07" | month_day > "01-21", 0, 1)
    ) %>%
    select(num_enc, num_enc_cardio, num_enc_resp, num_enc_neuro, num_enc_injury, date,
           pr, tmmx, tmmn, rmin, rmax, vs, srad, postjan7, time_period, `influenza-a`, `influenza-b`, rsv, `sars-cov2`)
  
  write.csv(df_all_cases, here(mod, paste0("df-predict-sf_", dataset_name, ".csv")), row.names = FALSE)
}

#--------------- create denominator data for analysis----------------------------#
# create dataset with denominator to merge in

# Create a separate dataset with dataset names and denom
denom_df <- out_enc_data %>%
  select(dataset_name, denom)

# Print or save denom_df
print(denom_df)

write.csv(denom_df, here(mod, "denoms_df.csv"))

# Clean up
rm(list = ls())