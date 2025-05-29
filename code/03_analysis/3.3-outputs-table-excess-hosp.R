# -------------------------------------------------------------------------------
#-------------Los Angeles Wildfires- ITS analysis------------------------------#   
#-------------------------R code-----------------------------------------------#
#-------------------------Date:5/29/25------------------------------------------#

# Code adapted from the following project:

# @project: Two-stage interrupted time series design
# @author: Arnab K. Dey (arnabxdey@gmail.com), Yiqun Ma
# @organization: Scripps Institution of Oceanography, UC San Diego
# @description: This script computes the excess hospitalizations due to the wildfire and saves as a gt table
# @date: Dec 28, 2024

# load libraries ----------------------------------------------------------------
rm(list = ls())
pacman::p_load(here)

# set paths
source("paths.R")

# load data ---------------------------------------------------
# List of dataset names
datasets<- c( "df_Virtual_high")
#datasets<- c("df_Virtual_high", "df_Virtual_moderate", "df_OP_high", "df_OP_moderate") 


# List of encounter types to loop through
encounter_types <- c("num_enc")
#encounter_types <- c( "num_enc", "num_enc_resp", "num_enc_cardio",  "num_enc_neuro", "num_enc_injury")

# Initialize an empty list to store results
results_list <- list()

# Loop through each dataset and load the models and process results
for (dataset_name in datasets) {
  
  # Loop through each encounter type 
  for (encounter_type in encounter_types) {
    
    # Read the forecast data for the current dataset
    df_forecast <- readRDS(here(outp, paste0("3.2-final-preds_", dataset_name,"_", encounter_type, ".rds")))
    denoms<- read.csv(here(mod, "denoms_df.csv"))
    
    
    # Subset and process the data
    data.period <- df_forecast |>
      #mutate(month_day = format(date, "%m-%d")) |>
      #filter(date >= "2025-01-07" & date <= "2025-01-20") |> # first two weeks
      filter(date >= "2025-01-07" & date <= "2025-01-13") |> # first week
      # mutate(period = "two_weeks")
      mutate(period = "one_week")
    
    # Add a column for the dataset name
    data.period$dataset_name <- dataset_name
    
    # Add a column for the encounter type
    data.period$encounter_type <- encounter_type
    
    # Merge denoms with df_forecast based on dataset_name
    data.period <- data.period |> 
      left_join(denoms, by = "dataset_name")|>  # Merge in denominators
      mutate(denom=as.numeric(denom))
    
    # Calculate and format estimates
    data.period <- data.period |>
      group_by(period,  dataset_name, encounter_type) |> #for estimations total by week
      #group_by(date, dataset_name, encounter_type) |> # for by day
      summarise(observed = sum(num_cases),
                expected = sum(num_pred),
                expected_low = sum(conf_lo),
                expected_up = sum(conf_hi),
                denom=mean(denom)) |>
      mutate(excess = observed - expected,
             excess_low = observed - expected_up,
             excess_up = observed - expected_low,
             excess_pct = excess / observed * 100,
             excess_low_pct = (excess_low / observed) * 100,
             excess_up_pct = (excess_up / observed) * 100,
             # Calculate excess per 1000 population
             excess_per1000 = (excess / denom) * 1000,
             excess_low_per1000 = (excess_low / denom) * 1000,
             excess_up_per1000 = (excess_up / denom) * 1000 ) |>
      mutate(expected_CI = paste0(round(expected),
                                  " (",
                                  round(expected_low),
                                  ", ",
                                  round(expected_up),
                                  ")"),
             excess_CI = paste0(round(excess),
                                " (",
                                round(excess_low),
                                ", ",
                                round(excess_up),
                                ")"),
             excess_pct_CI = paste0(round(excess_pct, 1),
                                    " (",
                                    round(excess_low_pct, 1),
                                    ", ",
                                    round(excess_up_pct, 1),
                                    ")"),
             excess_per1000_CI = paste0(round(excess_per1000, 4),
                                        " (", round(excess_low_per1000, 4), ", ", round(excess_up_per1000, 4), ")"))  |>
      # dplyr::select(date, observed, expected_CI, excess_CI, excess_pct_CI, excess_per1000_CI, dataset_name, encounter_type)
      dplyr::select(period, observed, expected_CI, excess_CI, excess_pct_CI, excess_per1000_CI, dataset_name, encounter_type)
    
    
    
    
    # Store the processed data for this dataset in the results list
    results_list[[paste0(dataset_name, "_", encounter_type)]] <- data.period
    
  }  # End of encounter type loop
}  # End of dataset loop

# Combine the results from all datasets into one data frame
final_results <- bind_rows(results_list)

# Print the combined results
print(final_results)

# For exporting results by day
# final_results_filename <- paste0(outp, "results_by_day.csv")
# write.csv(final_results, here(final_results_filename))

# For exporting total results for 1 week
final_results_filename <- paste0(outp, "results_week1.csv")
write.csv(final_results, here(final_results_filename))