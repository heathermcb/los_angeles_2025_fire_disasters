# -------------------------------------------------------------------------------
#-------------Los Angeles Wildfires- ITS analysis------------------------------#   
#-------------------------R code-----------------------------------------------#
#-------------------------Date:2/20/25------------------------------------------#

# Code adapted from the following project:

# @project: Two-stage interrupted time series design
# @author: Arnab K. Dey (arnabxdey@gmail.com), Yiqun Ma
# @organization: Scripps Institution of Oceanography, UC San Diego
# @description: This script computes the excess hospitalizations due to the wildfire and saves as a gt table
# @date: Dec 28, 2024

# load libraries ----------------------------------------------------------------
rm(list = ls())
pacman::p_load(here)

# load data ---------------------------------------------------
# List of dataset names
datasets<- c("df_OP_high")
#datasets<- c("df_Virtual_high", "df_OP_high", "df_OP_moderate", "df_Virtual_moderate") #

# List of encounter types to loop through
encounter_types <- c("num_enc_neuro") #test
#encounter_types <- c( "num_enc", "num_enc_resp", "num_enc_cardio",  "num_enc_neuro", "num_enc_injury")


# Server directories
inp <- "D:/Lara/los_angeles_2025_fires_rapid_response/los_angeles_2025_fire_disasters_exp/data/01_raw/"
mod <- "D:/Lara/los_angeles_2025_fires_rapid_response/los_angeles_2025_fire_disasters_exp/Outputs/"
#outp <- "D:/Lara/los_angeles_2025_fires_rapid_response/los_angeles_2025_fire_disasters_exp/Outputs/final_results/"
outp <- "D:/Lara/los_angeles_2025_fires_rapid_response/los_angeles_2025_fire_disasters_exp/Outputs/testing/"

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
    filter(date >= "2025-01-07" & date <= "2025-01-20") |> # first two weeks
   # filter(date >= "2025-01-07" & date <= "2025-01-13") |> # first week
    #dplyr::select(-date) |>
    mutate(period = "two_weeks")
   # mutate(period = "one_week")
  
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
    #group_by(period,  dataset_name, encounter_type) |> #for total
    group_by(date, dataset_name, encounter_type) |> # for by day
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
                                      " (", round(excess_low_per1000, 4), ", ", round(excess_up_per1000, 4), ")"))  ##|>
  #  dplyr::select(date, observed, expected_CI, excess_CI, excess_pct_CI, excess_per1000_CI, dataset_name, encounter_type)
 # dplyr::select(period, observed, expected_CI, excess_CI, excess_pct_CI, excess_per1000_CI, dataset_name, encounter_type)
  
  
  
  
  # Store the processed data for this dataset in the results list
  results_list[[paste0(dataset_name, "_", encounter_type)]] <- data.period
  #results_list[] <- data.period
  }  # End of encounter type loop
}  # End of dataset loop


# Combine the results from all datasets into one data frame
final_results <- bind_rows(results_list)

# Print the combined results
print(final_results)
 final_results_filename <- paste0(outp, "results_OP_high_neuro_feb25.csv")
 write.csv(final_results, here(final_results_filename))
#
#  final_results_filename <- paste0(outp, "results_resp_by_day_feb22.csv")
#  write.csv(final_results, here(final_results_filename))

# final_results_filename <- paste0(outp, "results_resp_week1_feb22.csv")
# write.csv(final_results, here(final_results_filename))
# 
# final_results_filename <- paste0(outp, "results_resp_week1+2_feb22.csv")
# write.csv(final_results, here(final_results_filename))
 # 
# final_results_filename <- paste0(outp, "results_by_day_feb20.csv")
# write.csv(final_results, here(final_results_filename))

# final_results_filename <- paste0(outp, "results_week1_feb20.csv")
# write.csv(final_results, here(final_results_filename))
# 
# final_results_filename <- paste0(outp, "results_week1+2_feb20.csv")
# write.csv(final_results, here(final_results_filename))

#Figures

# Ensure datasets with similar names are next to each other
final_results <- final_results %>%
  mutate(dataset_name = fct_relevel(dataset_name, 
                                  #  "df_Virtual_high", "df_Virtual_moderate", "df_OP_high", "df_OP_moderate"))
                                     "df_Virtual_high", "df_Virtual_moderate", "df_Virtual_least"))
#Add exposure level based on dataset name
final_results <- final_results %>%
  mutate(exposure_level = case_when(
    grepl("high", dataset_name, ignore.case = TRUE) ~ "Highly Exposed",
    grepl("moderate", dataset_name, ignore.case = TRUE) ~ "Moderately Exposed",
    grepl("least", dataset_name, ignore.case = TRUE) ~ "Least Exposed",
    TRUE ~ "Other"
  ))

# Split data into two subsets
df_OP_results <- final_results %>% filter(grepl("df_OP", dataset_name))
df_Virtual_results <- final_results %>% filter(grepl("df_Virtual", dataset_name))
# Define custom reds & oranges color palette
muted_rainbow <- c("#D55E00", "#E69F00",  "#009E73", "#56B4E9", "#0072B2", "#CC79A7")

df_Virtual_results$exposure_level <- factor(df_Virtual_results$exposure_level, levels = c("Highly Exposed", "Moderately Exposed", "Least Exposed")) 

# Function to create forest plots with custom exposure labels
plot_forest <- function(data) {
  # Define custom labels for encounter types
  encounter_type_labels <- c(
    "num_enc" = "Total Visits",
    "num_enc_resp" = "Respiratory Visits"
  )
  ggplot(data, aes(y = exposure_level, x = excess_pct, color = encounter_type)) +
    
 # ggplot(data, aes(y = exposure_level, x = excess_per1000, color = encounter_type)) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "black", size = 0.8) +  # Reference line at 0
    geom_point(size = 3, position = position_dodge(width = 0.5)) +  # Main points
    #geom_errorbar(aes(xmin = excess_low_per1000, xmax = excess_up_per1000), width = 0.2, position = position_dodge(width = 0.5)) +  # Error bars
    geom_errorbar(aes(xmin = excess_low_pct, xmax = excess_up_pct), width = 0.2, position = position_dodge(width = 0.5)) +  # Error bars
    
     facet_wrap(~ encounter_type, scales = "free_x", labeller = labeller(encounter_type = encounter_type_labels)) +  # Rename facets
    scale_color_manual(values = muted_rainbow, labels = encounter_type_labels) +  # Rename legend labels
    theme_minimal() +
    #labs(x = "Excess Cases (per 1000)",
         labs(x = "Excess Cases (%)",
              
         y = "Exposure Level",
         color = "Encounter Type") +  # Keep color legend but rename it
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          plot.title = element_blank(),  # Remove title
          legend.position = "none")  # Remove legend)  # Remove title
}

# Generate plots without titles
plot_OP <- plot_forest(df_OP_results)
plot_Virtual <- plot_forest(df_Virtual_results)