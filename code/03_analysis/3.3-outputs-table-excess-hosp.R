# -------------------------------------------------------------------------------
#-------------Los Angeles Wildfires- ITS analysis------------------------------#   
#-------------------------R code-----------------------------------------------#
#-------------------------Date:2/7/25------------------------------------------#

# Code adapted from the following project:

# @project: Two-stage interrupted time series design
# @author: Arnab K. Dey (arnabxdey@gmail.com), Yiqun Ma
# @organization: Scripps Institution of Oceanography, UC San Diego
# @description: This script computes the excess hospitalizations due to the wildfire and saves as a gt table
# @date: Dec 28, 2024

# load libraries ----------------------------------------------------------------
rm(list = ls())
pacman::p_load(here)

# List of datasets to iterate over
# used for testing
#datasets <- c("df_2022_2023_ED_high")

datasets <- c(
  "df_2022_2023_ED_high",
  "df_2023_2024_ED_high",
  "df_2024_2025_ED_high"  ,
 # "df_2022_2023_OP_high",
 #"df_2023_2024_OP_high",
  #"df_2024_2025_OP_high",
   "df_2022_2023_IP_high",
   "df_2023_2024_IP_high",
   "df_2024_2025_IP_high" #,
#   "df_2022_2023_Virtual_high",
#   "df_2023_2024_Virtual_high",
 #  "df_2024_2025_Virtual_high"
)

# Initialize an empty list to store results
results_list <- list()

# Loop through each dataset
for (dataset_name in datasets) {
  # Loop through each encounter type 
  for (encounter_type in encounter_types) {
  
  # Read the forecast data for the current dataset
  df_forecast <- readRDS(here("Outputs", paste0("3.2-final-preds_", dataset_name, ".rds")))
  
  # Extract the .model_desc from the best model
  # Check if the value exists and is not NA or empty before assigning it
  # Check if 'df_forecast$model_desc' exists as a column in the data frame
  if ("model_desc" %in% names(df_forecast) && !is.na(df_forecast$model_desc[1])) {
    best_model_desc <- df_forecast$model_desc[1]
  } else {
    best_model_desc <- NA  # Or some fallback value
  }
  
  # Subset and process the data
  data.period <- df_forecast |>
    mutate(month_day = format(date, "%m-%d")) |>
    filter(month_day >= "01-07" & month_day <= "01-26") |>
    dplyr::select(-date) |>
    mutate(period = "main event")
  
  # Calculate and format
  data.period <- data.period |>
    group_by(period) |>
    summarise(observed = sum(num_enc_neuro_actual),
              expected = sum(num_enc_neuro_pred),
              expected_low = sum(conf_lo),
              expected_up = sum(conf_hi)) |>
    mutate(excess = observed - expected,
           excess_low = observed - expected_up,
           excess_up = observed - expected_low,
           excess_pct = excess / observed * 100,
           excess_low_pct = excess_low / observed * 100,
           excess_up_pct = excess_up / observed * 100) |>
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
                                  ")")) |>
    dplyr::select(period, observed, expected_CI, excess_CI, excess_pct_CI)
  
  # Add the .model_desc to the forecast data
  data.period$model_desc <- ifelse(exists("best_model_desc"), best_model_desc, NA)
  
  # Add a column for the dataset name
  data.period$dataset_name <- dataset_name
  
  # Store the processed data for this dataset in the results list
  results_list[[dataset_name]] <- data.period
}

# Combine the results from all datasets into one data frame
final_results <- bind_rows(results_list)

# Print the combined results
print(final_results)

# Clean the data by removing the non-numeric parts of the 'expected_CI' column and convert it to numeric
final_results$expected_numeric <- as.numeric(sub(" .*", "", final_results$expected_CI))

# Create a new column to distinguish between 'IP' and 'ED'
final_results$dataset_type <- ifelse(grepl("IP", final_results$dataset_name), "IP", "ED")

# Separate the data by dataset type (IP and ED)
data_ip <- final_results %>% filter(dataset_type == "IP")
data_ed <- final_results %>% filter(dataset_type == "ED")

# Create a boxplot of observed vs expected values, separating by dataset_type (IP vs ED)
# Create a bar plot for "IP"

p_ip <- ggplot(data_ip, aes(x = dataset_name)) +
  geom_bar(aes(y = observed, fill = "Observed"), stat = "identity", position = position_dodge(width = 0.5), width = 0.3, alpha = 0.7) +
  geom_bar(aes(y = expected_numeric, fill = "Expected"), stat = "identity", position = position_dodge(width = 0.5), width = 0.3, alpha = 0.7) +
  labs(
    title = "Observed and Expected Values for IP",
    x = "Dataset Name",
    y = "Values"
  ) +
  scale_fill_manual(values = c("Observed" = "blue", "Expected" = "red")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Adjust text angle for readability

# Create a bar plot for "ED"
p_ed <- ggplot(data_ed, aes(x = dataset_name)) +
  geom_bar(aes(y = observed, fill = "Observed"), stat = "identity", position = "dodge", width = 0.3, alpha = 0.7) +
  geom_bar(aes(y = expected_numeric, fill = "Expected"), stat = "identity", position = "dodge", width = 0.3, alpha = 0.7) +
  labs(
    title = "Observed and Expected Values for ED",
    x = "Dataset",
    y = "Values"
  ) +
  scale_fill_manual(values = c("Observed" = "blue", "Expected" = "red")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Adjust text angle for readability


# Print the plots
print(p_ip)
print(p_ed) }