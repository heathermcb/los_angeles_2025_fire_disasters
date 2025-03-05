# -------------------------------------------------------------------------------
#-------------Los Angeles Wildfires- ITS analysis------------------------------#   
#-------------------------R code-----------------------------------------------#
#-----------------Last update:3/4/25------------------------------------------#

# Code adapted from the following project:

# -------------------------------------------------------------------------------
# @project: Two-stage interrupted time series design
# @author: Arnab K. Dey (arnabxdey@gmail.com), Yiqun Ma
# @organization: Scripps Institution of Oceanography, UC San Diego
# @description: This script generates counterfactual plots comparing actual vs predicted respiratory cases
# @date: Dec 20, 2024

# load libraries ----------------------------------------------------------------
rm(list = ls())
pacman::p_load(tidyverse, janitor, data.table, fst, openxlsx, here)

# Server directories
inp <- "D:/Lara/los_angeles_2025_fires_rapid_response/los_angeles_2025_fire_disasters_exp/data/01_raw/"
mod <- "D:/Lara/los_angeles_2025_fires_rapid_response/los_angeles_2025_fire_disasters_exp/Outputs/"
outp <- "D:/Lara/los_angeles_2025_fires_rapid_response/los_angeles_2025_fire_disasters_exp/Outputs/final_results/"


# load data ---------------------------------------------------
# List of dataset names
datasets<- c( "df_Virtual_high")
#datasets<- c("df_Virtual_high", "df_Virtual_moderate", "df_OP_high", "df_OP_moderate") 

# List of encounter types to loop through
encounter_types <- c("num_enc")
#encounter_types <- c( "num_enc", "num_enc_resp", "num_enc_cardio",  "num_enc_neuro", "num_enc_injury")

# set paths ---------------------------------------------------
#source(here("paths-mac.R"))

# Loop through each dataset and load the models and process results
for (dataset_name in datasets) {
  
  # Loop through each encounter type and create a recipe
  for (encounter_type in encounter_types) {
    
# load original data and forecasted predictions -------------------------------------
    forecast_filename <- paste0(outp, "3.2-final-preds_", dataset_name, "_", encounter_type, ".rds")
    
    df_forecast <- readRDS(here(forecast_filename))

# source function to generate counterfactual plots -------------------------------------
source(here("code/03_analysis/", "3.4-func-plot-counterfactuals.R"))

# generate counterfactual plots ---------------------------------------------------
plot_phxgb <- func_plot_counterfactual_boot_noci(df_forecast, model_name = "PROPHETXGB", start_time=1,
                                            intervention_time = 251,  encounter_type = encounter_type, #intervention_time= 191 for Virtual resp
                                            dataset_name = dataset_name)

# save the plot ---------------------------------------------------
    plot_filename <- paste0(outp, "3.5-plot-counterfac-phxgb-boot", dataset_name, "_", encounter_type, ".png")
    
    ggsave(here(plot_filename),
        plot_phxgb,
        width = 10,
        height = 6,
        units = "in",
        dpi = 300,
        bg = "white")
  }}

#---------------Making Figures with final results -----------------------------#

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
    grepl("least", dataset_name, ignore.case = TRUE) ~ "Minimally Exposed",
    TRUE ~ "Other"
  ))

# Split data into two subsets
df_OP_results <- final_results %>% filter(grepl("df_OP", dataset_name))
df_Virtual_results <- final_results %>% filter(grepl("df_Virtual", dataset_name))

df_Virtual_results$exposure_level <- factor(df_Virtual_results$exposure_level, levels = c("Highly Exposed", "Moderately Exposed", "Minimally Exposed")) 

okeefe <- c("#fbe3c2", "#f2c88f", "#ecb27d", "#e69c6b", "#d37750", "#b9563f", "#611F10")
minimal <- okeefe[1]
mod <- okeefe[3]
high <- okeefe[6]

scale_color_manual(values = c("Minimally Exposed" = minimal, 
                              "Moderately Exposed" = mod, 
                              "Highly Exposed" = high))

# Function to create forest plots with custom exposure labels
plot_forest <- function(data) {
  # Define custom labels for encounter types
  encounter_type_labels <- c(
    "num_enc" = "Total Visits",
    "num_enc_resp" = "Respiratory Visits"
  )
  ggplot(data, aes(y = exposure_level, x = excess_pct)) + #, color = encounter_type)) +
    
    # ggplot(data, aes(y = exposure_level, x = excess_per1000, color = encounter_type)) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "black", size = 0.8) +  # Reference line at 0
    geom_point(size = 3, position = position_dodge(width = 0.5)) +  # Main points
    #geom_errorbar(aes(xmin = excess_low_per1000, xmax = excess_up_per1000), width = 0.2, position = position_dodge(width = 0.5)) +  # Error bars
    geom_errorbar(aes(xmin = excess_low_pct, xmax = excess_up_pct), width = 0.2, position = position_dodge(width = 0.5)) +  # Error bars
    
    facet_wrap(~ encounter_type, scales = "free_x", labeller = labeller(encounter_type = encounter_type_labels)) +  # Rename facets
    scale_color_manual(values = c("Least Exposed" = minimal, 
                                  "Moderately Exposed" = mod, 
                                  "Highly Exposed" = high)) +  # Rename legend labels
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
#plot_OP <- plot_forest(df_OP_results)
plot_Virtual <- plot_forest(df_Virtual_results)

#Save plot
ggsave(here(outp, "Resp_Virtual_by_exposure.png"), plot = plot_Virtual, width = 8, height = 6, dpi = 300)
