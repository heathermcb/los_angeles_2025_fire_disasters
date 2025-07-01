# -------------------------------------------------------------------------------
#-------------Los Angeles Wildfires- ITS analysis------------------------------#   
#-------------------------R code-----------------------------------------------#
#-----------------Last update:2/28/25------------------------------------------#

# Code adapted from the following project:

# -------------------------------------------------------------------------------
# @project: Two-stage interrupted time series design
# @author: Arnab K. Dey , Yiqun Ma
# @organization: Scripps Institution of Oceanography, UC San Diego
# @description: This script contains functions to generate counterfactual plots
# @date: Nov 12, 2024
# -------------------------------------------------------------------------------

#' Generate Counterfactual Plots
#' 
#' @description Creates a ggplot comparing actual vs predicted respiratory hospitalizations
#'              with confidence intervals for the specified model
#'
#' @param df_forecast A data frame containing forecast data with columns:
#'                    date, respiratory_actual, respiratory_pred, conf_lo, conf_hi
#' @param model_name Character string specifying the model 
#' @param start_date Character string specifying the start date for plotting in "YYYY-MM-DD" format
#'                   
#' 
#' @return A ggplot object showing the actual vs predicted values with confidence intervals
#'

# 
#
# Create forecast plot ---------------------------------------------------
func_plot_counterfactual_boot <- function(df_forecast, 
                                          model_name = "PROPHETXGB", 
                                          start_time = 1,
                                          intervention_time = 251,
                                         # intervention_time = 191, #Virtual resp
                                          encounter_type = encounter_type,
                                          dataset_name = dataset_name) {
  
  # Create data subsets for actual and forecasted values
  df_forecast_filtered <- df_forecast |> filter(time >= start_time)
  ggplot(data = df_forecast_filtered) +
    geom_vline(xintercept = intervention_time,
               linetype = "dashed", color = "red") +
    geom_line(aes(x = time, y = num_cases, color = "Actual")) +
    geom_ribbon(data = df_forecast_filtered |> filter(time >= intervention_time),
                aes(x = time, ymin = conf_lo, ymax = conf_hi),
                alpha = 0.8, fill = "#FFD93D") +
    geom_line(aes(x = time, y = num_pred, color = model_name), linetype = "dashed") +
    scale_color_manual(values = setNames(c("#1E3D59", "grey"), c("Actual", model_name))) +
    theme_minimal() +
    labs(title = paste("Actual vs", model_name, "Counterfactual for", encounter_type, "in", dataset_name),
         x = "Time", y = "Number of encounters", color = "Type") +
    theme(legend.position = "bottom",
          axis.text.x = element_text(angle = 0, hjust = 0.5))
  
  
}

# Create forecast plot (NO CI)---------------------------------------------------

func_plot_counterfactual_boot_noci <- function(df_forecast, 
                                          model_name = "PROPHETXGB", 
                                          start_time = 1,
                                          intervention_time = 251,
                                         # intervention_time = 191, #Virtual resp
                                          
                                          encounter_type = encounter_type,
                                          dataset_name = dataset_name) {
  
  # Create data subsets for actual and forecasted values
  df_forecast_filtered <- df_forecast |> filter(time >= start_time)
  
  ggplot(data = df_forecast_filtered) +
    geom_vline(xintercept = intervention_time,
               linetype = "dashed", color = "red") +
    geom_line(aes(x = time, y = num_cases, color = "Actual")) +
    geom_line(aes(x = time, y = num_pred, color = model_name), linetype = "dashed") +
    scale_color_manual(values = setNames(c("#1E3D59", "orange"), c("Actual", model_name))) +
    theme_minimal() +
    labs(title = paste("Actual vs", model_name, "Counterfactual for", encounter_type, "in", dataset_name),
         x = "Time", y = "Number of encounters", color = "Type") +
    theme(legend.position = "bottom",
          axis.text.x = element_text(angle = 0, hjust = 0.5))
}
