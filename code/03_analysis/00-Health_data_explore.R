#------------------------------------------------------------------------------#
#-------------Los Angeles Wildfires- exploring aggregated health data ---------#   
#-------------------------R code-----------------------------------------------#
#-----------------Last update: 3/4/25------------------------------------------#

## Purpose: Descriptive stats for health data
#---------------Load packages and define working directory---------------------#
# load packages
if (!requireNamespace('pacman', quietly = TRUE)){install.packages('pacman')}
pacman::p_load( readr, dplyr, lubridate, ggplot2, RColorBrewer)

# Create the path to the 'health_data_explore' folder
output_dir <- file.path(getwd(), "data/03_summaries/health_data_explore")

# Check if the folder exists, and create it if it doesn't
if (!dir.exists(output_dir)) {
  dir.create(output_dir)
}

# Server directories
inp <- "D:/Lara/los_angeles_2025_fires_rapid_response/los_angeles_2025_fire_disasters_exp/data/01_raw/"

#---------------------- Data upload and prep-----------------------------------#
# upload dataset
df_original <- read_csv(paste0(inp, "ENC_EXP_DAILY_01302025.csv"))  
df_updated <- read_csv(paste0(inp,"ENC_EXP_DAILY_02102025_updated.csv"))
df_updated2 <- read_csv(paste0(inp,"ENC_EXP_DAILY_02232025.csv"))

# Define time periods
time_periods <- list(
  "2022_2023" = list(start = ymd("2022-11-01"), end = ymd("2023-01-21")),
  "2023_2024" = list(start = ymd("2023-11-01"), end = ymd("2024-01-21")),
  "2024_2025" = list(start = ymd("2024-11-01"), end = ymd("2025-01-21"))
)

# describe types of encounters
encounter_types <- c("num_enc", "num_enc_cardio", "num_enc_resp", "num_enc_neuro", "num_enc_injury")

#--------------------------------- Combining and cleaning datasets-----------------------------------#
# Function to process each dataset
process_dataset <- function(df, dataset_name) {
  df %>%
    mutate(date = as.Date(encounter_dt, format = "%m/%d/%Y"),
           month_day = format(date, "%m-%d"),
           exp_pov_category = case_when(
             exp_pov %in% c(0, 1) ~ "0+1",
             exp_pov %in% c(2, 3) ~ "2+3",
             exp_pov %in% c(4, 5) ~ "4+5",
             TRUE ~ NA_character_
           )) %>%
    filter(!is.na(exp_pov_category)) %>% 
    mutate(year = year(date),
           week = week(date),
           day_of_year = yday(date)) %>%
    group_by(date, exp_pov_category, day_of_year, year, enc_type, month_day) %>%
    summarise(num_enc = sum(num_enc, na.rm = TRUE),
              num_enc_cardio = sum(num_enc_cardio, na.rm = TRUE),
              num_enc_resp = sum(num_enc_resp, na.rm = TRUE),
              num_enc_neuro = sum(num_enc_neuro, na.rm = TRUE),
              num_enc_injury = sum(num_enc_injury, na.rm = TRUE),
              .groups = 'drop') %>%
    mutate(month_day = format(as.Date(date), "%m-%d"),
           time_period = case_when(
             between(date, time_periods[["2022_2023"]][["start"]], time_periods[["2022_2023"]][["end"]]) ~ "2022_2023",
             between(date, time_periods[["2023_2024"]][["start"]], time_periods[["2023_2024"]][["end"]]) ~ "2023_2024",
             between(date, time_periods[["2024_2025"]][["start"]], time_periods[["2024_2025"]][["end"]]) ~ "2024_2025",
             TRUE ~ NA_character_
           )) %>%
    filter(!is.na(time_period)) %>%
    mutate(dataset = dataset_name)  # Label the dataset source
}

# Process both datasets
df_summary <- process_dataset(df_original, "Original")
df_updated_summary <- process_dataset(df_updated, "Updated 2/9")
df_updated2_summary <- process_dataset(df_updated2, "Updated 2/23")

# Combine both summaries
df_combined_summary <- bind_rows(df_summary, df_updated_summary, df_updated2_summary)


# Generate the dates for November, December, and January
nov_dates <- sprintf("11-%02d", 1:30)
dec_dates <- sprintf("12-%02d", 1:31)
jan_dates <- sprintf("01-%02d", 1:21)

# Combine all the dates into a vector
all_dates <- c(nov_dates, dec_dates, jan_dates)

# Ensure month_day has consistent factor levels
df_combined_summary$month_day <- factor(df_combined_summary$month_day, levels = all_dates)

#---------------------------Descriptive Figures + Tables-----------------------------------#
## Define most up-to-date dataset
df_most_recent<-df_combined_summary %>%
  filter(dataset=="Updated 2/23")

## Figure 1: Daily time series with average pre-smoke of all years and only 2025 post-smoke

# keeping only data in 2025 after January 7th
df_most_recent <- df_most_recent %>%
  filter(!(month_day >= "01-07" & month_day <= "01-31" & 
             year(encounterdt) %in% c(2022, 2023, 2024)))

# Calculate the average for each unique month-day (for 2022-2024 from nov 1st to Jan 6th, for 2025 from jan 7th onwards)
df_most_recent <- df_most_recent %>%
  group_by(month_day, enc_type, exp_pov_category) %>%
  summarise(num_enc = mean(num_enc, na.rm = TRUE),
    num_enc_cardio = mean(num_enc_cardio, na.rm = TRUE),
    num_enc_resp = mean(num_enc_resp, na.rm = TRUE),
    num_enc_neuro = mean(num_enc_neuro, na.rm = TRUE),
    num_enc_injury = mean(num_enc_injury, na.rm = TRUE),
    .groups = 'drop'
  )

# Loop through each encounter type and create/save the plot
for(enc_type in encounter_types) {
  
  # Create a time series plot for the current encounter type
  p <- ggplot(df_most_recent, aes(x = month_day, y = .data[[enc_type]], color = exp_pov_category, group = exp_pov_category)) +
    geom_line() +  # Plot lines
    scale_color_brewer(palette = "Set1", direction = -1) +  # Reverse the color palette
    facet_wrap(~ enc_type, scales = "free_y") +  # Facet by enc_type with different y-axis scales
    geom_vline(xintercept = "01-07", linetype = "dotted", color = "black", size = 0.5) +
    labs(
      title = paste("Time-Series Plot by Exposure and Encounter Type:", enc_type),
      x = "Day",
      y = paste("Number of Visits for", enc_type),
      color = "Exposure",
      linetype = "Enc Type"
    ) +
    scale_color_brewer(palette = "Set1", direction = -1, labels = c("Least", "Moderate", "High")) +  # Customize legend labels
    scale_x_discrete(breaks = function(x) x[seq(1, length(x), by = 20)]) +  # Show every 20th day
    theme_light() +
    theme(legend.position = "bottom")  # Customize legend position
  
  # Construct the full path for the plot file
  plot_filename <- file.path(output_dir, paste0("time_series_plot_", enc_type, "_v2.png"))
  
  # Save the plot to the specified folder
  ggsave(plot_filename, plot = p, width = 10, height = 6)
  
}

## Figure 2: Boxplot of pre- and post-wildfire counts
# Loop through each encounter type and create/save the plot

df_most_recent_jan <- df_most_recent %>%
  filter(substr(as.character(month_day), 1, 2) == "01") %>%  # Ensure month_day is character
  mutate(binary_jan_period = ifelse(date > "2025-01-07", 1, 0))

for(enc_type in encounter_types) {
  
  # Create a box plot for the current diagnosis type
  p <- ggplot(df_most_recent_jan, aes(x = as.factor(binary_jan_period), y = .data[[enc_type]], 
                                  fill = as.factor(binary_jan_period))) +
    geom_boxplot() +  # Create box plot
    facet_grid(enc_type ~ exp_pov_category, scales = "free", 
               labeller = labeller(exp_pov_category = c("Least", "Moderate", "High"))) +  
    scale_fill_manual(values = c("0" = "lightblue", "1" = "lightcoral"), 
                      labels = c("Before Fire", "After Fire")) +  # Updated labels for binary_jan_period
    labs(
      title = paste("Average by Encounter Before/After Fires for", enc_type),
      x = "Before + After Fire",
      y = paste("Avg Count for", enc_type),
      fill = "Exposure"
    ) +  # Consistent label for fill
    theme_light() +
    theme(legend.position = "bottom")  # Match legend position with time-series plot
  
  # Construct the full path for the plot file
  plot_filename <- file.path(output_dir, paste0("boxplot_", enc_type, "_before_after_fires.png"))
  
  # Save the plot to the specified folder
  #ggsave(plot_filename, plot = p, width = 10, height = 6)
}

## Figure 3: Daily time series by period 

# Plotting the time series with facets by exposure and year
# Loop through each encounter variable and create/save the plot
for(enc_var in encounter_types) {
  
  # Create the plot for the current encounter variable
  p <- ggplot(df_summary, aes(x = month_day, y = .data[[enc_var]], group = time_period, color = factor(time_period))) +
    geom_line() + 
    facet_wrap(~ exp_pov_category + enc_type, scales = "free", labeller = labeller(
      exp_pov_category = c(
        "0+1" = "Least Exposure", 
        "2+3" = "Moderate Exposure", 
        "4+5" = "High Exposure"
      ))) +  # Facet by both exp_pov_category and enc_type
    labs(
      title = paste("Encounters by Year, Exposure Category, and Encounter Type for", enc_var),
      x = "Day of January",
      y = paste("Number of", gsub("avg_num_", "Avg ", enc_var)),  # Clean the label for better readability
      color = "Year"
    ) +
    geom_vline(xintercept = which(levels(df_summary$month_day) == "01-07"), linetype = "dotted", color = "black", size = 0.5) +  
    scale_color_brewer(palette = "Set1", direction = -1) +  # Customize legend labels +
    scale_x_discrete(breaks = df_summary$month_day[seq(1, length(df_summary$month_day), by = 50)]) +  # Show labels every 20 days
    theme_light() +
    theme(legend.position = "top")
  
  # Construct the full path for the plot file
 # plot_filename <- file.path(output_dir, paste0("encounters_avg_num_enc", enc_var, "_by_year_and_exposure.png"))
  plot_filename <- file.path(output_dir, paste0("encounters_", enc_var, "_by_time_period_and_exposure.png"))
  
  # Save the plot to the specified folder
 # ggsave(plot_filename, plot = p, width = 10, height = 6)
}

## Figure 4: Daily time series focusing on 2024-2025

# Filter for only the 2024-2025 time period
df_filtered <- df_combined_summary %>% filter(time_period == "2024_2025")

# Plot code for each encounter type
for(enc_var in encounter_types) {
  
  # Create the plot for the current encounter variable
  p <- ggplot(df_filtered, aes(x = month_day, y = .data[[enc_var]], group = dataset, color = dataset, linetype = dataset)) +
    geom_line(size = 1) +  # Use dataset to determine line type for differentiation
    facet_wrap(~ exp_pov_category + enc_type, scales = "free", labeller = labeller(
      exp_pov_category = c(
        "0+1" = "Least Exposure", 
        "2+3" = "Moderate Exposure", 
        "4+5" = "High Exposure"
      )
    )) +  
    labs(
      title = paste("Encounters by Exposure Category and Encounter Type for", enc_var, " (2024-2025)"),
      x = "Day of 2024-2025",
      y = paste("Number of", gsub("avg_num_", "Avg ", enc_var)),  
      color = "Dataset",
      linetype = "Dataset"
    ) +
    geom_vline(xintercept = which(levels(df_filtered$month_day) == "01-07"), 
               linetype = "dotted", color = "black", size = 0.3) +  
    scale_color_brewer(palette = "Set1", direction = -1) +  
    scale_x_discrete(breaks = levels(df_filtered$month_day)[seq(1, length(levels(df_filtered$month_day)))]) +  
    theme_light() +
    theme(legend.position = "top")
  
  # Construct the full path for the plot file
  plot_filename <- file.path(output_dir, paste0("encounters_", enc_var, "_2024_2025_by_dataset.png"))
  
  # Save the plot to the specified folder
  #ggsave(plot_filename, plot = p, width = 10, height = 6)
}


## Figure 4: Daily time series focusing on weekends

## weekend plot

# Convert month_day to Date format if necessary (assuming it includes a valid year)
df_combined_weekends <- df_combined_summary %>%
  mutate(date = as.Date(date, format = "%Y-%m-%d")) %>%
  filter(wday(date, label = TRUE) %in% c("Sat", "Sun")) %>%
arrange(date) %>%  # Ensure data is ordered by date
  mutate(date_index = row_number())  # Create a numeric sequence for weekends# Keep only weekends

df_combined_weekends <- df_combined_weekends %>% filter(dataset == "Updated 2/9")
df_combined_weekends <- df_combined_weekends %>% filter(enc_type == "OP"|enc_type == "Virtual")
df_combined_weekends <- df_combined_weekends %>% filter(exp_pov_category == "2+3"|exp_pov_category == "4+5")

# Ensure enc_type is a factor and order it
df_combined_weekends <- df_combined_weekends %>%
  mutate(
    enc_type = factor(enc_type, levels = sort(unique(enc_type)))  # Order alphabetically or define custom order
  )

# Plot code for each encounter type
for(enc_var in encounter_types) {
  
  # Create the plot using date_index instead of date
  p <- ggplot(df_combined_weekends, aes(x = date_index, y = .data[[enc_var]])) +
    geom_point(size = 1, color = "blue") +  # Single blue line
    facet_wrap(~ exp_pov_category + enc_type, scales = "free", labeller = labeller(
      exp_pov_category = c(
        "0+1" = "Least Exposure", 
        "2+3" = "Moderate Exposure", 
        "4+5" = "High Exposure"
      )
    )) +  
    labs(
      title = paste("Encounters by Exposure Category and Encounter Type for", enc_var, "(Weekends Only)"),
      x = "Date",
      y = paste("Number of", gsub("avg_num_", "Avg ", enc_var))
    ) +
    geom_vline(xintercept = df_combined_weekends$date_index[df_combined_weekends$date == as.Date("2025-01-11")], 
               linetype = "dotted", color = "red", size = 0.8, na.rm = TRUE) +  # Mark specific date if present
    scale_x_continuous(
      breaks = df_combined_weekends$date_index,  # Breaks based on the date_index
    ) + 
    theme_light() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels
      strip.text = element_text(size = 10, face = "bold")  # Make facet labels more readable
    )
  
  # Construct the full path for the plot file
  plot_filename <- file.path(output_dir, paste0("encounters_", enc_var, "_weekends_v3.png"))
  
  # Save the plot to the specified folder
 # ggsave(plot_filename, plot = p, width = 10, height = 6)
}


## Table 1: Total counts for 2024-2025

# Creating total counts
df_total <- df_filtered %>%
  group_by(dataset, enc_type) %>%
  summarise(across(all_of(encounter_types), sum, na.rm = TRUE), .groups = "drop")

write_csv(df_total, "comparison_totals_24_25.csv")


## Table 2: average pre-smoke and post-smoke averages

# Create a binary variable: 0 for Nov 1 to Jan 6, and 1 for Jan 7 to Jan 30
df_most_recent <- df_most_recent %>%
  mutate(binary_jan_period = case_when(
    as.character(month_day) <= "01-06" ~ 0,  # Days from Nov 1 to Jan 6
    as.character(month_day) >= "11-01" ~ 0,  # Days from Nov 1 to Jan 6
    as.character(month_day) >= "01-07" & as.character(month_day) <= "01-30" ~ 1  # Days from Jan 7 to Jan 30
  ))

df_most_recent <- df_most_recent %>%
  group_by(binary_jan_period, enc_type,  exp_pov_category ) %>%
  summarise(avg_num_enc = mean(num_enc, na.rm = TRUE),
            avg_num_enc_cardio = mean(num_enc_cardio, na.rm = TRUE),
            avg_num_enc_resp = mean(num_enc_resp, na.rm = TRUE),
            avg_num_enc_neuro = mean(num_enc_neuro, na.rm = TRUE),
            avg_num_enc_injury = mean(num_enc_injury, na.rm = TRUE),
            .groups = 'drop'
  )


# Save the table (summary_df) to the 'health_data_explore' folder
summary_csv_path <- file.path(output_dir, "summary_pre_post_wf.csv")
write_csv(summary_df, summary_csv_path)
