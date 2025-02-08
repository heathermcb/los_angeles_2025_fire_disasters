#------------------------------------------------------------------------------#
#-------------Los Angeles Wildfires- exploring aggregated health data ---------#   
#-------------------------R code-----------------------------------------------#
#-------------------------Date:2/6/25-----------------------------------------#
#-------------------------Lara Schwarz-----------------------------------------#
#------------------------------------------------------------------------------#
## Purpose: Descriptive stats for health data

# load packages
if (!requireNamespace('pacman', quietly = TRUE)){install.packages('pacman')}
pacman::p_load( readr, dplyr, lubridate, ggplot2, RColorBrewer)

# Create the path to the 'health_data_explore' folder
output_dir <- file.path(getwd(), "data/03_summaries/health_data_explore")

# Check if the folder exists, and create it if it doesn't
if (!dir.exists(output_dir)) {
  dir.create(output_dir)
}

# upload dataset
df <- read_csv("data/01_raw/ENC_EXP_DAILY_01302025.csv")

# Convert 'encounterdt' to Date format 
df$encounterdt <- mdy(df$encounter_dt)  # 'mdy' is from the 'lubridate' package, adjust format if needed

df <- df %>%
  mutate(date = as.Date(encounterdt, format = "%m/%d/%Y"))

# Create a new column with just Month-Day (e.g., "11-07")
df$month_day <- format(df$encounterdt, "%m-%d")

df<- df %>% mutate(exp_pov_category = case_when(
  exp_pov %in% c(0, 1) ~ "0+1",
  exp_pov %in% c(2, 3) ~ "2+3",
  exp_pov %in% c(4, 5) ~ "4+5",
  TRUE ~ NA_character_  # Any other values are set to NA
)) %>%
  filter(!is.na(exp_pov_category))  # Remove rows with NA (which corresponds to "Other")

df <- df %>%
  mutate(
    year = year(date),
    week=week(date)
  ) 

# keeping only data in 2025 after January 7th
df2 <- df %>%
  filter(!(format(encounterdt, "%m-%d") >= "01-07" & format(encounterdt, "%m-%d") <= "01-31" & 
             year(encounterdt) %in% c(2022, 2023, 2024)))

# dataset to look at January data
df3 <- df %>%
  filter( format(encounterdt, "%m-%d") >= "01-01" & format(encounterdt, "%m-%d") <="01-31" )

## Added code to consider different years
#df3 <- df %>%
 # filter( format(encounterdt, "%m-%d") >= "12-01" & format(encounterdt, "%m-%d") <="12-31" &
      #       year(encounterdt) %in% c( 2023) | format(encounterdt, "%m-%d") >= "01-01" & format(encounterdt, "%m-%d") <="01-08" &
        #    year(encounterdt) %in% c( 2024)) 

# Summarizing the sum of each variable by exp_pov_category
df2_exp <- df2 %>%
  group_by(exp_pov_category, date, month_day, enc_type) %>%
  summarise(across(c(num_enc, num_enc_cardio, num_enc_resp, num_enc_neuro, num_enc_injury), sum, na.rm = TRUE)) 

# Calculate the average for each unique month-day (for 2022-2024 from nov 1st to Jan 6th, for 2025 from jan 7th onwards)
df_summary_exp <- df2_exp %>%
  group_by(month_day, enc_type, exp_pov_category) %>%
  summarise(avg_num_tot = mean(num_enc, na.rm = TRUE),
    avg_num_enc_cardio = mean(num_enc_cardio, na.rm = TRUE),
    avg_num_enc_resp = mean(num_enc_resp, na.rm = TRUE),
    avg_num_enc_neuro = mean(num_enc_neuro, na.rm = TRUE),
    avg_num_enc_injury = mean(num_enc_injury, na.rm = TRUE),
    .groups = 'drop'
  )

# Reorder month_day as a factor, ensuring it starts from November to January
df_summary_exp$month_day <- factor(df_summary_exp$month_day, 
                                   levels = c("11-01", "11-02", "11-03", "11-04", "11-05", "11-06", "11-07", "11-08", "11-09", "11-10", "11-11", "11-12", 
                                              "11-13", "11-14", "11-15", "11-16", "11-17", "11-18", "11-19", "11-20", "11-21", "11-22", "11-23", "11-24", 
                                              "11-25", "11-26", "11-27", "11-28", "11-29", "11-30", "12-01", "12-02", "12-03", "12-04", "12-05", "12-06", 
                                              "12-07", "12-08", "12-09", "12-10", "12-11", "12-12", "12-13", "12-14", "12-15", "12-16", "12-17", "12-18", 
                                              "12-19", "12-20", "12-21", "12-22", "12-23", "12-24", "12-25", "12-26", "12-27", "12-28", "12-29", "12-30", 
                                              "12-31", "01-01", "01-02", "01-03", "01-04", "01-05", "01-06", "01-07", "01-08", "01-09", "01-10", "01-11", 
                                              "01-12", "01-13", "01-14", "01-15", "01-16", "01-17", "01-18", "01-19", "01-20", "01-21", "01-22", "01-23", 
                                              "01-24", "01-25", "01-26", "01-27", "01-28", "01-29", "01-30", "01-31"))
# describe types of encounters
encounter_types <- c("avg_num_tot", "avg_num_enc_cardio", "avg_num_enc_resp", "avg_num_enc_neuro", "avg_num_enc_injury")


# Loop through each encounter type and generate/save the plot
# Loop through each encounter type and create/save the plot
for(enc_type in encounter_types) {
  
  # Create a time series plot for the current encounter type
  p <- ggplot(df_summary_exp, aes(x = month_day, y = .data[[enc_type]], color = exp_pov_category, group = exp_pov_category)) +
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


# Create a binary variable: 0 for Nov 1 to Jan 6, and 1 for Jan 7 to Jan 30
df_summary_exp <- df_summary_exp %>%
  mutate(binary_jan_period = case_when(
    as.character(month_day) <= "01-06" ~ 0,  # Days from Nov 1 to Jan 6
    as.character(month_day) >= "11-01" ~ 0,  # Days from Nov 1 to Jan 6
    as.character(month_day) >= "01-07" & as.character(month_day) <= "01-30" ~ 1  # Days from Jan 7 to Jan 30
  ))

summary_df <- df_summary_exp %>%
  group_by(binary_jan_period, enc_type,  exp_pov_category ) %>%
  summarise(avg_num_tot = mean(avg_num_tot, na.rm = TRUE),
            avg_num_enc_cardio = mean(avg_num_enc_cardio, na.rm = TRUE),
            avg_num_enc_resp = mean(avg_num_enc_resp, na.rm = TRUE),
            avg_num_enc_neuro = mean(avg_num_enc_neuro, na.rm = TRUE),
            avg_num_enc_injury = mean(avg_num_enc_injury, na.rm = TRUE),
            .groups = 'drop'
  )


# Save the table (summary_df) to the 'health_data_explore' folder
summary_csv_path <- file.path(output_dir, "summary_pre_post_wf.csv")
write_csv(summary_df, summary_csv_path)


#labels for figure
new_exp_pov_labels <- c("0+1" = "Least", 
                        "2+3" = "Moderate", 
                        "4+5" = "High")
# Loop through each encounter type and create/save the plot
for(enc_type in encounter_types) {
  
  # Create a box plot for the current diagnosis type
  p <- ggplot(df_summary_exp, aes(x = as.factor(binary_jan_period), y = .data[[enc_type]], 
                                  fill = as.factor(binary_jan_period))) +
    geom_boxplot() +  # Create box plot
    facet_grid(enc_type ~ exp_pov_category, scales = "free", 
               labeller = labeller(exp_pov_category = new_exp_pov_labels)) +  
    scale_fill_manual(values = c("0" = "lightblue", "1" = "lightcoral")) +  # Custom colors for binary_jan_period
    labs(
      title = paste("Average by Encounter Before/After Fires for", enc_type),
      x = "Before + After Fire",
      y = paste("Avg Count for", enc_type),
      fill = "Before + After Fire"
    ) +  # Label for fill
    theme_light() +
    theme(legend.position = "none")  # Remove the global legend
  
  # Construct the full path for the plot file
  plot_filename <- file.path(output_dir, paste0("boxplot_", enc_type, "_before_after_fires.png"))
  
  # Save the plot to the specified folder
  ggsave(plot_filename, plot = p, width = 10, height = 6)
}

# Focusing on days 1-24 in January
df3 <- df3 %>%
  mutate(
    year = year(date), 
    day_of_year = yday(date),
   month_day = format(df3$date, "%m-%d") 
  ) #%>%
  #filter(day_of_year >= 1 & day_of_year <= 24)  # Focus on January (days 1-31)


df3_summary <- df3 %>%
  group_by(date, exp_pov_category, day_of_year, year, enc_type, month_day) %>%
  summarise(avg_num_tot = mean(num_enc, na.rm = TRUE),
            avg_num_enc_cardio = mean(num_enc_cardio, na.rm = TRUE),
            avg_num_enc_resp = mean(num_enc_resp, na.rm = TRUE),
            avg_num_enc_neuro = mean(num_enc_neuro, na.rm = TRUE),
            avg_num_enc_injury = mean(num_enc_injury, na.rm = TRUE),
            .groups = 'drop'
  )

df3_summary$month_day <- factor(df3_summary$month_day, 
                                   levels = c( "12-01", "12-02", "12-03", "12-04", "12-05", "12-06", 
                                              "12-07", "12-08", "12-09", "12-10", "12-11", "12-12", "12-13", "12-14", "12-15", "12-16", "12-17", "12-18", 
                                              "12-19", "12-20", "12-21", "12-22", "12-23", "12-24", "12-25", "12-26", "12-27", "12-28", "12-29", "12-30", 
                                              "12-31", "01-01", "01-02", "01-03", "01-04", "01-05", "01-06", "01-07", "01-08", "01-09", "01-10", "01-11", 
                                              "01-12", "01-13", "01-14", "01-15", "01-16", "01-17", "01-18", "01-19", "01-20", "01-21", "01-22", "01-23", 
                                              "01-24", "01-25", "01-26", "01-27", "01-28", "01-29", "01-30", "01-31"))
# Plotting the time series with facets by exposure and year
# Loop through each encounter variable and create/save the plot
for(enc_var in encounter_types) {
  
  # Create the plot for the current encounter variable
  p <- ggplot(df3_summary, aes(x = month_day, y = .data[[enc_var]], group = year, color = factor(year))) +
    geom_line() + 
    facet_wrap(~ exp_pov_category + enc_type, scales = "free", labeller = labeller(
      exp_pov_category = c(
        "0+1" = "Least Exposure", 
        "2+3" = "Moderate Exposure", 
        "4+5" = "High Exposure"
      ))) +  # Facet by both exp_pov_category and enc_type
    labs(
      title = paste("Encounters in January by Year, Exposure Category, and Encounter Type for", enc_var),
      x = "Day of January",
      y = paste("Number of", gsub("avg_num_", "Avg ", enc_var)),  # Clean the label for better readability
      color = "Year"
    ) +
    geom_vline(xintercept = 7, linetype = "dotted", color = "black", size = 0.5) +
    scale_color_brewer(palette = "Set1", direction = -1) +  # Customize legend labels
    theme_light() +
    theme(legend.position = "top")
  
  # Construct the full path for the plot file
 # plot_filename <- file.path(output_dir, paste0("encounters_avg_num_enc", enc_var, "_by_year_and_exposure.png"))
  plot_filename <- file.path(output_dir, paste0("encounters_dec+jan_23_24", enc_var, "_by_year_and_exposure.png"))
  
  # Save the plot to the specified folder
  ggsave(plot_filename, plot = p, width = 10, height = 6)
}

