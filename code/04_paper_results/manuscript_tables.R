## ---------------------------
## LBW
## created: 2025-02-27
## last updated: 22 Feb 25
## goal: produce manuscript tables
## breakdown: 
##       1. 8 tables total, all combos of the below: 
##          2 visit types (virtual versus outpatient (op))
##          2 exposure levels (moderately versus highly)
##          2 metrics (excess versus excess_pct)   
##          
##          each table then contains: all-cause, cardio, resp, neuro, injury
##
##       2. one summary table of counts! 
## ---------------------------


# setup ---------------------------
# install and load packages
if(!requireNamespace('pacman', quietly = TRUE)) install.packages('pacman') 
pacman::p_load(tidyverse, gt, magick, pagedown)

# project specific paths
rootdir_personal <- paste0("~/Desktop/Desktop/epidemiology_PhD/00_repos/")
rootdir_prj <- paste0(rootdir_personal, "LA25_fires_edvisits/results/")
rootdir_sp <- paste0("/Users/laurenwilner/Library/CloudStorage/OneDrive-SharedLibraries-UW/casey_cohort\ -\ Documents/")
rootdir_sp_studies <-  paste0(rootdir_sp, "studies/")
rootdir_sp_kaiser_la <- paste0(rootdir_sp_studies, "kaiser_la/")


# process results data ---------------------------
results <- read_csv(paste0(rootdir_sp_kaiser_la, "results_by_day_61725.csv")) %>%
  mutate(
    date = date,
    encounter_type = encounter_type,
    dataset_name = dataset_name,
    expected_CI = expected_CI,
    excess_CI = excess_CI,
    excess_pct_CI = excess_pct_CI,
    excess_per1000_CI = excess_per1000_CI,

    # extract values into three separate cols: expected
    expected = as.numeric(gsub("\\s*\\(.*$", "", expected_CI)),  # number before parentheses, remove any spaces
    expected_low = as.numeric(gsub(".*\\(\\s*([0-9.-]+)\\s*,.*", "\\1", expected_CI)),  # first number in parentheses
    expected_up = as.numeric(gsub(".*,\\s*([0-9.-]+)\\s*\\).*", "\\1", expected_CI)),  # second number in parentheses
    
    # extract values into three separate cols: excess
    excess = as.numeric(gsub("\\s*\\(.*", "", excess_CI)),  # number before parentheses
    excess_low = as.numeric(gsub(".*\\(([0-9.-]+),.*", "\\1", excess_CI)),  # first number in parentheses
    excess_up = as.numeric(gsub(".*,\\s*([0-9.-]+)\\).*", "\\1", excess_CI)),  # second number in parentheses
    
    # extract values into three separate cols: excess pct
    excess_pct = as.numeric(gsub("\\s*\\(.*", "", excess_pct_CI)),
    excess_low_pct = as.numeric(gsub(".*\\(([0-9.-]+),.*", "\\1", excess_pct_CI)),
    excess_up_pct = as.numeric(gsub(".*,\\s*([0-9.-]+)\\).*", "\\1", excess_pct_CI)),
    
    # extract values into three separate cols: excess per1000 [not using so leaving it]
    excess_per1000_CI = as.numeric(gsub("\\s*\\(.*", "", excess_per1000_CI))
  ) %>%
  select(
    date,
    encounter_type, 
    dataset_name,
    expected_CI,
    excess_CI, 
    excess_pct_CI,
    excess_per1000_CI,
    expected,
    expected_low,
    expected_up,
    excess,
    excess_low, 
    excess_up,
    excess_pct,
    excess_low_pct,
    excess_up_pct
  ) %>%   
  mutate(dataset_name = str_remove(dataset_name, "^df_")) %>%
  separate(dataset_name, into = c("visit_type", "exposure"), sep = "_", remove = FALSE) %>% 
  mutate_at(c("exposure", "visit_type", "encounter_type"), as.factor)  %>% 
  mutate(visit_encounter = paste0(visit_type, "_", encounter_type)) %>% 
  mutate(exposure = ifelse(exposure == "moderate", "Moderately", "Highly"),
        date = as.Date(date, format = "%m/%d/%y"), 
        Weekday = weekdays(date)) %>% 
  # filter to just the first week 
  filter(date >= "2025-01-07" & date <= "2025-01-13")

# process weekly change data ---------------------------
# these will be appended to the end of the tables
weekly_change_pct <- read_csv(paste0(rootdir_sp_kaiser_la, "results_first_week_61725.csv")) %>% 
  filter(dataset_name != "df_Virtual_least") %>%
  mutate(
    # round all decimal numbers
    excess_pct_CI = stringr::str_replace_all(excess_pct_CI, "\\d+\\.\\d+", function(x) as.character(round(as.numeric(x)))),
    # add percent signs
    excess_pct_CI = gsub("(\\d+)(?=\\s|,|\\))", "\\1%", excess_pct_CI, perl = TRUE)
  ) %>%
  select(period, dataset_name, encounter_type, excess_pct_CI) %>%
  pivot_wider(
    names_from = encounter_type,
    values_from = excess_pct_CI
  ) %>% 
  rename("All-cause" = num_enc,
         "Cardiovascular" = num_enc_cardio,
         "Respiratory" = num_enc_resp,
         "Neuropsychiatric" = num_enc_neuro,
         "Injury" = num_enc_injury) %>%
  mutate("date" = "Weekly change", "Weekday" = "") %>% 
  select(date, Weekday, everything()) %>% 
  mutate("metric" = "pct") %>% 
  mutate(visit_type = ifelse(grepl("Virtual", dataset_name), "Virtual", "OP"),
         exposure = ifelse(grepl("high", dataset_name), "Highly", "Moderately")) %>% 
  select(-c("period", "dataset_name"))


weekly_change_abs <- read_csv(paste0(rootdir_sp_kaiser_la, "results_first_week_61725.csv")) %>% 
  filter(dataset_name != "df_Virtual_least") %>%
  select(period, dataset_name, encounter_type, excess_CI) %>%
  pivot_wider(
    names_from = encounter_type,
    values_from = excess_CI
  ) %>% 
  rename("All-cause" = num_enc,
         "Cardiovascular" = num_enc_cardio,
         "Respiratory" = num_enc_resp,
         "Neuropsychiatric" = num_enc_neuro,
         "Injury" = num_enc_injury) %>%
  mutate("date" = "Weekly change", "Weekday" = "") %>% 
  select(date, Weekday, everything()) %>% 
  mutate("metric" = "abs") %>%
  mutate(visit_type = ifelse(grepl("Virtual", dataset_name), "Virtual", "OP"),
         exposure = ifelse(grepl("high", dataset_name), "Highly", "Moderately")) %>% 
  select(-c("period", "dataset_name"))


okeefe <- c("#fbe3c2", "#f2c88f", "#ecb27d", "#e69c6b", "#d37750", "#b9563f", "#611F10")

# helper function ---------------------------

## function to create table dfs
create_encounter_table <- function(df, exposure_level, visit, metric_suffix = "") {

  # define vars using suffix
  main_var <- paste0("excess", metric_suffix)
  lower_var <- paste0("excess_low", metric_suffix)
  upper_var <- paste0("excess_up", metric_suffix)
  
  # filter by exposure and visit type and select correct vars
  df_small <- df %>%
    filter(exposure == exposure_level & visit_type == visit) %>% 
    select(date, Weekday, encounter_type, !!sym(main_var), !!sym(lower_var), !!sym(upper_var)) %>% 
    # round main var and upper/lower to whole numbers
    mutate(
      !!sym(main_var) := round(!!sym(main_var)),
      !!sym(lower_var) := round(!!sym(lower_var)),
      !!sym(upper_var) := round(!!sym(upper_var))
    )
  
  # create the formatted table
  result_table <- df_small %>% 
    mutate(
      formatted_value = case_when(
        # for percentage metrics, format with % sign
        grepl("_pct$", metric_suffix) ~ 
          sprintf("%d%% (%d%%, %d%%)", 
                 as.integer(!!sym(main_var)),
                 as.integer(!!sym(lower_var)),
                 as.integer(!!sym(upper_var))),
        # Foforr non-percentage metrics, format without % sign
        TRUE ~ 
          sprintf("%d (%d, %d)", 
                 as.integer(!!sym(main_var)),
                 as.integer(!!sym(lower_var)),
                 as.integer(!!sym(upper_var)))
      )
    ) %>%
    select(encounter_type, formatted_value, date, Weekday) %>%
    # make nice labels for encounter types
    mutate(
      encounter_type = case_when(
        encounter_type == "num_enc_cardio" ~ "Cardiovascular",
        encounter_type == "num_enc_injury" ~ "Injury",
        encounter_type == "num_enc_resp" ~ "Respiratory",
        encounter_type == "num_enc_neuro" ~ "Neuropsychiatric",
        encounter_type == "num_enc" ~ "All-cause",
        TRUE ~ encounter_type
      )
    ) %>%
    pivot_wider(names_from = encounter_type, values_from = formatted_value)
  
  # reorder cols to be in alphabetical order 
  result_table <- result_table[, c("date", "Weekday", "All-cause", "Cardiovascular", "Injury", "Neuropsychiatric", "Respiratory")]
  return(result_table)
}

# make the tables finally ---------------------------

## make the 4 combinations of tables (combining percent and absolute)
visit_types <- c("OP", "Virtual")
exposure_levels <- c("Moderately", "Highly")

for(visit in visit_types) {
  for(exposure_level in exposure_levels) {
    print(paste0("Creating combined table for ", visit, " visits, ", exposure_level, " exposed"))
    
    # create the absolute table
    result_table_abs <- create_encounter_table(results, exposure_level, visit, "") %>% 
      mutate(date = as.character(date))
    # rbind the absolute table with the weekly change data
    result_table_abs <- bind_rows(result_table_abs, weekly_change_abs %>% filter(visit_type == visit, exposure == exposure_level)) %>% 
      select(-c("metric", "visit_type", "exposure"))
    
    # create the percent table
    result_table_pct <- create_encounter_table(results, exposure_level, visit, "_pct") %>% 
      mutate(date = as.character(date)) 
    # rbind the pct table with the weekly change data
    result_table_pct <- bind_rows(result_table_pct, weekly_change_pct %>% filter(visit_type == visit, exposure == exposure_level)) %>% 
      select(-c("metric", "visit_type", "exposure"))

    
    # combine the tables by merging the encounter type columns
    combined_table <- result_table_abs %>%
      left_join(result_table_pct, by = c("date", "Weekday"), suffix = c("_abs", "_pct")) %>%
      mutate(
        `All-cause` = paste0(`All-cause_pct`, "<br>", `All-cause_abs`),
        `Cardiovascular` = paste0(`Cardiovascular_pct`, "<br>", `Cardiovascular_abs`),
        `Injury` = paste0(`Injury_pct`, "<br>", `Injury_abs`),
        `Neuropsychiatric` = paste0(`Neuropsychiatric_pct`, "<br>", `Neuropsychiatric_abs`),
        `Respiratory` = paste0(`Respiratory_pct`, "<br>", `Respiratory_abs`)
      ) %>%
      select(date, Weekday, `All-cause`, Cardiovascular, Injury, Neuropsychiatric, Respiratory)
    
    # make the pretty table
    filename <- paste0(rootdir_sp_kaiser_la, "output/june2025/table_", visit, "_", exposure_level, "_combined.png")
    filename_html <- paste0(rootdir_sp_kaiser_la, "output/june2025/table_", visit, "_", exposure_level, "_combined.html")
    
    # modify the pretty table function call to handle combined data
    pretty_table <- combined_table %>%
      rename(Date = date) %>%
      gt() %>%
      tab_header(title = md("*Excess percent (95% eCI)<br>Excess count (95% eCI)*")) %>% 

      # set custom widths for columns
      cols_width(
        `Date` ~ px(115),              # Date column width
        `Weekday` ~ px(100),           # Weekday column width
        `All-cause` ~ px(180),         # All-cause column width (wider for combined values)
        `Cardiovascular` ~ px(180),    # Cardiovascular column width
        `Respiratory` ~ px(180),       # Respiratory column width
        `Injury` ~ px(180),            # Injury column width
        `Neuropsychiatric` ~ px(185)   # Neuropsychiatric column width
      ) %>% 

      # left justify date
      cols_align(
        align = "left"
      ) %>% 
      
      # enable HTML formatting for the data columns
      fmt_markdown(columns = c(`All-cause`, Cardiovascular, Injury, Neuropsychiatric, Respiratory)) %>% 
      
      # use a theme! set up the table style
      opt_row_striping() %>%
      tab_options(
        heading.background.color = okeefe[1],
        heading.title.font.size = 18,
        heading.padding = px(10), 
        table.font.size = 14,
        column_labels.background.color = okeefe[3],
        column_labels.font.weight = "bold",
        table.border.top.style = "hidden",
        table.border.bottom.style = "hidden")
    
    # this is something webshot needs to work...
    options(chromote.headless = "new")

    # save the table as html using cat 
     pretty_table %>% 
      as_raw_html() %>% 
      cat(file = filename_html)
    
    # save the table as png
      # doing it this way becuase i couldnt get the dpi high enough with gtsave
webshot2::webshot(
  url = filename_html,
  file = filename,
  zoom = 7,
  selector = "table",
  vwidth = 1400,
  delay = 2
)
  }
}