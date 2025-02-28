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

results <- read_csv(paste0(rootdir_prj, "results_feb27_v3.csv")) %>% 
  mutate(dataset_name = str_remove(dataset_name, "^df_")) %>%
  separate(dataset_name, into = c("visit_type", "exposure"), sep = "_", remove = FALSE) %>% 
  mutate_at(c("exposure", "visit_type", "encounter_type"), as.factor)  %>% 
  mutate(visit_encounter = paste0(visit, "_", encounter_type)) %>% 
  mutate(exposure = ifelse(exposure == "moderate", "Moderately", "Highly"),
        date = as.Date(date, format = "%m/%d/%y"), 
        Weekday = weekdays(date))

count_results <- read_csv(paste0(rootdir_prj, "excess_enc_table.csv"))


okeefe <- c("#fbe3c2", "#f2c88f", "#ecb27d", "#e69c6b", "#d37750", "#b9563f", "#611F10")

# helper functions ---------------------------

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

## function to make the pretty table for paper
make_pretty_table <- function(result_table, filename) {
  
  title_label <- if(grepl("_pct", filename)){
    md("*Percent (95% eCI)*")
    } else {
    md("*n (95% eCI)*")
    }

  # make the gt table
  pretty_table <- result_table %>%
    # select(-c(title)) %>% 
    rename(Date = date) %>%
    gt() %>%
    tab_header(title = title_label) %>% 

    # set custom widths for columns
    cols_width(
      `Date` ~ px(115),              # Date column width
      `Weekday` ~ px(140),           # Weekday column width
      `All-cause` ~ px(120),         # All-cause column width (wider for the CI values)
      `Cardiovascular` ~ px(120),    # Cardiovascular column width
      `Respiratory` ~ px(120),       # Respiratory column width
      `Injury` ~ px(120),            # Injury column width
      `Neuropsychiatric` ~ px(125)   # Neuropsychiatric column width
    ) %>% 

    # left justify date
    cols_align(
      align = "left"
    ) %>% 
    
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
    zoom = 7,         # apparently this is approx 300 DPI
    selector = "table"  # only capture the table
  )

  # return table for viewing pleasure
  return(pretty_table)
}



# make the tables finally ---------------------------

## make the 8 combinations of tables 
visit_types <- c("OP", "Virtual")
exposure_levels <- c("Moderately", "Highly")
metrics <- c("", "_pct")

for(visit in visit_types) {
  for(exposure_level in exposure_levels) {
    for(metric in metrics) {
      print(paste0("Creating table for ", visit, " visits, ", exposure_level, " exposed, ", metric))
      # create the table
      result_table <- create_encounter_table(results, exposure_level, visit, metric)
      
      # make the pretty table
      filename <- paste0(rootdir_sp_kaiser_la, "output/table_", visit, "_", exposure_level, metric, ".png")
      filename_html <- paste0(rootdir_sp_kaiser_la, "output/table_", visit, "_", exposure_level, metric, ".html")
      pretty_table <- make_pretty_table(result_table, filename)
      
    }
  }
}


## make summary count table 
# this one is pretty different so just using the code above but not running the function. good enough for a same day turnaround... 
pretty_table <- count_results %>%
    rename(`Exposure group` = exposure_group, 
            `Encounter type` = encounter_type) %>%
    gt() %>%
    tab_header(title = "Estimated excess encounters") %>% 

    # set custom widths for columns
    cols_width(
      `Exposure group` ~ px(140),    # exp group column width
      `Encounter type` ~ px(140),    # encounter type column width
      `All-cause` ~ px(120),         # All-cause column width (wider for the CI values)
      `Cardiovascular` ~ px(120),    # Cardiovascular column width
      `Respiratory` ~ px(120),       # Respiratory column width
      `Injury` ~ px(120),            # Injury column width
      `Neuropsychiatric` ~ px(125)   # Neuropsychiatric column width
    ) %>% 

    # center align all columns
    cols_align(
      align = "center"
    ) %>% 
    
    # center align column labels
    tab_style(
      style = cell_text(align = "center"),
      locations = cells_column_labels()
    ) %>%
    
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

  filename_count_png <- paste0(rootdir_sp_kaiser_la, "output/table_count.png")
  filename_count_html <- paste0(rootdir_sp_kaiser_la, "output/table_count.html")
  # save the table as html using cat 
   pretty_table %>% 
    as_raw_html() %>% 
    cat(file = filename_count_html)
  
  # save the table as png
    # doing it this way becuase i couldnt get the dpi high enough with gtsave
  webshot2::webshot(
    url = filename_count_html,
    file = filename_count_png,
    zoom = 7,         # apparently this is approx 300 DPI
    selector = "table"  # only capture the table
  )

