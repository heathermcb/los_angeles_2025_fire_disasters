## ---------------------------
## LBW
## created: 2025-02-27
## last updated: 25 june 2025
## goal: produce manuscript result figures
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
    observed,
    expected,
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

results_cardio <- results %>% filter(visit_type == "Virtual" & encounter_type == "num_enc_cardio")
results_resp <- results %>% filter(visit_type == "Virtual" & encounter_type == "num_enc_resp")


## CARDIO results ---------------------------
results <- results_cardio
# total ALL excess 1/7-1/13 CARDIO virtual: 
val1 <- sum(results$excess)
paste0("The total virtual/cardio/excess (HIGH+MOD) 1/7-1/13 was ", val1)

# total MODERATE excess 1/7-1/13 CARDIO virtual: 
val2 <- sum(results$excess[results$exposure == "Moderately"])
paste0("The total virtual/cardio/excess (MOD) 1/7-1/13 was ", val2)

# total HIGH excess 1/7-1/13 CARDIO virtual: 
val3 <- sum(results$excess[results$exposure == "Highly"])
paste0("The total virtual/cardio/excess (HIGH) 1/7-1/13 was ", val3)

# total ALL excess pct 1/7-1/13 CARDIO virtual:
obs <- sum(results$observed)
exp <- sum(results$expected)
val4 <- ((obs - exp) / exp)*100
paste0("The total virtual/cardio/excess_pct (HIGH+MOD) 1/7-1/13 was ", val4, "%")

# total MODERATE excess pct 1/7-1/13 CARDIO virtual: 
obs <- sum(results$observed[results$exposure == "Moderately"])
exp <- sum(results$expected[results$exposure == "Moderately"])
val5 <- ((obs - exp) / exp)*100
paste0("The total virtual/cardio/excess_pct (MOD) 1/7-1/13 was ", val5, "%")

# total HIGH excess pct 1/7-1/13 CARDIO virtual: 
obs <- sum(results$observed[results$exposure == "Highly"])
exp <- sum(results$expected[results$exposure == "Highly"])
val6 <- ((obs - exp) / exp)*100
paste0("The total virtual/cardio/excess_pct (HIGH) 1/7-1/13 was ", val6, "%")


## RESP results ---------------------------
results <- results_resp
# total ALL excess 1/7-1/13 RESP virtual: 
val7 <- sum(results$excess)
paste0("The total virtual/resp/excess (HIGH+MOD) 1/7-1/13 was ", val7)

# total MODERATE excess 1/7-1/13 RESP virtual: 
val8 <- sum(results$excess[results$exposure == "Moderately"])
paste0("The total virtual/resp/excess (MOD) 1/7-1/13 was ", val8)

# total HIGH excess 1/7-1/13 RESP virtual: 
val9 <- sum(results$excess[results$exposure == "Highly"])
paste0("The total virtual/resp/excess (HIGH) 1/7-1/13 was ", val9)

# total ALL excess pct 1/7-1/13 RESP virtual:
obs <- sum(results$observed)
exp <- sum(results$expected)
val10 <- ((obs - exp) / exp)*100
paste0("The total virtual/resp/excess_pct (HIGH+MOD) 1/7-1/13 was ", val10, "%")

# total MODERATE excess pct 1/7-1/13 RESP virtual: 
obs <- sum(results$observed[results$exposure == "Moderately"])
exp <- sum(results$expected[results$exposure == "Moderately"])
val11 <- ((obs - exp) / exp)*100
paste0("The total virtual/resp/excess_pct (MOD) 1/7-1/13 was ", val11, "%")

# total HIGH excess pct 1/7-1/13 RESP virtual: 
obs <- sum(results$observed[results$exposure == "Highly"])
exp <- sum(results$expected[results$exposure == "Highly"])
val12 <- ((obs - exp) / exp)*100
paste0("The total virtual/resp/excess_pct (HIGH) 1/7-1/13 was ", val12, "%")