## ---------------------------
## EMB
## created: 2025-02-10
## last updated: 2025-02-25
## goal: rough diff and diff
##       calcs + tables 
## ---------------------------
## notes:
##   we are grouping by exp 
##   and exp_pov in the same script
##   both final csvs include raw count 
##   and rate calcs 
## ---------------------------

# setup ---------------------------

# install and load packages
if(!requireNamespace('pacman', quietly = TRUE)) install.packages('pacman') 
pacman::p_load(folders, readr, readxl, snakecase, lubridate, dplyr, tidyr, stringr, forcats)

# project specific paths
rootdir_personal <- paste0("~/Library/Mobile Documents/com~apple~CloudDocs/Documents/bow_down/projects/")
rootdir_prj <- paste0(rootdir_personal, "wildfires/kaiser_la_fires/data/")
rootdir_sp <- paste0("/Users/emblake/Library/CloudStorage/OneDrive-SharedLibraries-UW/casey_cohort\ -\ Documents/")
rootdir_sp_studies <-  paste0(rootdir_sp, "studies/")
rootdir_sp_kaiser_la <- paste0(rootdir_sp_studies, "kaiser_la/")

# read in ed visits csv ---------------------------
edv <- read_csv(paste0(rootdir_prj, "ENC_EXP_DAILY_02102025_updated.csv")) %>% 
  select(!c("...10", "...11", "...12", "...13", "...14")) %>% 
  filter(exp_pov != 99) %>% 
  # convert to date format
  mutate(encounter_dt = mdy(encounter_dt)) %>% 
        # create word factor levels for exp, ex_pov, and pov
  mutate(exp = case_when(exp_pov %in% c(4,5) ~ "highly",
                              exp_pov %in% c(2,3) ~ "moderately",
                              exp_pov %in% c(0,1) ~ "minimally"),
         pov = case_when(exp_pov %in% c(1, 3, 5) ~ "high",
                             exp_pov %in% c(0, 2, 4) ~ "low"),
         exp_pov = case_when(exp_pov == 0 ~ "low-minimally",
                             exp_pov == 1 ~ "high-minimally",
                             exp_pov == 2 ~ "low-moderately",
                             exp_pov == 3 ~ "high-moderately",
                             exp_pov == 4 ~ "low-highly",
                             exp_pov == 5 ~ "high-highly"),
         # calculate rates for each encounter type
         rate_enc = (num_enc/denom)*10000,
         rate_cardio = (num_enc_cardio/denom)*10000,
         rate_resp = (num_enc_resp/denom)*10000,
         rate_neuro = (num_enc_neuro/denom)*10000,
         rate_injury = (num_enc_injury/denom)*10000)

# filter by dates to calculate totals ---------------------------
# 2025
# fire period (1/7/2025 - 1/13/2025)
fire_25 <- edv %>% 
                filter(encounter_dt >= as.Date("2025-01-07") & 
                          encounter_dt <= as.Date("2025-01-13"))

# pre-fire period (12/31/2024 - 1/6/2025)
prefire_25 <- edv %>%
  filter(encounter_dt >= as.Date("2024-12-31") & 
           encounter_dt <= as.Date("2025-01-06"))

# 2024
# fire period (1/7/2024 - 1/13/2024)
fire_24 <- edv %>% 
  filter(encounter_dt >= as.Date("2024-01-07") & 
           encounter_dt <= as.Date("2024-01-13"))

# pre-fire period (12/31/2023 - 1/6/2024)
prefire_24 <- edv %>%
  filter(encounter_dt >= as.Date("2023-12-31") & 
           encounter_dt <= as.Date("2024-01-06"))

# 2023
# fire period (1/7/203 - 1/13/2023)
fire_23 <- edv %>% 
  filter(encounter_dt >= as.Date("2023-01-07") & 
           encounter_dt <= as.Date("2023-01-13"))

# pre-fire period (12/31/2022 - 1/6/2023)
prefire_23 <- edv %>%
  filter(encounter_dt >= as.Date("2022-12-31") & 
           encounter_dt <= as.Date("2023-01-06"))


# summarize visit numbers ---------------------------
summing_visits <- function(data) {
  
  # pull data name to use for naming
  data_name <- deparse(substitute(data))
  
  # function to perform the summing
  summarize_encounters <- function(grouped_data) {
    grouped_data %>%
      summarise(
        total_num_enc = sum(num_enc),
        total_num_cardio = sum(num_enc_cardio),
        total_num_resp = sum(num_enc_resp),
        total_num_neuro = sum(num_enc_neuro),
        total_num_injury = sum(num_enc_injury),
        total_rate_enc = sum(rate_enc),
        total_rate_cardio = sum(rate_cardio),
        total_rate_resp = sum(rate_resp),
        total_rate_neuro = sum(rate_neuro),
        total_rate_injury = sum(rate_injury)
      )
  }
  
  # create df grouped by exp and apply summarize function
  exp_data <- data %>%
    group_by(exp, enc_type) %>%
    summarize_encounters()
  
  # create df grouped by exp_pov and apply summarize function 
  exppov_data <- data %>%
    group_by(exp_pov, enc_type) %>%
    summarize_encounters()
  
  # assign results to environment with appropriate names
  assign(paste0("exp_", data_name), exp_data, envir = .GlobalEnv)
  assign(paste0("exppov_", data_name), exppov_data, envir = .GlobalEnv)
}

# use above function on all wf period datasets 
summing_visits(fire_25)
summing_visits(prefire_25)
summing_visits(fire_24)
summing_visits(prefire_24)
summing_visits(fire_23)
summing_visits(prefire_23)

# average 23 and 24 ed counts pre and fire periods ---------------------------
avg_23_24 <- function(df_24, df_23, variable_name) {
  
  # pull variable to join based on
  variable_str <- deparse(substitute(variable_name))
  
  # create a dynamic join condition
  join_by <- c(variable_str, "enc_type")
  
  avg_df <- df_24 %>%
    # join 2024 and 2023 with unique identifier
    inner_join(df_23, by = join_by, suffix = c("_23", "_24")) %>%
    # average for all encounter types (num and rates)
    mutate(
      avg_num_enc = (total_num_enc_24 + total_num_enc_23) / 2,
      avg_num_cardio = (total_num_cardio_24 + total_num_cardio_23) / 2,
      avg_num_resp = (total_num_resp_24 + total_num_resp_23) / 2,
      avg_num_neuro = (total_num_neuro_24 + total_num_neuro_23) / 2,
      avg_num_injury = (total_num_injury_24 + total_num_injury_23) / 2,
      avg_rate_enc = (total_rate_enc_24 + total_rate_enc_23) / 2,
      avg_rate_cardio = (total_rate_cardio_24 + total_rate_cardio_23) / 2,
      avg_rate_resp = (total_rate_resp_24 + total_rate_resp_23) / 2,
      avg_rate_neuro = (total_rate_neuro_24 + total_rate_neuro_23) / 2,
      avg_rate_injury = (total_rate_injury_24 + total_rate_injury_23) / 2
    ) %>%
    
    # select columns
    select(all_of(variable_str), enc_type, starts_with("avg_"))

}

# use above function to calculate avg for 2023 and 2024 seasons
  # fire season 
exp_fire_23_24 <- avg_23_24(exp_fire_24, exp_fire_23, exp)
exppov_fire_23_24 <- avg_23_24(exppov_fire_24, exppov_fire_23, exp_pov)
  # prefire season
exp_prefire_23_24 <- avg_23_24(exp_prefire_24, exp_prefire_23, exp)
exppov_prefire_23_24 <- avg_23_24(exppov_prefire_24, exppov_prefire_23, exp_pov)


# subtract 2025 - avg(2023,2024)  ---------------------------
subbing_wi_szntype <- function(df_25, df_23_24, variable_name) {
  
  # pull variable to join based on
  variable_str <- deparse(substitute(variable_name))
  
  # join condition
  join_by <- c(variable_str, "enc_type")
  
  season <- df_25 %>%
    inner_join(df_23_24, by = join_by) %>%
    mutate(
      num_enc = total_num_enc - avg_num_enc,
      num_cardio = total_num_cardio - avg_num_cardio,
      num_resp = total_num_resp - avg_num_resp,
      num_neuro = total_num_neuro - avg_num_neuro,
      num_injury = total_num_injury - avg_num_injury,
      rate_enc = total_rate_enc - avg_rate_enc,
      rate_cardio = total_rate_cardio - avg_rate_cardio,
      rate_resp = total_rate_resp - avg_rate_resp,
      rate_neuro = total_rate_neuro - avg_rate_neuro,
      rate_injury = total_rate_injury - avg_rate_injury,
    ) %>% 
    
    select(all_of(variable_str), enc_type, starts_with(c("num_", "rate_")))
  
}

# use above function 
  # fire season 
exp_fire_season <- subbing_wi_szntype(exp_fire_25, 
                                      exp_fire_23_24, 
                                      exp)
exppov_fire_season <- subbing_wi_szntype(exppov_fire_25, 
                                         exppov_fire_23_24, 
                                         exp_pov)
  # prefire season
exp_prefire_season <- subbing_wi_szntype(exp_prefire_25, 
                                         exp_prefire_23_24, 
                                         exp)
exppov_prefire_season <- subbing_wi_szntype(exppov_prefire_25, 
                                            exppov_prefire_23_24, 
                                            exp_pov)


# subtract: fire season visits - prefire season visits ---------------------------
subbing_btwn_szntype <- function(fire_season, prefire_season, variable_name) {

# pull variable to join based on
variable_str <- deparse(substitute(variable_name))
  
# join condition
join_by <- c(variable_str, "enc_type")

diffs <- fire_season %>%
  inner_join(prefire_season, by = join_by, suffix = c("_fire", "_prefire")) %>%
  mutate(
    diff_num_enc = num_enc_fire - num_enc_prefire,
    diff_num_cardio = num_cardio_fire - num_cardio_prefire,
    diff_num_resp = num_resp_fire - num_resp_prefire,
    diff_num_neuro = num_neuro_fire - num_neuro_prefire,
    diff_num_injury = num_injury_fire - num_injury_prefire,
    diff_rate_enc = rate_enc_fire - rate_enc_prefire,
    diff_rate_cardio = rate_cardio_fire - rate_cardio_prefire,
    diff_rate_resp = rate_resp_fire - rate_resp_prefire,
    diff_rate_neuro = rate_neuro_fire - rate_neuro_prefire,
    diff_rate_injury = rate_injury_fire - rate_injury_prefire
  ) %>% 
  select(variable_str, enc_type, starts_with("diff_"))

  return(diffs)
}

# use above function and save csvs 
exp_diffs <- subbing_btwn_szntype(exp_fire_season, 
                                  exp_prefire_season, 
                                  exp)
exppov_diffs <- subbing_btwn_szntype(exppov_fire_season, 
                                     exppov_prefire_season, 
                                     exp_pov)


write_csv(exp_diffs, paste0(rootdir_sp_kaiser_la, "output/exp_difftable.csv"))
write_csv(exppov_diffs, paste0(rootdir_sp_kaiser_la, "output/exppov_difftable.csv"))
