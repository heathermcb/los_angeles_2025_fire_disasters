## ---------------------------
## LBW
## created: 2025-05-14
## last updated: 2025-05-14
## goal: process new results to be the same as the last results
## ---------------------------

## NOTE: old and new labels were reversed in the original csv! so intentionally reversed here!

# setup ---------------------------
# install and load packages
if(!requireNamespace('pacman', quietly = TRUE)) install.packages('pacman') 
pacman::p_load(folders, readr, readxl, snakecase, lubridate, 
               dplyr, tidyr, stringr, forcats, cowplot,
               ggplot2, patchwork, gridExtra, ggtext)

# project specific paths
rootdir_personal <- paste0("~/Desktop/Desktop/epidemiology_PhD/00_repos/")
rootdir_prj <- paste0(rootdir_personal, "LA25_fires_edvisits/results/")
rootdir_sp <- paste0("/Users/laurenwilner/Library/CloudStorage/OneDrive-SharedLibraries-UW/casey_cohort\ -\ Documents/")
rootdir_sp_studies <-  paste0(rootdir_sp, "studies/")
rootdir_sp_kaiser_la <- paste0(rootdir_sp_studies, "kaiser_la/")

# process the comparison results ---------------------------
results <- read_csv(paste0(rootdir_sp_kaiser_la, "results_comparison_5925.csv")) %>%
    select(-c(contains("new"), "...1")) %>%
    rename_with(~str_remove(., "_old"), contains("_old")) %>%
    mutate(
        expected = as.numeric(str_extract(expected_CI, "-?\\d+\\.?\\d*(?=\\s\\()")),
        expected_low = as.numeric(str_extract(expected_CI, "(?<=\\()\\s*-?\\d+\\.?\\d*")),
        expected_up = as.numeric(str_extract(expected_CI, "(?<=,)\\s*-?\\d+\\.?\\d*(?=\\s*\\))")),
        
        excess = as.numeric(str_extract(excess_CI, "-?\\d+\\.?\\d*(?=\\s\\()")),
        excess_low = as.numeric(str_extract(excess_CI, "(?<=\\()\\s*-?\\d+\\.?\\d*")),
        excess_up = as.numeric(str_extract(excess_CI, "(?<=,)\\s*-?\\d+\\.?\\d*(?=\\s*\\))")),
        
        excess_pct = as.numeric(str_extract(excess_pct_CI, "-?\\d+\\.?\\d*(?=\\s\\()")),
        excess_low_pct = as.numeric(str_extract(excess_pct_CI, "(?<=\\()\\s*-?\\d+\\.?\\d*")),
        excess_up_pct = as.numeric(str_extract(excess_pct_CI, "(?<=,)\\s*-?\\d+\\.?\\d*(?=\\s*\\))"))
    )
write.csv(results, paste0(rootdir_sp_kaiser_la, "results_9may2025.csv"))

# old results
results_old <- read_csv(paste0(rootdir_sp_kaiser_la, "results_comparison_5925.csv")) %>%
    select(-c(contains("old"), "...1")) %>%
    rename_with(~str_remove(., "_new"), contains("_new")) %>%
    mutate(
        # Updated to handle negative values and decimals
        expected = as.numeric(str_extract(expected_CI, "-?\\d+\\.?\\d*(?=\\s\\()")),
        expected_low = as.numeric(str_extract(expected_CI, "(?<=\\()\\s*-?\\d+\\.?\\d*")),
        expected_up = as.numeric(str_extract(expected_CI, "(?<=,)\\s*-?\\d+\\.?\\d*(?=\\s*\\))")),
        
        # Updated to handle negative values and decimals
        excess = as.numeric(str_extract(excess_CI, "-?\\d+\\.?\\d*(?=\\s\\()")),
        excess_low = as.numeric(str_extract(excess_CI, "(?<=\\()\\s*-?\\d+\\.?\\d*")),
        excess_up = as.numeric(str_extract(excess_CI, "(?<=,)\\s*-?\\d+\\.?\\d*(?=\\s*\\))")),
        
        # Updated to handle negative values and decimals
        excess_pct = as.numeric(str_extract(excess_pct_CI, "-?\\d+\\.?\\d*(?=\\s\\()")),
        excess_low_pct = as.numeric(str_extract(excess_pct_CI, "(?<=\\()\\s*-?\\d+\\.?\\d*")),
        excess_up_pct = as.numeric(str_extract(excess_pct_CI, "(?<=,)\\s*-?\\d+\\.?\\d*(?=\\s*\\))"))
    )
write.csv(results_old, paste0(rootdir_sp_kaiser_la, "results_OLD_9may2025.csv"))