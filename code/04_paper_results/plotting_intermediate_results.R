if(!requireNamespace('pacman', quietly = TRUE)) install.packages('pacman') 
pacman::p_load(folders, readr, readxl, snakecase, lubridate, 
               dplyr, tidyr, stringr, forcats, 
               ggplot2, patchwork, gridExtra)

# project specific paths
rootdir_personal <- paste0("~/Desktop/Desktop/epidemiology_PhD/00_repos/")
rootdir_prj <- paste0(rootdir_personal, "LA25_fires_edvisits/results/")
rootdir_sp <- paste0("/Users/laurenwilner/Library/CloudStorage/OneDrive-SharedLibraries-UW/casey_cohort\ -\ Documents/")
rootdir_sp_studies <-  paste0(rootdir_sp, "studies/")
rootdir_sp_kaiser_la <- paste0(rootdir_sp_studies, "kaiser_la/")

results <- read_csv(paste0(rootdir_prj, "results_feb26.csv")) %>% 
  filter(date <= "2025-01-13") %>% 
  mutate(dataset_name = str_remove(dataset_name, "^df_")) %>%
  separate(dataset_name, into = c("visit_type", "exposure"), sep = "_", remove = FALSE) %>% 
  mutate_at(c("exposure", "visit_type", "encounter_type"), as.factor)  %>% 
  mutate(visit_encounter = paste0(visit_type, "_", encounter_type)) %>% 
  mutate(exposure = ifelse(exposure == "moderate", "Moderately", "Highly"),
        date = as.Date(date, format = "%m/%d/%y"))
  
#   var <- "excess"
#   var_lo <- "excess_low"
#   var_hi <- "excess_up"
  
  var <- "excess_pct"
  var_lo <- "excess_low_pct"
  var_hi <- "excess_up_pct"

#   var <- "excess_per1000"
#   var_lo <- "excess_low_per1000"
#   var_hi <- "excess_up_per1000"

  encounter <- "num_enc_neuro"
  visit <- "OP"
  y_axis_label <- "Percent excess visits"
  encounter_title <- "neuro"

scale_y_symlog <- function(base = 10, ...) {
  trans <- scales::trans_new(
    name = "symlog",
    transform = function(x) sign(x) * log10(abs(x) + 1),
    inverse = function(x) sign(x) * (10^abs(x) - 1)
  )
  scale_y_continuous(trans = trans, ...)
}
results %>% 
    filter(visit_type == visit, encounter_type == encounter) %>% 
    ggplot(aes(x = date, y = !!sym(var), color = exposure)) +
    geom_point(position = position_dodge(width = 0.6), size = 2) +
    scale_color_manual(values = c(okeefe[5], okeefe[3]), name = "Exposure") + 
    geom_errorbar(aes(ymin = !!sym(var_lo), 
                      ymax = !!sym(var_hi)), 
                  position = position_dodge(width = 0.6), width = 0.2, size = 0.7) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
    theme_minimal(base_size = 16) +
    theme(legend.position = "none",
          plot.title = element_text(hjust = 0.5),
          axis.text.x = element_text(angle = 90)) +
    scale_x_date(date_breaks = "1 day", date_labels = "%Y-%m-%d") +
    # coord_cartesian(ylim = c(-300, 300)) +
    # scale_y_symlog() +
    labs(x = NULL, y = y_axis_label, title = encounter_title)
