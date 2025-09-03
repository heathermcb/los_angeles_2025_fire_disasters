## ---------------------------
## EMB & LBW - COMPARISON VERSION
## created: 2025-02-20
## last updated: 27 Feb 25
## goal: produce plots visualizing 
##       excess ed visits (raw, per 1000, and percent)
##       comparing Aug 6 vs May 9 results for HIGHLY exposed only
## breakdown: 
##       2 visit types (virtual versus outpatient (op))
##       5 outcomes/encounter types (overall, cardio, resp, neuro, injury)
##       highly exposed only, comparing two time periods
## ---------------------------

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

# read in both datasets ---------------------------
# Aug 6 results (already highly exposed only)
results_aug6 <- read_csv(paste0(rootdir_sp_kaiser_la, "results_6aug2025.csv")) %>% 
  filter(date < "2025-01-11") %>%   # filter out post jan 11 to make just weekday plot
  mutate(dataset_name = str_remove(dataset_name, "^df_")) %>%
  separate(dataset_name, into = c("visit_type", "exposure"), sep = "_", remove = FALSE) %>% 
  mutate_at(c("exposure", "visit_type", "encounter_type"), as.factor) %>% 
  mutate(visit_encounter = paste0(visit_type, "_", encounter_type)) %>% 
  mutate(exposure = ifelse(exposure == "moderate", "Moderately", "Highly"),
         date = as.Date(date, format = "%m/%d/%y"),
         dataset = "10km (sensitivity analysis)")

# May 9 results
results_may9 <- read_csv(paste0(rootdir_sp_kaiser_la, "results_9may2025.csv")) %>% 
  filter(date < "2025-01-11") %>%   # filter out post jan 11 to make just weekday plot
  mutate(dataset_name = str_remove(dataset_name, "^df_")) %>%
  separate(dataset_name, into = c("visit_type", "exposure"), sep = "_", remove = FALSE) %>% 
  mutate_at(c("exposure", "visit_type", "encounter_type"), as.factor) %>% 
  mutate(visit_encounter = paste0(visit_type, "_", encounter_type)) %>% 
  mutate(exposure = ifelse(exposure == "moderate", "Moderately", "Highly"),
         date = as.Date(date, format = "%m/%d/%y"),
         dataset = "20km (primary analysis)") %>%
  filter(exposure == "Highly")  # Filter to highly exposed only for May 9

# Combine datasets
results <- bind_rows(results_aug6, results_may9)

# helper function ---------------------------
separating_cis <- function(df, col, prefix) { 
  df %>% 
    separate({{col}}, 
             into = c(paste0(prefix, "_estimate"), paste0(prefix, "_ci_bounds")),
             sep = "\\s(?=\\()",  
             extra = "merge",
             remove = FALSE) %>%
    separate(paste0(prefix, "_ci_bounds"),
             into = c(paste0(prefix, "_lci"), paste0(prefix, "_uci")),
             sep = ",",
             remove = TRUE) %>%
    mutate(
      !!sym(paste0(prefix, "_lci")) := str_remove_all(!!sym(paste0(prefix, "_lci")), "[\\(\\)]") %>% str_trim(),
      !!sym(paste0(prefix, "_uci")) := str_remove_all(!!sym(paste0(prefix, "_uci")), "[\\(\\)]") %>% str_trim()) %>% 
    mutate_at(
      c(paste0(prefix, "_estimate"), paste0(prefix, "_lci"), paste0(prefix, "_uci")), as.numeric)
}

# plotting function ---------------------------
plot_estimates_comparison <- function(visit, encounter, prefix) {
  # color pal - using different colors for the two datasets
  comparison_colors <- c("10km (sensitivity analysis)" = "#3498db", "20km (primary analysis)" = "#d37750")  # Orange-red for Aug 6, Blue for May 9

  # define human-friendly labels for encounter types
  encounter_labels <- case_when(
    encounter == "num_enc_cardio" ~ "Cardiovascular",
    encounter == "num_enc_injury" ~ "Injury",
    encounter == "num_enc_resp" ~ "Respiratory",
    encounter == "num_enc_neuro" ~ "Neuropsychiatric",
    encounter == "num_enc" ~ "All-cause",  # general case for num_enc
    TRUE ~ encounter  
  )
  
  # assign plot title based on encounter and visit type
  if(encounter %in% c("num_enc") & visit == "OP") {
    plot_title <- "A                              Outpatient"
  } else if (encounter %in% c("num_enc") & visit == "Virtual") {
     plot_title <- "B                                   Virtual"
  } else {
    plot_title <- ""
  }
    
  # Assign y-axis label based on prefix.
  y_axis_label <- case_when(
    grepl("per", prefix) ~ paste0(encounter_labels, "\n \nExcess visits\n per 1000"),
    grepl("pct", prefix) ~ paste0(encounter_labels, "\n \nPercent excess\n visits"),
    TRUE ~ paste0(encounter_labels, "\n \nExcess visits")
  )

  # remove y axis label for virtual plots since we just need 1 label and op is on the left
  if (visit == "Virtual") {
    y_axis_label <- ""
  }  

  # Create main week plot
  week_plot <- results %>% 
    filter(visit_type == visit, encounter_type == encounter, exposure == "Highly") %>% 
    ggplot(aes(x = date, y = !!sym(paste0(prefix, "_estimate")), color = dataset)) +
    geom_point(position = position_dodge(width = 0.6), size = 2.75) +
    scale_color_manual(values = comparison_colors, name = "Buffer distance") + 
    geom_errorbar(aes(ymin = !!sym(paste0(prefix, "_lci")), 
                      ymax = !!sym(paste0(prefix, "_uci"))), 
                  position = position_dodge(width = 0.6), width = .2, size = 1.25) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
    theme_minimal(base_size = 22) +
    theme(plot.title = element_text(hjust = 0, size = 26, face = "bold"),
          axis.text.x = element_text(angle = 90, vjust=0.5),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank(),
          axis.line = element_line(color = "darkgrey", size=.5)) +
    scale_x_date(date_breaks = "1 day", date_labels = "%Y-%m-%d") +
    labs(x = NULL, y = y_axis_label, title = plot_title)

  # pull legend out so we have it for later and can have one shared legend for all plots
  legend <- get_legend(week_plot)

  # now take legend out of week_plot since we will add it back in later
  week_plot <- week_plot + theme(legend.position = "none")

  # return both plot and legend so we can add the legend to the end of the patchwork
  return(list(plot = week_plot, legend = legend))
}

# create and store plots as objects to patchwork ---------------------------

# first clean up CIs to prepare! 
results <- separating_cis(results, excess_CI, "excess")
results <- separating_cis(results, excess_per1000_CI, "excess_per")
results <- separating_cis(results, excess_pct_CI, "excess_pct")

# excess - virtual 
virtual_enc_plot <- plot_estimates_comparison("Virtual", "num_enc", "excess")$plot
virtual_cardio_plot <- plot_estimates_comparison("Virtual", "num_enc_cardio", "excess")$plot
virtual_injury_plot <- plot_estimates_comparison("Virtual", "num_enc_injury", "excess")$plot
virtual_resp_plot <- plot_estimates_comparison("Virtual", "num_enc_resp", "excess")$plot
virtual_neuro_plot <- plot_estimates_comparison("Virtual", "num_enc_neuro", "excess")$plot
# excess - op 
op_enc_plot <- plot_estimates_comparison("OP", "num_enc", "excess")$plot
op_cardio_plot <- plot_estimates_comparison("OP", "num_enc_cardio", "excess")$plot
op_injury_plot <- plot_estimates_comparison("OP", "num_enc_injury", "excess")$plot
op_resp_plot <- plot_estimates_comparison("OP", "num_enc_resp", "excess")$plot
op_neuro_plot <- plot_estimates_comparison("OP", "num_enc_neuro", "excess")$plot

# excess per 1000 - virtual 
per_virtual_enc_plot <- plot_estimates_comparison("Virtual", "num_enc", "excess_per")$plot
per_virtual_cardio_plot <- plot_estimates_comparison("Virtual", "num_enc_cardio", "excess_per")$plot
per_virtual_injury_plot <- plot_estimates_comparison("Virtual", "num_enc_injury", "excess_per")$plot
per_virtual_resp_plot <- plot_estimates_comparison("Virtual", "num_enc_resp", "excess_per")$plot
per_virtual_neuro_plot <- plot_estimates_comparison("Virtual", "num_enc_neuro", "excess_per")$plot
# excess per 1000 - op
per_op_enc_plot <- plot_estimates_comparison("OP", "num_enc", "excess_per")$plot
per_op_cardio_plot <- plot_estimates_comparison("OP", "num_enc_cardio", "excess_per")$plot
per_op_injury_plot <- plot_estimates_comparison("OP", "num_enc_injury", "excess_per")$plot
per_op_resp_plot <- plot_estimates_comparison("OP", "num_enc_resp", "excess_per")$plot
per_op_neuro_plot <- plot_estimates_comparison("OP", "num_enc_neuro", "excess_per")$plot

# excess pct - virtual
pct_virtual_enc_plot <- plot_estimates_comparison("Virtual", "num_enc", "excess_pct")$plot
pct_virtual_cardio_plot <- plot_estimates_comparison("Virtual", "num_enc_cardio", "excess_pct")$plot
pct_virtual_injury_plot <- plot_estimates_comparison("Virtual", "num_enc_injury", "excess_pct")$plot
pct_virtual_resp_plot <- plot_estimates_comparison("Virtual", "num_enc_resp", "excess_pct")$plot
pct_virtual_neuro_plot <- plot_estimates_comparison("Virtual", "num_enc_neuro", "excess_pct")$plot
# excess pct - op
pct_op_enc_plot <- plot_estimates_comparison("OP", "num_enc", "excess_pct")$plot
pct_op_cardio_plot <- plot_estimates_comparison("OP", "num_enc_cardio", "excess_pct")$plot
pct_op_injury_plot <- plot_estimates_comparison("OP", "num_enc_injury", "excess_pct")$plot
pct_op_resp_plot <- plot_estimates_comparison("OP", "num_enc_resp", "excess_pct")$plot
pct_op_neuro_plot <- plot_estimates_comparison("OP", "num_enc_neuro", "excess_pct")$plot

# combining with patchwork and saving final plots to sp---------------------------
## excess
op_excess <- (op_enc_plot / op_cardio_plot / op_injury_plot / op_neuro_plot / op_resp_plot)
virtual_excess <- (virtual_enc_plot / virtual_cardio_plot / virtual_injury_plot / virtual_neuro_plot / virtual_resp_plot)

full_excess <- (op_excess | virtual_excess) +
  plot_layout(guides = "collect") & 
  theme(legend.position = "bottom")

# now write it out! 
png(paste0(rootdir_sp_kaiser_la, "output/aug2025_10km/comparison_full_excess.png"), width = 20, height = 22, units = "in", res = 300)
print(full_excess)
dev.off()

## excess per 1000
virtual_per_excess <- (per_virtual_enc_plot / per_virtual_cardio_plot / per_virtual_injury_plot / per_virtual_neuro_plot / per_virtual_resp_plot)
op_per_excess <- (per_op_enc_plot / per_op_cardio_plot / per_op_injury_plot / per_op_neuro_plot / per_op_resp_plot) 

full_per_excess <- (op_per_excess | virtual_per_excess) +
  plot_layout(guides = "collect") & 
  theme(legend.position = "bottom")

png(paste0(rootdir_sp_kaiser_la, "output/aug2025_10km/comparison_full_per_excess.png"), width = 20, height = 22, units = "in", res = 300)
print(full_per_excess)
dev.off()

## excess pct 
virtual_pct_excess <- (pct_virtual_enc_plot / pct_virtual_cardio_plot / pct_virtual_injury_plot / pct_virtual_neuro_plot / pct_virtual_resp_plot)
op_pct_excess <- (pct_op_enc_plot / pct_op_cardio_plot / pct_op_injury_plot / pct_op_neuro_plot / pct_op_resp_plot)

full_pct_excess <- (op_pct_excess | virtual_pct_excess) +
  plot_layout(guides = "collect") & 
  theme(legend.position = "bottom")

png(paste0(rootdir_sp_kaiser_la, "output/aug2025_10km/comparison_full_pct_excess.png"), width = 20, height = 22, units = "in", res = 300)
print(full_pct_excess)
dev.off()