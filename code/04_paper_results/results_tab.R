results <- read_csv(paste0(rootdir_sp_kaiser_la, "results_feb27_v3.csv")) %>% 
  filter(date <= "2025-01-13") %>%   # filter out post jan 11 to make just weekday plot
  mutate(dataset_name = str_remove(dataset_name, "^df_")) %>%
  separate(dataset_name, into = c("visit_type", "exposure"), sep = "_", remove = FALSE) %>% 
  mutate_at(c("exposure", "visit_type", "encounter_type"), as.factor)  %>% 
  mutate(visit_encounter = paste0(visit_type, "_", encounter_type)) %>% 
  mutate(exposure = ifelse(exposure == "moderate", "Moderately", "Highly"),
        date = as.Date(date, format = "%m/%d/%y"))

results <- results %>% filter(visit_type == "Virtual" & encounter_type == "num_enc_cardio")
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



results <- read_csv(paste0(rootdir_prj, "results_feb27_v3.csv")) %>% 
  filter(date <= "2025-01-13") %>%   # filter out post jan 11 to make just weekday plot
  mutate(dataset_name = str_remove(dataset_name, "^df_")) %>%
  separate(dataset_name, into = c("visit_type", "exposure"), sep = "_", remove = FALSE) %>% 
  mutate_at(c("exposure", "visit_type", "encounter_type"), as.factor)  %>% 
  mutate(visit_encounter = paste0(visit_type, "_", encounter_type)) %>% 
  mutate(exposure = ifelse(exposure == "moderate", "Moderately", "Highly"),
        date = as.Date(date, format = "%m/%d/%y"))

results <- results %>% filter(visit_type == "Virtual" & encounter_type == "num_enc_resp")
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