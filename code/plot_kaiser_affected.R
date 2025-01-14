# Plot zctas and kaiser members affected


# Libraries ---------------------------------------------------------------

pacman::p_load(here, tidyverse, sf, arrow, sfarrow)


# Read --------------------------------------------------------------------

k_af <- read_csv(here("data", "num_kaiser_pop_age_60_affected_la_wf.csv"))

zctas <- read_parquet(here("data", "zctas_2020.parquet"))

fires <- read_parquet(here("data", "sep_fires.parquet"))

fires <- as.data.frame(fires)
fires <- st_geometry(fires$geometry)

fires %>% ggplot() + geom_sf()
