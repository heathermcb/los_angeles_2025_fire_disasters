
# Libraries
pacman::p_load(tigris, sf, sfarrow, here)

# states 
states <- c(
  "AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "FL", "GA", "HI", "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD", 
  "MA", "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH", "NJ", "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA", "RI", "SC", 
  "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV", "WI", "WY", "DC"
)

# Initialize an empty list to store the tracts data
tracts_list <- list()

# Loop through the states to download the census tract files
# is this a silly solution to this problem?? idk man
# it downloaded fast at least 
for (state in states) {
  tracts_data <- tracts(state = state, year = 2010)
  tracts_list[[state]] <- tracts_data
}

# Combine the results into a single sf object
us_tracts_2010_sf <- do.call(rbind, tracts_list)

# write the data to a GeoParquet file
st_write_parquet(us_tracts_2010_sf, here("data", "us_tracts_2010.parquet"))
