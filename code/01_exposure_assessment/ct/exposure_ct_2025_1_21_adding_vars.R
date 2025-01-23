#------------------------------------------------------------------------------#
#-------------Los Angeles Wildfire poverty-exposure census tracts -------------#   
#-------------------------R code-----------------------------------------------#
#-------------------------Date:1/21/25-----------------------------------------#
#-------------------------Lara Schwarz-----------------------------------------#
#------------------------------------------------------------------------------#
## Purpose: Add poverty variable and los angeles boundary to the exposure data 

# Final dataset we want to have the following columns:
#  (1) <20km + low poverty; (2) <20km + high poverty; (3) LA not <20km + low pov; (4) LA not <20km + high pov; (5) not LA not <20 km + low pov; (6) not LA  not <20km + high pov

# load packages
if (!requireNamespace('pacman', quietly = TRUE)){install.packages('pacman')}
pacman::p_load(sf, readr, dplyr, tidycensus, tigris, tidyr, stringr)

### Data Prep
# set a variable for today's date
current_date <- format(Sys.Date(), "%Y_%m_%d")

# Set API key for census download
census_api_key("63f642da86ed1c8ca2540a16df961d517ba14a4a")

#Crosswalk for census
url <- "https://www2.census.gov/geo/docs/maps-data/data/rel2020/tract/tab20_tract20_tract10_st06.txt"

# Defining southern CA counties 
southern_ca_counties <- c("San Luis Obispo", "Kern", "Santa Barbara", "Ventura", 
                          "Los Angeles", "Orange", "San Diego", "Riverside", 
                          "San Bernardino", "Imperial")

### Loading datasets

# Load exposure data (update based on current day)
ct_exposures_2025 <- read_csv("data/02_processed/ct_exposures_2025_01_21.csv")

# Read in crosswalk from url above
census_crosswalk <- read.table(url, sep = "|", header = TRUE)


### Data Processing 

## census data processing
# Rename the `GEOID` column in ct_exposures_2025 to match `GEOID_TRACT10` in census_crosswalk
colnames(census_crosswalk)[colnames(census_crosswalk) == "GEOID_TRACT_10"] <- "GEOID10"
census_crosswalk$GEOID10<-as.character(census_crosswalk$GEOID10)
census_crosswalk$GEOID10 <- str_pad(census_crosswalk$GEOID10, width = nchar(census_crosswalk$GEOID10) + 1, side = "left", pad = "0")

# Also for GEOID_TRACT_20
census_crosswalk$GEOID_TRACT_20<-as.character(census_crosswalk$GEOID_TRACT_20)
census_crosswalk$GEOID_TRACT_20 <- str_pad(census_crosswalk$GEOID_TRACT_20, width = nchar(census_crosswalk$GEOID_TRACT_20) + 1, side = "left", pad = "0")

# Extract the ACS data [C17002 variable]
acs_data <- get_acs(
  geography = "tract",
  state = "CA", 
  county = southern_ca_counties, 
  #geometry = TRUE,
  variables = c("C17002_002", "C17002_001", "C17002_003"),  
  # C17002_001- Total: Ratio of Income to Poverty Level in the Past 12 Months
  # C17002_002- Under .50 Ratio of Income to Poverty Level in the Past 12 Months
  # C17002_003- .50 to .99 Ratio of Income to Poverty Level in the Past 12 Months
  year = 2023 ,
  survey = "acs5"
)

# Create a dataset where each variables is a row
acs_data_wide <- acs_data %>%
  pivot_wider(
    id_cols = c(GEOID, NAME),  # Make sure each row is a GEOID-NAME combination
    names_from = variable,     # Create a separate column for each variable
    values_from = estimate     # Fill the new columns with estimates
  )

# percentage of population for which the ratio of income to poverty is under 1 
acs_data_wide <- acs_data_wide %>%
  mutate(
    pct_below_poverty = (C17002_002 + C17002_003) / C17002_001) 

## Exposure data processing 
# keep the 20km buffer as main exposure
ct_exposures_2025 <- ct_exposures_2025 %>%
  select(GEOID10, exposed_20buffer)

## Data Compilation
# Poverty classification
# High poverty census tracts were those in which ≥15% of the population was living below the federal poverty threshold. 
# Tracts were otherwise classified as “low poverty.” 

# Keep only the GEOID_TRACT20 column from the merged data
ct_exposures_2025 <- merge(ct_exposures_2025, census_crosswalk[, c("GEOID10", "GEOID_TRACT_20")], 
                     by = "GEOID10", all.x=TRUE)

# Merge in the poverty data from census
ct_exposures_2025_poverty <- merge(ct_exposures_2025, 
                    acs_data_wide[, c("GEOID", "pct_below_poverty")], 
                     by.x = "GEOID_TRACT_20",  # Column in ct_exposures_2025
                     by.y = "GEOID", all=TRUE)   # Column in census_poverty_data

# If some 2010 census boundaries correspond to multiple 2020 CTs,  take average
ct_exposures_2025_poverty <- ct_exposures_2025_poverty %>%
  select(-GEOID_TRACT_20) %>%  # Remove GEOID_TRACT_20
  group_by(GEOID10) %>%
  summarise(across(everything(), ~mean(. , na.rm = TRUE)))

# Poverty binary variable (based on 15%)
ct_exposures_2025_poverty <- ct_exposures_2025_poverty %>%
  mutate(poverty = ifelse(pct_below_poverty >= 0.15, 1, 0))

## Making the exposure data
# exposure variable for census tracts in Los Angeles County and not in a wildfire burn zone (1=yes, 0=no)
ct_exposures_2025_poverty$exposed_la <- ifelse(ct_exposures_2025_poverty$exposed_20buffer == 1, 0, 
                                             ifelse(substr(ct_exposures_2025_poverty$GEOID10, 3, 5) == "037", 1, 0))

# exposure variable for census tracts outside of wildfire burn area and outside of Los Angeles County (1=yes, 0=no)
ct_exposures_2025_poverty$least_exposed <- ifelse(ct_exposures_2025_poverty$exposed_20buffer == 1 | ct_exposures_2025_poverty$exposed_la == 1, 0, 1)

# Create exposure variables based on whether the tract is in LA County, wildfire zone, and poverty level
ct_exposures_2025_exposure_data <- ct_exposures_2025_poverty %>%
  mutate(
    # High exposure variable: in wildfire buffer zone, including both high and low poverty levels
    high_exposure = ifelse(exposed_20buffer == 1, 1, 0),
    
    # Less exposure variable: outside wildfire buffer zone but in LA County, including both high and low poverty levels
    less_exposure = ifelse(exposed_la == 1, 1, 0),
    
    # Least exposure variable: outside both wildfire buffer and LA County, including both high and low poverty levels
    least_exposure = ifelse(least_exposed == 1, 1, 0),
    
    # High exposure with high poverty
    high_exposure_high_poverty = ifelse(high_exposure == 1 & poverty == 1, 1, 0),
    
    # High exposure with low poverty
    high_exposure_low_poverty = ifelse(high_exposure == 1 & poverty == 0, 1, 0),
    
    # Less exposure with high poverty
    less_exposure_high_poverty = ifelse(less_exposure == 1 & poverty == 1, 1, 0),
    
    # Less exposure with low poverty
    less_exposure_low_poverty = ifelse(less_exposure == 1 & poverty == 0, 1, 0),
    
    # Least exposure with high poverty
    least_exposure_high_poverty = ifelse(least_exposure == 1 & poverty == 1, 1, 0),
    
    # Least exposure with low poverty
    least_exposure_low_poverty = ifelse(least_exposure == 1 & poverty == 0, 1, 0)
  )

# Keep only the specified variables in the dataset
ct_exposures_2025_exposure_data <- ct_exposures_2025_exposure_data %>%
  select(GEOID10,
         high_exposure,
         high_exposure_high_poverty,
         high_exposure_low_poverty,
         less_exposure,
         less_exposure_high_poverty,
         less_exposure_low_poverty,
         least_exposure,
         least_exposure_high_poverty,
         least_exposure_low_poverty)

# Reshape data to have two columns: one for exposure and one for exposure_poverty
# Create the exposure and poverty_exposure variables
ct_exposures_2025_exposure_data_long <- ct_exposures_2025_exposure_data %>%
  mutate(
    exposure = case_when(
      high_exposure == 1 ~ 2, 
      less_exposure == 1 ~ 1, 
      least_exposure == 1 ~ 0,
      TRUE ~ NA_real_  # if no exposure, leave as NA
    ),
    
    # Combine the exposure and poverty categories into `poverty_exposure`
    poverty = case_when(
      high_exposure_high_poverty == 1 ~ 1,
       high_exposure_low_poverty == 1 ~ 0,
      less_exposure_high_poverty == 1 ~ 1,
       less_exposure_low_poverty == 1 ~ 0,
       least_exposure_high_poverty == 1 ~ 1,
       least_exposure_low_poverty == 1 ~ 0,
      TRUE ~ NA_real_  # If no match, we leave it as NA
    ),
    # Create the poverty_exposure variable based on the combination
    exp_pov = case_when(
      high_exposure_high_poverty == 1 ~ 5,   # high exposure, high poverty
      high_exposure_low_poverty == 1 ~ 4,    # high exposure, low poverty
      less_exposure_high_poverty == 1 ~ 3,   # less exposure, high poverty
      less_exposure_low_poverty == 1 ~ 2,    # less exposure, low poverty
      least_exposure_high_poverty == 1 ~ 1,  # least exposure, high poverty
      least_exposure_low_poverty == 1 ~ 0,   # least exposure, low poverty
      TRUE ~ NA_real_  # If no match, we leave it as NA
    )
  ) %>%
  # Keep only the necessary columns: GEOID10, exposure, and poverty_exposure
  select(GEOID10, exposure, poverty, exp_pov)

## Exposure dataset summary and export
# Make a table of total counts
count_table <- ct_exposures_2025_exposure_data %>%
  summarise(
    high_exposure_count = sum(high_exposure == 1, na.rm = TRUE),
    high_exposure_high_poverty_count = sum(high_exposure_high_poverty == 1, na.rm = TRUE),
    high_exposure_low_poverty_count = sum(high_exposure_low_poverty == 1, na.rm = TRUE),
    less_exposure_count = sum(less_exposure == 1, na.rm = TRUE),
    less_exposure_high_poverty_count = sum(less_exposure_high_poverty == 1, na.rm = TRUE),
    less_exposure_low_poverty_count = sum(less_exposure_low_poverty == 1, na.rm = TRUE),
    least_exposure_count = sum(least_exposure == 1, na.rm = TRUE),
    least_exposure_high_poverty_count = sum(least_exposure_high_poverty == 1, na.rm = TRUE),
    least_exposure_low_poverty_count = sum(least_exposure_low_poverty == 1, na.rm = TRUE),
  )

#making the table more readable
count_table_long <- count_table %>%
  pivot_longer(cols = everything(), 
               names_to = "Exposure_Category", 
               values_to = "Count")

#Export table of counts to csv
# Construct the file name with the current date
file_name <- paste0("data/02_processed/ct_exposed_poverty_", current_date, "_counts.csv")
# Save the data frame as a CSV file with the dynamically generated file name
write.csv(count_table_long, file_name)

# Export exposure dataset
# Construct the file name with the current date
file_name <- paste0("data/02_processed/ct_exposed_poverty_num_", current_date, ".csv")
# Save the data frame as a CSV file with the dynamically generated file name
write.csv(ct_exposures_2025_exposure_data_long, file_name)