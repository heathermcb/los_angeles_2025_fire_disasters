# los_angeles_2025_fire_disasters

## The 2025 Los Angeles Wildfires and Short-Term Outpatient Acute Healthcare Utilization
Authors: Joan A. Casey,  Yuqian M. Gu, Lara Schwarz, Timothy B. Frankland, Lauren B. Wilner, Heather McBrien, Nina M. Flores, Arnab K. Dey, Gina
S. Lee, Chen Chen,  Tarik Benmarhnia, Sara Y. Tartof

This repository contains scripts used to analyze the impact of the Los Angeles wildfires using a two-stage interrupted time series design. The following sections describe data sources and scripts used for the analysis. The codes have been adapted from the following study:

              Two-Stage Interrupted Time Series Analysis with Machine Learning: Evaluating the Health Effects of the 2018 Wildfire Smoke Event in San Francisco County as a Case Study
              Authors: Arnab K. Dey, Yiqun Ma, Gabriel Carrasco-Escobar, Changwoo Han, François Rerolle, Tarik Benmarhnia

## Data Sources
Data from this analysis comes from multiple sources:
-Wildfire boundaries from CALFIRE (CALFIRE. Current Emergency Incidents. https://www.fire.ca.gov/Incidents. Published 425 2025. Accessed 19 Jan 2025)
-Daily virtual and outpatient visits from Kaiser Permanente for five disease categories using International Classification of Diseases, Tenth Revision codes (all-cause, cardiovascular [I00-I99], injury [S00-T88], neuropsychiatric [F01-F99], and respiratory [J00-J99]). We aggregated visits by type, cause, day, and wildfire exposure category.
-Daily maximum and minimum temperature and humidity, wind velocity, and surface downward shortwave radiation from gridMET (
-Daily weekly wastewater surveillance data on flu, respiratory syncytial virus (RSV), and SARS-CoV-2. (Los Angeles County Department of Public Health. RESPWatch: Respiratory Illness Surveillance. http://ph.lacounty.gov/acd/respwatch/#Viruses. Accessed 02/12/2025.)

## Data Dictionary

This section describes the variables present in the datasets used in the study. We use two datasets df-train-test-sf.csv and df-predict.csv. These datasets correspond to the pre-event period and the entire study period including the post-event period.

-df-train-test-sf.csv is used to train the models, to perform crossvalidation, and to evaluate model performance.
-df-predict.csv is used to predict hospitalizations under the counterfactual scenario.

## Data Analysis Scripts

# Prep Script
1.0-ITS-prep.R
This scripts prepares the data for the modelling.

# Modelling Scripts

1.1-model-tune-phyxgb.R 
This script configures, tunes, and fits a Prophet-XGBoost model to the aggregated data. It combines Facebook Prophet for trend/seasonality with XGBoost for residuals, using a space-filling grid of 100 combinations.

2.1-model-error-metrics.R
This script loads the modeltime table and calculates training and testing errors. 

# Output generation scripts

3.1-func-generate-MC-CIs.R
This script contains functions to generate confidence intervals for time series forecasts.

3.2-model-forecast.R
This script fits the model from the selected method to the entire data (including post wildfire data) and generates forecasts for the entire dataset with bootstrapped confidence intervals.

3.3-outputs-table-excess-hosp.R
This script computes the excess hospitalizations due to the wildfire and saves as a table.

3.4-func-plot-counterfactuals.R
This script contains functions to plot counterfactual and actual trends.

3.5-outputs-plot-counterfactuals.R
This script applies the function above and saves plots
