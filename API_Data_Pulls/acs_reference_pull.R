# Pulling ACS Reference Data
# By Hannah De los Santos
# Originated on: 7/19/21

# load data and libraries ----

library(tidycensus)
library(dplyr)

# census_api_key("YOUR_KEY_HERE")

# pull data ----

# ACS 5 year 2021
acs_data <- get_acs(
  "zcta",
  variables = c(
    "B01001_001E", # Estimate!!Total: SEX BY AGE (population)
    "B01001B_001E" # SEX BY AGE (BLACK OR AFRICAN AMERICAN ALONE)
  ), 
  year = 2021,
  output = "wide"
)

# remove margin columns
acs_data <- as.data.frame(acs_data)
acs_data <- acs_data[, c(T,T,!grepl("M", colnames(acs_data)[-c(1:2)]))]

# rename columns
acs_data <- rename(acs_data, 
                   total_pop = B01001_001E,
                   total_black = B01001B_001E)

# write out data ----

data_folder <- file.path(
  gsub("\\\\","/", gsub("OneDrive - ","", Sys.getenv("OneDrive"))), 
  "Health and Social Equity - SJP - BHN Score Creation",
  "Data", "Resources")

write.csv(acs_data, file = file.path(data_folder,
                                      "ACS_ZCTA_Total_Populations.csv"), 
          row.names = F, na = "")
