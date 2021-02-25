# Pulling Raw ACS Data
# By Hannah De los Santos, Karen Jiang
# Originated on: 2/24/21

# load data and libraries ----

library(tidycensus)

# census_api_key("YOUR_KEY_HERE")

# pull data ----

# ACS 5 year 2019
acs_data <- get_acs(
  "zcta",
  variables = c(
    "B10058_002E", # Estimate!!Total:!!In labor force:
    "B10058_007E", # Estimate!!Total:!!Not in labor force:
    "B06009_002E", # Estimate!!Total:!!Less than high school graduate
    "B06009_004E", # Estimate!!Total:!!Some college or associate's degree
    "B05010_018E" # Estimate!!Total:!!2.00 and over: (ratio of income to poverty level)
  ),
  year = 2019,
  output = "wide"
)

# remove margin columns
acs_data <- as.data.frame(acs_data)
acs_data <- acs_data[, c(T,T,!grepl("M", colnames(acs_data)[-c(1:2)]))]

# write out ----

write.csv(acs_data, file = file.path("..", "ACS_API_Data.csv"), row.names = F)
