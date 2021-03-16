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
    
    # Place of Birth by Educational Attainment
    "B06009_001E", # Estimate!!Total:	
    "B06009_002E", # Estimate!!Total:!!Less than high school graduate
    "B06009_004E", # Estimate!!Total:!!Some college or associate's degree
    
    # Ratio of Income to Poverty Level in the past 12 months
    "B05010_001E", # Estimate!!Total:	
    "B05010_018E", # Estimate!!Total:!!2.00 and over: (ratio of income to poverty level)
    
    # Household Size by Vehicles Available
    "B08201_001E", # Estimate!!Total:	
    "B08201_002E", # Estimate!!Total:!!No vehicle available	
    
    # Presence and Types of Internet Subscriptions in Household
    "B28002_001E", # Estimate!!Total:	
    "B28002_004E" #Estimate!!Total:!!With an Internet subscription!!Broadband of any type	
  ),
  year = 2019,
  output = "wide"
)

acs_subject_data <- get_acs(
  "zcta",
  survey = "acs5/subject",
  year = 2019,
  output = "wide",
  variables = c(
    # Employment Status
    "S2301_C04_001E", # Estimate!!Unemployment rate!!Population 16 years and over	
    
    # Poverty Status in the Past 12 Months
    "S1701_C01_001E", # Estimate!!Total!!Population for whom poverty status is determined
    "S1701_C01_042E" # S1701_C01_042E	Estimate!!Total!!Population for whom poverty status is determined!!ALL INDIVIDUALS WITH INCOME BELOW THE FOLLOWING POVERTY RATIOS!!200 percent of poverty level
  )
)
# remove margin columns
acs_data <- as.data.frame(acs_data)
acs_data <- acs_data[, c(T,T,!grepl("M", colnames(acs_data)[-c(1:2)]))]

# write out ----

write.csv(acs_data, file = file.path("..", "ACS_API_Data.csv"), row.names = F)
