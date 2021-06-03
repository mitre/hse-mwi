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
    
    # educational attainment ----
    # Place of Birth by Educational Attainment
    "B06009_001E", # Estimate!!Total:	
    "B06009_002E", # Estimate!!Total:!!Less than high school graduate
    "B06009_004E", # Estimate!!Total:!!Some college or associate's degree
    # Sex by Educational Attainment - black
    "C15002B_001", # Estimate!!Total: (black/african american alone)
    "C15002B_003", # Estimate!!Total:!!Male:!!Less than high school diploma (black alone)
    "C15002B_008", # Estimate!!Total:!!Female:!!Less than high school diploma (black alone)
    "C15002B_005", # Estimate!!Total:!!Male:!!Some college or associate's degree (black alone)
    "C15002B_010", # Estimate!!Total:!!Female:!!Some college or associate's degree (black alone)
    
    # income below poverty level ----
    
    # Ratio of Income to Poverty Level in the past 12 months - NO RACE AVAILABLE
    # "B05010_001E", # Estimate!!Total:	
    # "B05010_018E", # Estimate!!Total:!!2.00 and over: (ratio of income to poverty level)
    
    # WHAT IS AVAILABLE: Income in the past 12 months below poverty level:
    # (calculated below)
    
    # vehicles available ----
    
    # # Household Size by Vehicles Available - NO RACE 
    # "B08201_001E", # Estimate!!Total:	
    # "B08201_002E", # Estimate!!Total:!!No vehicle available
    
    # WHAT IS AVAILABLE: MEANS OF TRANSPORTATION TO WORK (BLACK OR AFRICAN AMERICAN ALONE)
    
    # Means of transportation to work -- OWN A CAR TO USE
    "B08006_001", # 	Estimate!!Total:
    "B08006_003", # Estimate!!Total:!!Car, truck, or van:!!Drove alone
    "B08105B_001",# B08105B_001 (black or african american alone)
    "B08105B_002", # Estimate!!Total:!!Car, truck, or van - drove alone (black or african american alone)
    
    # internet ----
    
    # # Presence and Types of Internet Subscriptions in Household - NO RACE
    # "B28002_001E", # Estimate!!Total:	
    # "B28002_004E", #Estimate!!Total:!!With an Internet subscription!!Broadband of any type	
    
    # PRESENCE OF A COMPUTER AND TYPE OF INTERNET SUBSCRIPTION IN HOUSEHOLD
    "B28003_001", #Estimate!!Total:
    "B28003_004", # Estimate!!Total:!!Has a computer:!!With a broadband Internet subscription
    "B28009B_001", # Estimate!!Total: (black alone)
    "B28009B_004", # Estimate!!Total:!!Has a computer:!!With a broadband Internet subscription (black alone)
    
    # unemployment ----
    
    # Employment Status
    "S2301_C04_001E", # Estimate!!Unemployment rate!!Population 16 years and over	
    # Employment status, all
    "C18120_001", # Estimate!!Total:
    "C18120_002", # Estimate!!Total:!!In the labor force:
    "C18120_006", # Estimate!!Total:!!In the labor force:!!Unemployed:
    # Employment status, black alone
    "C23002B_001", # Estimate!!Total: (black alone)
    "C23002B_004", # Estimate!!Total:!!Male:!!16 to 64 years:!!In labor force: (black alone)
    "C23002B_008", # Estimate!!Total:!!Male:!!16 to 64 years:!!In labor force:!!Civilian:!!Unemployed (black alone)
    "C23002B_011", # Estimate!!Total:!!Male:!!65 years and over:!!In labor force:
    "C23002B_013", # Estimate!!Total:!!Male:!!65 years and over:!!In labor force:!!Unemployed
    "C23002B_017", # Estimate!!Total:!!Female:!!16 to 64 years:!!In labor force: (black alone)
    "C23002B_021", # Estimate!!Total:!!Female:!!16 to 64 years:!!In labor force:!!Civilian:!!Unemployed (black alone)
    "C23002B_024", # Estimate!!Total:!!Female:!!65 years and over:!!In labor force:
    "C23002B_026", # Estimate!!Total:!!female:!!65 years and over:!!In labor force:!!Unemployed
    
    # poverty status ----
    
    # 200% below is not available for black
    # # Poverty Status in the Past 12 Months
    # "S1701_C01_001E", # Estimate!!Total!!Population for whom poverty status is determined
    # "S1701_C01_042E", # S1701_C01_042E	Estimate!!Total!!Population for whom poverty status is determined!!ALL INDIVIDUALS WITH INCOME BELOW THE FOLLOWING POVERTY RATIOS!!200 percent of poverty level
    
    # Poverty Status -- Income in past 12 months below poverty level
    "B17021_001", # Estimate!!Total:
    "B17021_002", # Estimate!!Total:!!Income in the past 12 months below poverty level:
    "B17010B_001", # Estimate!!Total: (black alone)
    "B17010B_002", # Estimate!!Total:!!Income in the past 12 months below poverty level:
    
    # veteran status ----
    
    # Veteran Status
    "B21001_001", # Estimate!!Total:
    "B21001_002", # Estimate!!Total:!!Veteran
    # Veteran Status - black alone
    "C21001B_001", 	# Estimate!!Total: (black alone)
    "C21001B_004", # Estimate!!Total:!!Male:!!18 to 64 years:!!Veteran (black)
    "C21001B_007", # Estimate!!Total:!!Male:!!65 years and over:!!Veteran (black)
    "C21001B_011", # Estimate!!Total:!!Female:!!18 to 64 years:!!Veteran (black)
    "C21001B_014", # Estimate!!Total:!!Female:!!65 years and over:!!Veteran (black)
    
    # disability status ----
    
    # Disability Status
    "B23024_001", # Estimate!!Total:
    "B23024_003", # Estimate!!Total:!!Income in the past 12 months below poverty level:!!With a disability:
    "B23024_018", # Estimate!!Total:!!Income in the past 12 months at or above poverty level:!!With a disability:
    # Disability Status - black alone
    "B18101B_001", # Estimate!!Total:
    "B18101B_003", # Estimate!!Total:!!Under 18 years:!!With a disability
    "B18101B_006", # Estimate!!Total:!!18 to 64 years:!!With a disability
    "B18101B_009", # Estimate!!Total:!!65 years and over:!!With a disability
    
    # health insurance coverage -----
    
    # Health insurance coverage status
    "B27020_001", # Estimate!!Total:
    "B27020_006", # Estimate!!Total:!!Native Born:!!No health insurance coverage
    "B27020_012", # Estimate!!Total:!!Foreign Born:!!Naturalized:!!No health insurance coverage
    "B27020_017", # Estimate!!Total:!!Foreign Born:!!Noncitizen:!!No health insurance coverage
    # Health insurance coverage status - black alone
    "C27001B_001", # Estimate!!Total:
    "C27001B_004", # Estimate!!Total:!!Under 19 years:!!No health insurance coverage

    "C27001B_007", # Estimate!!Total:!!19 to 64 years:!!No health insurance coverage
    "C27001B_010" #Estimate!!Total:!!65 years and over:!!No health insurance coverage
  ),
  year = 2019,
  output = "wide"
)


# remove margin columns
acs_data <- as.data.frame(acs_data)
acs_data <- acs_data[, c(T,T,!grepl("M", colnames(acs_data)[-c(1:2)]))]

# write out ----

data_folder <- file.path(
  gsub("\\\\","/", gsub("OneDrive - ","", Sys.getenv("OneDrive"))), 
  "Health and Social Equity - SJP - BHN Score Creation",
  "Data", "Preprocessed")

write.csv(acs_data, file = file.path(data_folder,
                                     "ACS_API_Data.csv"), row.names = F, na = "")
