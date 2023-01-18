# Pulling Raw ACS Data
# By Hannah De los Santos, Karen Jiang
# Originated on: 2/24/21

# load data and libraries ----

library(tidycensus)

# census_api_key("YOUR_KEY_HERE")

# pull data ----

# ACS 5 year 2021
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
    
    # # below 125% poverty level -- subject tables -- TOO MUCH MISSINGNESS
    # "S1703_C04_001", # Estimate!!Less than 125 percent of the poverty level!!Population for whom poverty status is determined
    # "S1703_C01_001", # Estimate!!Total!!Population for whom poverty status is determined
    # 
    # "S1703_C04_010", # Estimate!!Less than 125 percent of the poverty level!!Population for whom poverty status is determined!!RACE AND HISPANIC OR LATINO ORIGIN!!One race!!Black or African American
    # "S1703_C01_010", # Estimate!!Total!!Population for whom poverty status is determined!!RACE AND HISPANIC OR LATINO ORIGIN!!One race!!Black or African American
    
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
    "C27001B_010", #Estimate!!Total:!!65 years and over:!!No health insurance coverage,
    
    # Early Childhood Education -----
    
    # Numerator: School enrollment by detailed level of school for the population 3 years and older
    "B14007_003", # Estimate!!Total:!!Enrolled in school:!!Enrolled in nursery school, preschool

    "B14007B_003", # Estimate!!Total:!!Enrolled in school:!!Enrolled in nursery school, preschool (Black or African American Alone)

    # Denominator: Under 5 population
    "B01001_003", # Estimate!!Total:!!Male:!!Under 5 years
    "B01001_027", # Estimate!!Total:!!Female:!!Under 5 years
    
    "B01001B_003", # Estimate!!Total:!!Male:!!Under 5 years (Black or African American Alone)
    "B01001B_018", # Estimate!!Total:!!Female:!!Under 5 years (Black or African American Alone)
    
    
    # ICE variables -----
    
    # Index of Concentration at the Extremes for Black / White Income
    # HOUSEHOLD INCOME IN THE PAST 12 MONTHS (IN 2019 INFLATION-ADJUSTED DOLLARS)
    "B19001_001", # Estimate!!Total:
    
    # All, Less than $25k
    "B19001_002", # Estimate!!Total:!!Less than $10,000
    "B19001_003", # Estimate!!Total:!!$10,000 to $14,999
    "B19001_004", # Estimate!!Total:!!$15,000 to $19,999
    "B19001_005", # Estimate!!Total:!!$20,000 to $24,999
    
    # # White Non-Hispanic, Less than $25k
    "B19001H_002", # Estimate!!Total:!!Less than $10,000
    "B19001H_003", # Estimate!!Total:!!$10,000 to $14,999
    "B19001H_004", # Estimate!!Total:!!$15,000 to $19,999
    "B19001H_005", # Estimate!!Total:!!$20,000 to $24,999
    
    # Black or African American, Less than $25k
    "B19001B_002", # Estimate!!Total:!!Less than $10,000
    "B19001B_003", # Estimate!!Total:!!$10,000 to $14,999
    "B19001B_004", # Estimate!!Total:!!$15,000 to $19,999
    "B19001B_005", # Estimate!!Total:!!$20,000 to $24,999
    
    # White Non-Hispanic, Greater than $125k
    "B19001H_015", # Estimate!!Total:!!$125,000 to $249,999
    "B19001H_016", # Estimate!!Total:!!$150,000 to $199,999
    "B19001H_017"  # Estimate!!Total:!!$200,000 or more
  ), 
  year = 2021,
  output = "wide"
)

# remove margin columns
acs_data <- as.data.frame(acs_data)
acs_data <- acs_data[, c(T,T,!grepl("M", colnames(acs_data)[-c(1:2)]))]

# add columns for a total output ----

# we want to add certain columns for a "total output"
# we're going to do this by hand
# column title naming convention: varnamecategory_(varname_)(denom_)(pop OR black)
cname_map <- list(
  "educationalattainment_denom_pop" = c("B06009_001E"),
  "educationalattainment_lessthanhighschool_pop" = c("B06009_002E"),
  "educationalattainment_somecollege_pop" = c("B06009_004E"),
  "educationalattainment_denom_black" = c("C15002B_001E"),
  "educationalattainment_lessthanhighschool_black" = c("C15002B_003E", "C15002B_003E"),
  "educationalattainment_somecollege_black" = c("C15002B_005E", "C15002B_010E"),
  "worktransportation_denom_pop" = c("B08006_001E"),
  "worktransportation_caralone_pop" = c("B08006_003E"),
  "worktransportation_denom_black" = c("B08105B_001E"),
  "worktransportation_caralone_black" = c("B08105B_002E"),
  "computerinternet_denom_pop" = c("B28003_001E"),
  "computerinternet_computerandbroadband_pop" = c("B28003_004E"),
  "computerinternet_denom_black" = c("B28009B_001E"),
  "computerinternet_computerandbroadband_black" = c("B28009B_004E"),
  "unemployment_denom_pop" = c("C18120_002E"),
  "unemployment_unemployed_pop" = c("C18120_006E"),
  "unemployment_denom_black" = c("C23002B_004E", "C23002B_011E", "C23002B_017E", "C23002B_024E"),
  "unemployment_unemployed_black" = c("C23002B_008E", "C23002B_013E","C23002B_021E", "C23002B_026E"),
  "povertystatus_denom_pop" = c("B17021_001E"),
  "povertystatus_below100povertylevel_pop" = c("B17021_002E"),
  "povertystatus_denom_black" = c("B17010B_001E"),
  "povertystatus_below100povertylevel_black" = c("B17010B_002E"),
  "veteranstatus_denom_pop" = c("B21001_001E"),
  "veteranstatus_veteran_pop" = c("B21001_002E"),
  "veteranstatus_denom_black" = c("C21001B_001E"),
  "veteranstatus_veteran_black" = c("C21001B_004E","C21001B_007E","C21001B_011E","C21001B_014E"),
  "disabilitystatus_denom_pop" = c("B23024_001E"),
  "disabilitystatus_disability_pop" = c("B23024_003E", "B23024_018E"),
  "disabilitystatus_denom_black" = c("B18101B_001E"),
  "disabilitystatus_disability_black" = c("B18101B_003E", "B18101B_006E", "B18101B_009E"),
  "healthinsurance_denom_pop" = c("B27020_001E"),
  "healthinsurance_nohealthinsurance_pop" = c("B27020_006E", "B27020_012E", "B27020_017E"),
  "healthinsurance_denom_black" = c("C27001B_001E"),
  "healthinsurance_nohealthinsurance_black" = c("C27001B_004E", "C27001B_007E","C27001B_010E"),

  "earlychildhoodeducation_denom_pop" = c("B01001_003E", "B01001_027E"),
  "earlychildhoodeducation_enrolled_pop" = c("B14007_003E"),
  "earlychildhoodeducation_denom_black" = c("B01001B_003E", "B01001B_018E"),
  "earlychildhoodeducation_enrolled_black" = c("B14007B_003E")

)

# preallocate -- we're going to just keep the geoid and name columns
acs_final <- acs_data[, c("GEOID", "NAME")]
for (cn in 1:length(cname_map)){
  acs_final[, names(cname_map)[cn]] <- 
    rowSums(acs_data[,cname_map[[cn]], drop = F], na.rm = T)
  # if all na, we want to leave as NA
  acs_final[rowSums(is.na(acs_data[,cname_map[[cn]], drop = F]), na.rm = T) ==
              length(cname_map[[cn]]), names(cname_map)[cn]] <- NA
}

# ice pre-calculations ----

## Formula ICE (black/white) = (High Income White - Low Income Black) / Total Pop

acs_final$ice_numerator_black <-
  # high income white
  rowSums(acs_data[, c("B19001H_015E",
                       "B19001H_016E",
                       "B19001H_017E")], na.rm = T) -
  # low income black
  rowSums(acs_data[, c("B19001B_002E",
                       "B19001B_003E",
                       "B19001B_004E",
                       "B19001B_005E")], na.rm = T)
# Take absolute value of numerator (to align so that all higher numbers indicate more segregation)
acs_final$iceabs_numerator_black <- abs(acs_final$ice_numerator_black)

acs_final$ice_denom_black <- acs_data$B19001_001E

## Formula ICE (nonwhite/white) = (High Income White - Low Income Nonwhite) / Total Pop

acs_final$ice_numerator_pop <-
  # high income white
  rowSums(acs_data[, c("B19001H_015E",
                       "B19001H_016E",
                       "B19001H_017E")], na.rm = T) -
  # low income all
  (rowSums(acs_data[, c("B19001_002E",
                       "B19001_003E",
                       "B19001_004E",
                       "B19001_005E")], na.rm = T) -
  # low income white
  rowSums(acs_data[, c("B19001H_002E",
                     "B19001H_003E",
                     "B19001H_004E",
                     "B19001H_005E")], na.rm = T))
# Take absolute value of numerator (to align so that all higher numbers indicate more segregation)
acs_final$iceabs_numerator_pop <- abs(acs_final$ice_numerator_pop)

acs_final$ice_denom_pop <- acs_data$B19001_001E

# write out ----

data_folder <- file.path(
  gsub("\\\\","/", gsub("OneDrive - ","", Sys.getenv("OneDrive"))), 
  "Health and Social Equity - SJP - BHN Score Creation",
  "Data", "Preprocessed")

write.csv(acs_final, file = file.path(data_folder,
                                     "ACS_API_Data.csv"), 
          row.names = F, na = "")
