# Generate Number of Business per 3rd Space NAICS Code
# By Mitch Strahlman
# Originated on: 8/30/2021

# Library Load----
library(reshape)
library(censusapi)
library(dplyr)
library(readr)
library(stringr)
library(reshape2)
library(tidycensus)

# Load crosswalks
source("Processing_Pipeline/crosswalk_func.R")

# Supporting Function ----
# Read in and clean zip code according to common mistakes
# inputs:
#   fn: file name (including path)
#   zip_cn: zip code column name
# outputs: read and cleaned data frame
read_zips <- function(fn, zip_cn){
  # read in specified file
  zip_df <- read.csv(fn,
                     colClasses = setNames("character", zip_cn))
  
  # remove all dashes
  zip_df[, zip_cn] <- gsub("-", "", zip_df[,zip_cn])
  
  # pad input zips with leading 0s
  zip_df[, zip_cn] <- str_pad(zip_df[, zip_cn], 5, pad = "0")
  
  # get rid of weirdly long ones
  zip_df[, zip_cn] <- substr(zip_df[, zip_cn], 1, 5)
  
  return(zip_df)
}

# Load data----
# Read in NAICS codes
data_folder <- file.path(
  gsub("\\\\","/", gsub("OneDrive - ","", Sys.getenv("OneDrive"))), 
  "Health and Social Equity - SJP - BHN Score Creation",
  "Data", "Raw", "Third_Spaces")

# Codes that Hannah Highlighted in xlsx doc (prevents doublecounting)
naics_codes <- 
  read.csv(file.path(data_folder, "NAICS_Third_Places_aggregation.csv")) %>%
  dplyr::rename("Code" = "X2017.NAICS.Code",
                "Business_Type" = "X2017.NAICS.Title") %>%
  filter(Code %in% c(4451, 4452, 451, 51912, 51919, 7111, 7112, 712, 7131,
                     7139, 72233, 7224, 7225, 8121, 81221, 81222, 81231,
                     81232, 81291, 81292, 81299, 813, 8134))

# Trim White Space from Character Values
naics_codes$Business_Type <- trimws(naics_codes$Business_Type)

# Read in all zip codes
resource_folder <- file.path(
  gsub("\\\\","/", gsub("OneDrive - ","", Sys.getenv("OneDrive"))), 
  "Health and Social Equity - SJP - BHN Score Creation",
  "Data", "Resources")

zip <- read_zips(
  file.path(resource_folder, "Zip_to_zcta_crosswalk_2021.csv"),
  "ZIP_CODE")

# Filter out Territories
territories <- c("AS", "FM", "GU", "MH", "MP", "PW", "PR", "VI")
zip <- zip[!zip$STATE %in% territories, ]

# Load in CBP data
results <- list()
for (i in 1:nrow(naics_codes)) {
  cbp <- getCensus(
    name = "cbp",
    vars = c("ESTAB"), 
    vintage = 2020,
    region = "zipcode:*",
    show_call = F,
    NAICS2017 = naics_codes$Code[i]
  )
  results[[i]] <- cbp
  }
cbp <- do.call(rbind, results)

# Data Cleaning----
# Create Column for each NAICS Code
cbp <- reshape::cast(cbp, zip_code ~ NAICS2017, sum, value = 'ESTAB') %>%
  # Add in the remainder of the zipcodes with 0 third spaces
  full_join(zip, by = c("zip_code" = "ZIP_CODE")) %>%
  # Remove irrelevant variables
  select(c(1:24))

# Convert zip to zcta
cbp_zcta <- zip_to_zcta(cbp, 
            geoid_col = "zip_code", 
            meas_col = colnames(cbp)[-1], 
            use_mean = F)
# 1 zip code not found in master crosswalk
# Resulting in 6 ZCTAs missing/not mapped for each category

# Identify how many ZCTAs have 0 third spaces in these categories
nrow(subset(cbp_zcta, `4451` == 0 & `4452` == 0 & `451` == 0 & `51912` == 0 &
         `51919` == 0 & `7111` == 0 & `7112` == 0 & `712` == 0 &
         `7131` == 0 & `7139` == 0 & `72233` == 0 & `7224` == 0 &
         `7225` == 0 & `8121` == 0 & `81221` == 0 & `81222` == 0 &
         `81231` == 0 & `81232` == 0 & `81291` == 0 & `81292` == 0 &
         `81299` == 0 & `813` == 0 & `8134` == 0))

# 13327 ZCTAs have 0 third spaces - this is about 40% of ZCTAs

# Load ZCTA Populations
pop <- get_acs(geography = "zcta",
               output = "wide",
               year = 2020,
               survey = "acs5",
               variables = "B01001_001",
               geometry = F
) %>%
  select(GEOID, B01001_001E)

# Calculate Measure----
# Calculating measure value = # third spaces / 100k population in ZCTA
final <- full_join(cbp_zcta, pop, by = c("ZCTA" = "GEOID")) %>%
  dplyr::rename(population = B01001_001E)
final <- mutate(final,
                thirdspaces_pop = (rowSums(final[2:24])/population)*100000,
              # Replacing Infs with 0s
              thirdspaces_pop = ifelse(is.infinite(thirdspaces_pop),
                                       0, thirdspaces_pop),
              # Replacing NaNs with 0
              thirdspaces_pop = ifelse(is.nan(thirdspaces_pop),
                                       0, thirdspaces_pop),
              third_spaces_total = rowSums(final[2:24])) %>%
  select(ZCTA, third_spaces_total, population, thirdspaces_pop)

# NaNs indicated 0 third spaces and a 0 population value
# Inf indicated a non-zero integer for at least one third
# space and a 0 population value

# Export Data Frame----

data_folder <- file.path(
  gsub("\\\\","/", gsub("OneDrive - ","", Sys.getenv("OneDrive"))), 
  "Health and Social Equity - SJP - BHN Score Creation",
  "Data", "Preprocessed")

write.csv(final, 
          file = file.path(data_folder,
                           "NAICS_Third_Spaces.csv"), 
          row.names = F, 
          na = "")
