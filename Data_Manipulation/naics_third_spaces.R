# Generate Number of Business per 3rd Space NAICS Code
# By Mitch Strahlman
# Originated on: 8/30/2021

# Library Load----
library(censusapi)
library(dplyr)
library(readr)
library(stringr)
library(reshape2)
library(zipcodeR)

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
  rename("Code" = "X2017.NAICS.Code",
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

# Load all zip codes
zip <- read_zips(
  file.path(resource_folder, "Zip_to_zcta_crosswalk_2020.csv"),
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
    vintage = 2019,
    region = "zipcode:*",
    show_call = F,
    NAICS2017 = naics_codes$Code[i]
  )
  results[[i]] <- cbp
  }
cbp <- do.call(rbind, results)

# Convert zip to zcta
zip_to_zcta(data.frame(zip), 
            geoid_col = "ZIP_CODE", 
            meas_col = "ESTAB", 
            use_mean = F)

# Load Zipcode Populations
pop <- get_acs(geography = "zcta",
               output = "wide",
               year = 2019,
               survey = "acs5",
               variables = "B01001_001",
               geometry = F
) %>%
  select(GEOID, B01001_001E)


# Clean Data----


# Merge business count data into zipcode data


# Merge zipcode populations data into business count data
zip <- left_join(zip, zip_pop, by = c("ZIP_CODE" = "zipcode"))

# Replace NAs with 0s
zip[is.na(zip)] <- 0

# Remove Unnecessary Columns & Add Population/100k variable
zip <- zip %>%
  mutate(thirdspaces_pop = rowSums(.[7:15])/(population/100000)) %>%
  select(-c(2:6, "population")) %>%
  relocate(thirdspaces_pop, .after = ZIP_CODE) %>%
  mutate(thirdspaces_pop = replace(thirdspaces_pop, is.nan(thirdspaces_pop), 0))

# Replace Inf with NA
zip$thirdspaces_pop[is.infinite(zip$thirdspaces_pop)] <- NA

# Export Data Frame----

data_folder <- file.path(
  gsub("\\\\","/", gsub("OneDrive - ","", Sys.getenv("OneDrive"))), 
  "Health and Social Equity - SJP - BHN Score Creation",
  "Data", "Preprocessed")

write.csv(zip, 
          file = file.path(data_folder,
                           "NAICS_Third_Spaces.csv"), 
          row.names = F, 
          na = "")
