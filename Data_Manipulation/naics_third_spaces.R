# Generate Number of Business per 3rd Space NAICS Code
# By Mitch Strahlman
# Originated on: 8/30/2021

# Library Load----
library(censusapi)
library(dplyr)
library(readr)
library(stringr)
library(reshape2)

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

naics_codes <- 
  read.csv(file.path(data_folder, "NAICS_Third_Places_aggregation.csv")) %>%
  rename("Code" = "X2017.NAICS.Code",
         "Business_Type" = "X2017.NAICS.Title") %>%
  filter(Code %in% c(71, 445, 8134, 522110, 722, 51912, 812, 8131, 451))

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

# Clean Data----
# Create NAICS columns
naics_codes_columns <- naics_codes %>%
  dcast(Code ~ Business_Type) %>%
  select(-1) 
naics_codes_columns[1:9] <- c("")

# Merge Columns into Zipcode Data
zip <- cbind(zip, naics_codes_columns)
  
# Pull Count of Businesses in Each Zip Code
for (i in 1:9){
  getCensus(name = "cbp",
            vars = c("ESTAB"), 
            vintage = 2019,
            region = "zipcode:*",
            show_call = F,
            NAICS2017 = naics_codes$Code[i]
  )
}