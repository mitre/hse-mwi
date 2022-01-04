# Alcohol Outlet Density - Spatial Accessibility Method for CBP Data (County Business Patterns)

# Packages
library(dplyr)
library(tidycensus)
library(censusapi)
library(readxl)

source("Processing_Pipeline/crosswalk_func.R")

# Set Data & Resource Folders
data_folder <- file.path(
  gsub("\\\\","/", gsub("OneDrive - ","", Sys.getenv("OneDrive"))), 
  "Health and Social Equity - SJP - BHN Score Creation",
  "Data", "Raw")

resource_folder <-file.path(
  gsub("\\\\","/", gsub("OneDrive - ","", Sys.getenv("OneDrive"))), 
  "Health and Social Equity - SJP - BHN Score Creation",
  "Data", "Resources")

# Spreadsheet with zipcodes and state FIPS


# Spreadsheet with state by state Grocery Laws
grocery_states <- read_xlsx(file.path(resource_folder, "BWL_Grocery_Laws.xlsx"),
                            skip = 1)

# Create column with indicator if state allows ANY type of alc sales
grocery_states[,3:5] <- ifelse(grocery_states[,3:5] == "Y", TRUE, FALSE)
grocery_states$any_alc_sales <- ifelse(rowSums(grocery_states[,3:5] , na.rm = T) > 0, TRUE, FALSE)

# Function to get cbp code from api, and return df with zipcode FIPS and number of establishments   
get_zbp <- function(naics, name){
  dat <- getCensus(name = "cbp",
                   key = Sys.getenv("CENSUS_API_KEY"),
                   vintage = "2019",
                   vars = "ESTAB",
                   region = "zipcode:*",
                   NAICS2017 = naics
  ) %>%
    mutate(zip = paste0(zip_code)) %>%
    select(zip, ESTAB) 
  colnames(dat) <- c("zip", name)
  return(dat)
}

