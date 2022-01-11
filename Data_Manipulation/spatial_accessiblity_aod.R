# Alcohol Outlet Density - Spatial Accessibility Method for CBP Data (County Business Patterns)

# Packages
library(dplyr)
library(tidycensus)
library(censusapi)
library(readxl)

# Set system environment
# Sys.setenv("OneDrive" = "/Users/mstrahlman/The MITRE Corporation")

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

# Import ZCTAs & polygons
load(file.path(resource_folder,"ZCTAs_shapefile_US.RData"))
zctas <- zips
rm(zips)

# Spreadsheet with state by state Grocery Laws
grocery_states <- read_xlsx(file.path(resource_folder, "BWL_Grocery_Laws.xlsx"),
                            skip = 1)

# Create column with indicator if state allows ANY type of alc sales
grocery_states[,3:5] <- ifelse(grocery_states[,3:5] == "Y", TRUE, FALSE)
grocery_states$any_alc_sales <- ifelse(rowSums(grocery_states[,3:5] , na.rm = T) > 0, TRUE, FALSE)

# Function to get zbp code from api, and return df with zipcode FIPS and number of establishments   
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

# Pull all counts for BWL, convenience stores, and convenience and gas stores
bwl <- get_zbp(445310, "bwl") #4557 zip codes
convenience <- get_zbp(445120, "convenience") #3785 zip codes
convenience_gas <- get_zbp(447110, "convenience_gas") #11135 zip codes
grocery <- get_zbp(445110, "grocery") #7005 zip codes

# Pull counts for convenience, convenience/gas, grocery stores within allowed states
allowed_states <- sapply(grocery_states[grocery_states$any_alc_sales,"Abbrev"], 
                         as.character)
state_fips <- unique(fips_codes[,1:2])
allowed_zips <- zip_cw %>% 
  left_join(state_fips, by = c("STATE" = "state")) %>%
  filter(STATE %in% allowed_states)

convenience <- convenience %>% filter(zip %in% allowed_zips$ZIP_CODE) #3459 zip codes
convenience_gas <- convenience_gas %>% filter(zip %in% allowed_zips$ZIP_CODE) #10745 zip codes
grocery <- grocery %>% filter(zip %in% allowed_zips$ZIP_CODE) #6542 zip codes

# Combine outlets together into ESTAB count
all <- full_join(bwl, convenience, by = "zip") %>%
  full_join(convenience_gas, by = "zip") %>%
  full_join(grocery, by = "zip") 
all[is.na(all)] <- 0
all <- all %>%
  mutate(ESTAB = bwl + convenience + convenience_gas + grocery) %>%
  select(ESTAB, zip)

# Convert zips to zctas
all <- all %>%
  left_join(zip_cw, by = c("zip" = "ZIP_CODE")) %>%
  select(ESTAB, ZCTA)
