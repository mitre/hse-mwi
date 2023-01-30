# Alcohol Outlet Density - Container Methods for CBP Data (County Business Patterns)

# Author: Karen Jiang
# Version: 2021-12-27

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

# Spreadsheet with state by state Grocery Laws
grocery_states <- read_xlsx(file.path(resource_folder, "BWL_Grocery_Laws.xlsx"), skip = 1)
# Create column with indicator if state allows ANY type of alc sales
grocery_states[,3:5] <- ifelse(grocery_states[,3:5] == "Y", TRUE, FALSE)
grocery_states$any_alc_sales <- ifelse(rowSums(grocery_states[,3:5] , na.rm = T) > 0, TRUE, FALSE)

# Function to get cbp code from api, and return df with county FIPS and number of establishments   
get_cbp <- function(naics, name){
  dat <- getCensus(name = "cbp",
                   key = Sys.getenv("CENSUS_API_KEY"),
                   vintage = "2020",
                   vars = "ESTAB",
                   region = "county:*",
                   NAICS2017 = naics
  ) %>%
    mutate(fips = paste0(state,county)) %>%
    select(fips, ESTAB) 
  colnames(dat) <- c("fips", name)
  return(dat)
}

# Pull all counts for BWL, convenience stores, and convenience and gas stores
bwl <- get_cbp(445310, "bwl") #1470 counties
convenience <- get_cbp(445120, "convenience") #1215 counties
convenience_gas <- get_cbp(447110, "convenience_gas") #2942 counties
grocery <- get_cbp(445110, "grocery") #2354 counties

# Pull counts for convenience, convenience/gas, grocery stores within allowed states
allowed_states <- sapply(grocery_states[grocery_states$any_alc_sales,"Abbrev"], 
                         as.character)
state_fips <- unique(fips_codes[,1:2])
allowed_counties <- county_cw %>% 
  left_join(state_fips, by = c("STATE" = "state_code")) %>%
  filter(state %in% allowed_states)

convenience <- convenience %>% filter(fips %in% allowed_counties$GEOID) #1134 counties
convenience_gas <- convenience_gas %>% filter(fips %in% allowed_counties$GEOID) #2802 counties
grocery <- grocery %>% filter(fips %in% allowed_counties$GEOID) #2191 counties

# Combine outlets together into ESTAB count
all <- full_join(bwl, convenience, by = "fips") %>%
  full_join(convenience_gas, by = "fips") %>%
  full_join(grocery, by = "fips") 
all[is.na(all)] <- 0
all <- all %>%
  mutate(ESTAB = bwl + convenience + convenience_gas + grocery) %>%
  select(ESTAB, fips)

# Get population denominators from ACS ------
pop <- get_acs(geography = "county",
               output = "wide",
               year = 2020,
               survey = "acs5",
               variables = "B01001_001",
               geometry = F
) %>%
  select(GEOID, B01001_001E)

# Join population denominators by county
cbp <- full_join(all, pop, by = c("fips" = "GEOID")) %>%
  mutate(alcoholoutlet_pop = ESTAB / B01001_001E, 
         # counties with NA establishment set to 0
         alcoholoutlet_pop = ifelse(is.na(alcoholoutlet_pop), 0, alcoholoutlet_pop)) %>%
  select(fips, alcoholoutlet_pop)


# write out ----
data_folder <- file.path(
  gsub("\\\\","/", gsub("OneDrive - ","", Sys.getenv("OneDrive"))), 
  "Health and Social Equity - SJP - BHN Score Creation",
  "Data", "Preprocessed")

write.csv(cbp, 
          file = file.path(
            data_folder,
            "CBP_County_AlcoholOutlet.csv"
          ), 
          row.names = F, 
          na = "")
