# Container Methods for CBP Data (County Business Patterns)
# By Karen Jiang
# Originated on: 6/8/21

library(tidyverse)
source("Processing_Pipeline/crosswalk_func.R")

# Set Data Folder 
data_folder <- file.path(
  gsub("\\\\","/", gsub("OneDrive - ","", Sys.getenv("OneDrive"))), 
  "Health and Social Equity - SJP - BHN Score Creation",
  "Data", "Raw")

# Load in CBP data
cbp <- read.csv(paste0(data_folder, "/CBP_zbp18detail.txt"))


# Get population denominators from ACS
pop <- get_acs(geography = "zcta",
               output = "wide",
               year = 2019,
               survey = "acs5",
               variables = "B01001_001",
               geometry = F
) %>%
  select(GEOID, B01001_001E)



# Filter for all NAICS Codes for Beer, Wine, and Liquor Stores
# https://www.naics.com/code-search/?naicstrms=liquor) = 445310 
bwl <- cbp %>% 
  filter(naics == 445310) %>%
  # Adding back any dropped leading 0s in the ZIP Codes
  mutate(zip = str_pad(zip, width = 5, side = "left", pad = "0")) %>%
  select(zip, est) %>%
  
  # Using crosswalk function from zip to zcta, sums up many to one relationships.
  zip_to_zcta(geoid_col = "zip", meas_col = "est", use_mean = F)


# Calculating measure value = # outlets / population in ZCTA
final <- full_join(bwl, pop, by = c("ZCTA" = "GEOID")) %>%
  mutate(alcoholoutlet_pop = est/B01001_001E) %>%
  select(ZCTA, alcoholoutlet_pop)

# write out ----
data_folder <- file.path(
  gsub("\\\\","/", gsub("OneDrive - ","", Sys.getenv("OneDrive"))), 
  "Health and Social Equity - SJP - BHN Score Creation",
  "Data", "Preprocessed")

write.csv(final, 
          file = file.path(
            data_folder,
            "CBP_ZCTA_AlcoholOutlet.csv"
          ), 
          row.names = F, 
          na = "")
