# Container Methods for CBP Data (County Business Patterns)
# By Karen Jiang
# Originated on: 6/8/21

library(dplyr)
library(tidycensus)
library(censusapi)

source("Processing_Pipeline/crosswalk_func.R")

# Set Data Folder 
data_folder <- file.path(
  gsub("\\\\","/", gsub("OneDrive - ","", Sys.getenv("OneDrive"))), 
  "Health and Social Equity - SJP - BHN Score Creation",
  "Data", "Raw")

# Load in CBP data
  # Filter for all NAICS Codes for Beer, Wine, and Liquor Stores
  # https://www.naics.com/code-search/?naicstrms=liquor) = 445310 
cbp <- getCensus(name = "zbp",
          key = Sys.getenv("CENSUS_API_KEY"),
          vintage = "2018",
          vars = c("ESTAB","ZIPCODE"),
          NAICS2017 = 445310, 
          ) %>%

  # Grabbing the max establishment size from each ZIP Code group. 
    # Duplicate values are establishment counts by employee size.  
    # Double checked with the original data and was an exact match.
  group_by(ZIPCODE) %>%
  arrange(desc(ESTAB)) %>%
  filter(row_number() == 1) %>%
  
  # Adding back any dropped leading 0s in the ZIP Codes
  mutate(ZIPCODE = str_pad(ZIPCODE, width = 5, side = "left", pad = "0")) %>%
  select(ZIPCODE, ESTAB) 
  
# Using crosswalk function from zip to zcta, sums up many to one relationships.
cbp <-  zip_to_zcta(data.frame(cbp), 
                    geoid_col = "ZIPCODE", 
                    meas_col = "ESTAB", 
                    use_mean = F)

#[1] "2021-08-31 12:52:20: Converting ZIP code to ZCTA"
#[1] "2021-08-31 12:52:20: 0.02192% (1) of ZIP codes not found in master crosswalk"
#[1] "2021-08-31 12:53:12: Mapped est to ZCTA, 86.18% (28429) missing/not mapped"


# Get population denominators from ACS
pop <- get_acs(geography = "zcta",
               output = "wide",
               year = 2019,
               survey = "acs5",
               variables = "B01001_001",
               geometry = F
) %>%
  select(GEOID, B01001_001E)



# Calculating measure value = # outlets / population in ZCTA
final <- full_join(cbp, pop, by = c("ZCTA" = "GEOID")) %>%
  mutate(alcoholoutlet_pop = ESTAB/B01001_001E,
         # Replacing NAs with 0s
         alcoholoutlet_pop = ifelse(is.na(alcoholoutlet_pop), 0, alcoholoutlet_pop),
         # Set max for areas with greater than 100 outlets per 100,000 (55 ZCTAs)
         alcoholoutlet_pop = ifelse(alcoholoutlet_pop > 0.001, 0.001, alcoholoutlet_pop)) %>%
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
