# Container Methods for CBP Data (County Business Patterns)
# By Karen Jiang
# Originated on: 6/8/21

library(tidyverse)

# Set Data Folder 
data_folder <- file.path(
  gsub("\\\\","/", gsub("OneDrive - ","", Sys.getenv("OneDrive"))), 
  "Health and Social Equity - SJP - BHN Score Creation",
  "Data", "Raw")

# Get population denominators
pop <- get_acs(geography = "zcta",
               output = "wide",
               year = 2019,
               survey = "acs5",
               variables = "B01001_001",
               geometry = F
)

# Crosswalk Data
zip_to_zcta <- read.csv(paste0(data_folder, "/Resources/zip_to_zcta_2019.csv")) %>%
  mutate(ZIP_CODE = str_pad(ZIP_CODE, 5, "left", "0"))


# Load in CBP data
cbp <- read.csv(paste0(data_folder, "/CBP_zbp18detail.txt"))

# Filter for all NAICS Codes for Beer, Wine, and Liquor Stores
# https://www.naics.com/code-search/?naicstrms=liquor) = 445310 
bwl <- cbp %>% 
  filter(naics == 445310) %>%
  mutate(zip = str_pad(zip, width = 5, side = "left", pad = "0")) %>%
  select(zip, est)




# write out ----

data_folder <- file.path(
  gsub("\\\\","/", gsub("OneDrive - ","", Sys.getenv("OneDrive"))), 
  "Health and Social Equity - SJP - BHN Score Creation",
  "Data", "Preprocessed")

write.csv(bwl, 
          file = file.path(
            data_folder,
            "CBP_ZIP_AlcoholOutlet.csv"
          ), 
          row.names = F, 
          na = "")
