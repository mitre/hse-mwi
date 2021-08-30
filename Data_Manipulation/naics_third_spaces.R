# Library Load----
library(censusapi)
library(tidyverse)

# Read in NAICS codes
data_folder <- file.path(
  gsub("\\\\","/", gsub("OneDrive - ","", Sys.getenv("OneDrive"))), 
  "Health and Social Equity - SJP - BHN Score Creation",
  "Data", "Raw", "Third_Spaces")

naics_codes <- 
  read.csv(file.path(data_folder, "NAICS_Third_Places_aggregation.csv")) %>%
  rename("Code" = "X2017.NAICS.Code",
         "Business_Type" = "X2017.NAICS.Title")
