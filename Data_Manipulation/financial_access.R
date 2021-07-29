# Generating Financial Accessibility
# By Hannah De los Santos
# Originated on: 7/29/21

# load data and libraries ----

library(censusapi)

data_folder <- file.path(
  gsub("\\\\","/", gsub("OneDrive - ","", Sys.getenv("OneDrive"))), 
  "Health and Social Equity - SJP - BHN Score Creation",
  "Data", "Raw", "FDIC_NCUA")

# federal banking institutions
fed_banks <- read.csv(file.path(data_folder, "FDIC_Institutions_7_29_2021.csv"))
# credit unions
cu_banks <- read.csv(file.path(data_folder, 
                               "NCUA_Credit_Union_Branch_Information.csv"))

