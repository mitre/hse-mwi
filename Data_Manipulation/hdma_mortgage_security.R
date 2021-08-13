# Preprocessing Mortgage Security Data
# By Hannah De los Santos
# Originated on: 8/13/21

# load data and libraries ----

library(data.table)

data_folder <- file.path(
  gsub("\\\\","/", gsub("OneDrive - ","", Sys.getenv("OneDrive"))), 
  "Health and Social Equity - SJP - BHN Score Creation",
  "Data", "Raw")

# load mortgage data
# using alaska for ease of processing, will use national data in the end
mort_dt <- read.csv(file.path(
  data_folder,
  "HDMA",
  "hmda_2017_ak_all-records_labels.csv"
))
