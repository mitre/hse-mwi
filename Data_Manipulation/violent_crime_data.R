# Preprocessing Violent Crime Data (Community Violence)
# By Emily Pantalone
# Originated on: 6/14/2021

# Load data, read csv, call packages
library(readr)
library(data.table)
library(tidyverse)
library(ggplot2)

data_folder <- file.path(
  gsub("\\\\","/", gsub("OneDrive - ","", Sys.getenv("OneDrive"))), 
  "Health and Social Equity - SJP - BHN Score Creation",
  "Data", "Raw")

vc_data <- read_csv(file.path(data_folder,"analytic_data2020_0.csv"))

#explore data dimensions
dim(vc_data)
colnames(vc_data)
unique(vc_data$`State Abbreviation`)

#delete first two rows (they are aggregate, not county-specific)
vc_data <- vc_data[-c(1,2),]

#delete unneeded columns
vc_data <- vc_data[c(1,2,3,4,5,6,7,233,234,235)]

#rename columns




# write out ----

data_folder <- file.path(
  gsub("\\\\","/", gsub("OneDrive - ","", Sys.getenv("OneDrive"))), 
  "Health and Social Equity - SJP - BHN Score Creation",
  "Data", "Preprocessed")

write.csv(vc_data, 
          file = file.path(
            data_folder,
            "CHR_County_violent_crime.csv"
          ), 
          row.names = F, 
          na = "")






