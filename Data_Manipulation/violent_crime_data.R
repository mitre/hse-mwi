# Exploring Violent Crime Data (Community Violence)
# By Emily Pantalone
# Originated on: 6/14/2021

# Set working directory, load data and packages

data_folder <- file.path(
  gsub("\\\\","/", gsub("OneDrive - ","", Sys.getenv("OneDrive"))), 
  "Health and Social Equity - SJP - BHN Score Creation",
  "Data", "Raw")

vc_data <- read_csv(file.path(data_folder,"analytic_data2020_0.csv"))
#change document 

library(readr)
library(data.table)
library(tidyverse)
library(ggplot2)
vc_data <- read_csv("~/OneDrive - The MITRE Corporation/Health and Social Equity - SJP - BHN Score Creation/Data/Raw/analytic_data2020_0.csv")

vc_data <- read_csv("C:/Users/epantalone/The MITRE Corporation/Health and Social Equity - SJP - BHN Score Creation/Data/Raw/analytic_data2020_0.csv")

#explore data dimensions
dim(vc_data)m 
colnames(vc_data)
unique(vc_data$source)
unique(vc_data$sumlevel)
unique(vc_data$name)

# START OVER, SET DATA THROUGH ONEDRIVE
data_folder <- file.path(
  gsub("\\\\","/", gsub("OneDrive - ","", Sys.getenv("OneDrive"))), 
  "Health and Social Equity - SJP - BHN Score Creation",
  "Data", "Raw")
vc_data <- read_csv("analytic_data2020_0.csv")



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






