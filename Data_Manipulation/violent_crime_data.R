# Preprocessing Violent Crime Data (Community Violence)
# By Emily Pantalone
# Originated on: 6/14/2021

# Load data, read csv, call packages
library(readr)
library(data.table)
library(tidyverse)

data_folder <- file.path(
  gsub("\\\\","/", gsub("OneDrive - ","", Sys.getenv("OneDrive"))), 
  "Health and Social Equity - SJP - BHN Score Creation",
  "Data", "Raw")

vc_data <- read_csv(file.path(data_folder,"analytic_data2020_0.csv"))
# og_vc_data <- read_csv(file.path(data_folder,"analytic_data2020_0.csv"))

#explore data dimensions
dim(vc_data)
colnames(vc_data)

#delete first two rows (they are aggregate, not county-specific)
vc_data <- vc_data[-c(1,2),]

#delete unneeded columns
vc_data <- vc_data[c(1,2,3,4,5,233,234,235)]


#rename columns
names(vc_data)[names(vc_data) == "County FIPS Code"] <- "county_FIPS"
names(vc_data)[names(vc_data) == "5-digit FIPS Code"] <- "full_FIPS"
names(vc_data)[names(vc_data) == "State FIPS Code"] <- "state_FIPS"
names(vc_data)[names(vc_data) == "State Abbreviation"] <- "state"
names(vc_data)[names(vc_data) == "Name"] <- "county"
names(vc_data)[names(vc_data) == "Violent crime numerator"] <- "violent_crime_num"
names(vc_data)[names(vc_data) == "Violent crime denominator"] <- "violent_crime_denom"
names(vc_data)[names(vc_data) == "Violent crime raw value"] <- "violent_crime_value"


#remove state aggregate rows
vc_data <- vc_data[vc_data$county_FIPS != '000', ] 

#change violent crime numbers to integers (currently is a character)
vc_data$violent_crime_num <- as.numeric(vc_data$violent_crime_num)
vc_data$violent_crime_denom <- as.numeric(vc_data$violent_crime_denom)
vc_data$violent_crime_value <- as.numeric(vc_data$violent_crime_value)

#create column with raw integer of violent crimes per population (num/denom), 
# not per 100,000 population as violent_crime_value currently indicates
vc_data$violent_crime_ratio <- vc_data$violent_crime_num/vc_data$violent_crime_denom

#scale violent_crime_value to be per 10,000 population
vc_data$violent_crime_value <- vc_data$violent_crime_value/10

#explore outliers that may need to be clipped and count NAs
summary(vc_data$violent_crime_value)
#library(ggplot2)
#boxplot(vc_data$violent_crime_value)
#sum(vc_data$violent_crime_value > 100, na.rm = TRUE)
#nrow(vc_data)

# There are 23 values that are > 100 that could be clipped, out of 3142 values (0.7%)
# There are 191 NAs out of 3142 possible values (6% missing)



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






