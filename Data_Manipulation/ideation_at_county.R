# Tidyverse for Data Manipulation
library(tidyverse)
# cdlTools for converting FIPs to state names
library(cdlTools)
# Haven for Data Import
library(haven)

# Download Substate level data that is delineated by Census Tract (7 states)
temp <- tempfile()
download.file("https://www.samhsa.gov/data/sites/default/files/cbhsq-reports/NSDUHsubstateRegionDefs2016B/substate_definitions_SAS.zip", 
              temp)
tract <- read_sas(unz(temp, "substate_tract141516.sas7bdat"))
unlink(temp)
tract <- select(tract, c(-1)) # Remove Census Tract Column

# Download Substate level data that is delineated only by County (44 states)
temp <- tempfile()
download.file("https://www.samhsa.gov/data/sites/default/files/cbhsq-reports/NSDUHsubstateRegionDefs2016B/substate_definitions_SAS.zip", 
              temp)
county <- read_sas(unz(temp, "substate_county141516.sas7bdat"))
unlink(temp)

# Merge the datasets
substate <- rbind(county, tract)

# Correct Format of County FIPs 
substate$county <- sprintf("%05d", substate$county) 

# Correct County FIPs for All 51 States
for (i in 1:2) {
  substate[substate$state == i, ] <- filter(substate, state == i) %>%
    mutate(county = str_replace(county, "00", paste("0", i, sep = "")))
}

for (i in 4:6) {
  substate[substate$state == i, ] <- filter(substate, state == i) %>%
    mutate(county = str_replace(county, "00", paste("0", i, sep = "")))
}

for (i in 8:9) {
  substate[substate$state == i, ] <- filter(substate, state == i) %>%
    mutate(county = str_replace(county, "00", paste("0",i, sep = "")))
}

for (i in 10:13) {
  substate[substate$state == i, ] <- filter(substate, state == i) %>%
    mutate(county = str_replace(county, "00", paste(i)))
}

for (i in 15:42) {
  substate[substate$state == i, ] <- filter(substate, state == i) %>%
    mutate(county = str_replace(county, "00", paste(i)))
}

for (i in 44:51) {
  substate[substate$state == i, ] <- filter(substate, state == i) %>%
    mutate(county = str_replace(county, "00", paste(i)))
}

for (i in 53:56) {
  substate[substate$state == i, ] <- filter(substate, state == i) %>%
    mutate(county = str_replace(county, "00", paste(i)))
}

# Add State Names & Align Substate Names across substate and ideations datasets
substate <- substate %>%
  mutate(state_name = fips(substate$state, to = "Name"),
         sbst16n = paste(state_name, sbst16n))

# Download Ideation Data
data_folder <- file.path(
  gsub("\\\\","/", gsub("OneDrive - ","", Sys.getenv("OneDrive"))), 
  "Health and Social Equity - SJP - BHN Score Creation",
  "Data", "Raw")

ideations <- 
  read.csv(file.path(data_folder, "ideation_nsduh.csv"))

# We only want Data for the 18+ Age group
ideations <- ideations %>%
  filter(age_group == "18 or Older")

# Merge Substate Data and Ideation Data
# Select relevant variables
# Rename Variables for Clarity 
ideation_county <- left_join(substate, ideations,
                             by = c("sbst16n" = "geography")) %>%
  select(-c(2:6, 8, 10:12, 14:15)) %>%
  select(c(3, 1, 2, 4)) %>%
  rename(substate_region = sbst16n,
         county_FIP = county, ideation_percent = estimate) 
  
# Export Final Ideations per county Dataset
data_folder <- file.path(
  gsub("\\\\","/", gsub("OneDrive - ","", Sys.getenv("OneDrive"))), 
  "Health and Social Equity - SJP - BHN Score Creation",
  "Data", "Preprocessed")

write.csv(ideation_county, file = file.path(data_folder,
                                 "ideation_county.csv"), row.names = F, na = "")














