# Tidyverse for Data Manipulation
library(tidyverse)
# cdlTools for converting FIPs to state names
library(cdlTools)
# Haven for Data Import
library(haven)
# Tidycensus for attaining CTs
library(tidycensus)
# census_api_key("CENSUS_API_KEY")

# Download Substate level data that is delineated by Census Tract (7 states)
temp <- tempfile()
download.file("https://www.samhsa.gov/data/sites/default/files/cbhsq-reports/NSDUHsubstateRegionDefs2016B/substate_definitions_SAS.zip", 
              temp)
raw_tract <- read_sas(unz(temp, "substate_tract141516.sas7bdat"))

# Format state values to be 2 digits and county values to be 3 digits
# Format tract values based off of state and county values
raw_tract <-  raw_tract %>%
  mutate(state = str_pad(state, 2, pad = "0"),
         county = str_pad(county, 3, pad = "0"),
         tract = str_c(state, county, tract))

# Download Substate level data that is delineated only by County (44 states)
raw_county <- read_sas(unz(temp, "substate_county141516.sas7bdat"))
unlink(temp)

# Correct Format of County FIPs 
raw_county$county <- sprintf("%05d", raw_county$county) 

# Correct County FIPs for Single Digit States
states <- c(1:2, 4:6, 8:9)

for (i in states) {
  raw_county[raw_county$state == i, ] <- filter(raw_county, state == i) %>%
    mutate(county = str_replace(county, "00", paste("0", i, sep = "")))
}

# Correct County FIPs for Double Digit States

states2 <- c(10:13, 15:42, 44:51, 53:56)

for (i in states2) {
  raw_county[raw_county$state == i, ] <- filter(raw_county, state == i) %>%
    mutate(county = str_replace(county, "00", paste(i)))
}

# Add State Names & Align Substate Names across county and ideations datasets
raw_county <- raw_county %>%
  mutate(state_name = fips(state, to = "Name"),
         sbst16n = paste(state_name, sbst16n))

# Add State Names & Align Substate Names across tract and ideations datasets
raw_tract <- raw_tract %>%
  mutate(state_name = fips(state, to = "Name"),
         sbst16n = paste(state_name, sbst16n))
         
# Download Ideation Data
data_folder <- file.path(
  gsub("\\\\","/", gsub("OneDrive - ","", Sys.getenv("OneDrive"))), 
  "Health and Social Equity - SJP - BHN Score Creation",
  "Data", "Raw")

raw_ideations <- 
  read.csv(file.path(data_folder, "ideation_nsduh.csv"))

# We only want Data for the 18+ Age group
raw_ideations <- raw_ideations %>%
  filter(age_group == "18 or Older")

# Merge County Data and Ideation Data
# Select relevant variables
# Rename Variables for Clarity 
ideation_county <- left_join(raw_county, raw_ideations,
                             by = c("sbst16n" = "geography")) %>%
  select(-c("state", "aggflg16", "sbsta16n", "sbst16", "sbstag16",
            "sbstfg16", "outcome", "age_group", "years",
            "ci_lower", "ci_upper"), c("state_name", "sbst16n", 
                                       "county", "estimate")) %>%
  rename(substate_region = sbst16n,
         county_FIP = county, ideation_percent = estimate) 

# Merge Census Tract Data and Ideation Data
ideations_some_tracts <- left_join(raw_tract, raw_ideations,
                             by = c("sbst16n" = "geography")) %>%
  select(-c("state", "aggflg16", "sbsta16n", "sbst16", "sbstag16",
            "sbstfg16", "outcome", "age_group", "years", "sbst16n",
            "ci_lower", "ci_upper", "state_name", "county")) %>%
  relocate("tract", "estimate") %>%
  rename("suicideideation_pop" = "estimate", "census_tract" = "tract")

# Download All US Census Tracts & Create County Variable
census_tracts <- get_acs(geography = "tract",
                      variable = "B19013_001",
                      state = c(states, states2)) %>%
  mutate(county = substr(GEOID, 1, 5))
  
# Add Ideation Percentages to the Census Tract Level for County Data
# Remove Unnecessary Variables
# Select and Rename Variables for Clarity
# Remove states that are accounted for in "ideations_some_tracts" dataset 
ideation_tract <- left_join(census_tracts,
                            ideation_county,
                            by = c("county" = "county_FIP")) %>%
  select(-c("NAME", "variable", "estimate", "moe", "substate_region", "state_name", "county")) %>%
  select(c("GEOID", "ideation_percent")) %>%
  rename("census_tract" = "GEOID", "suicideideation_pop" = "ideation_percent") 

# Merge Data for all 50 states + DC
NSDUH_CT_suicideideation <- rbind(ideation_tract, ideations_some_tracts)

# 7 states were duplicated and assigned NAs due to not having ideation values
# in the "ideation county" dataset, becase they have values in the 
# "ideation_some_tracts" dataset
# Remove those duplicated cases from the above merge
NSDUH_CT_suicideideation <- 
  NSDUH_CT_suicideideation[complete.cases(NSDUH_CT_suicideideation), ]

# Export Final Ideations per Census Tract Dataset
data_folder <- file.path(
  gsub("\\\\","/", gsub("OneDrive - ","", Sys.getenv("OneDrive"))), 
  "Health and Social Equity - SJP - BHN Score Creation",
  "Data", "Preprocessed")

write.csv(NSDUH_CT_suicideideation, file = file.path(data_folder,
                                 "NSDUH_CT_suicideideation.csv"),
          row.names = F, na = "")














