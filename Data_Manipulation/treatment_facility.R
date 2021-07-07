# Library Download
library(tidyverse)
library(readxl)
library(tidycensus)
library(cdlTools)
library(sp)
library(sf)
library(tigris)

# Import Datasets
# SSACD to County FIPS Crosswalk
cw <- read_csv("https://data.nber.org/ssa-fips-state-county-crosswalk/2018/xwalk2018.csv")

# Census Tract Population and Point Data
ct <- read_csv("https://www2.census.gov/geo/docs/reference/cenpop2010/tract/CenPop2010_Mean_TR.txt") 

# County Distance Thresholds
data_folder <- file.path(
  gsub("\\\\","/", gsub("OneDrive - ","", Sys.getenv("OneDrive"))), 
  "Health and Social Equity - SJP - BHN Score Creation",
  "Data", "Raw")

thresh <- 
  read_excel(file.path(data_folder, "county_distance.xlsx"))
thresh <- thresh[c(-1, -2), ]

# Crosswalk SSACD to County 
thresh <- left_join(thresh, cw,
                    by = "SSACD")
thresh <- thresh[!is.na(thresh$`FIPS County Code`), ] %>%
  select(COUNTY, ST, "FIPS County Code", Distance) %>%
  relocate(COUNTY, ST, "FIPS County Code", Distance) 

# Mental Health Facility Treatment Centers
mh_fac <- read_csv(file.path(data_folder,
                               "SAMHSA_Locator/Mental_Health_Treament_Facility_listing_2021_05_18_134835.csv"))

# Substance Abuse Facility Treatment Centers
sa_fac <- read_csv(file.path(data_folder,
                             "SAMHSA_Locator/Substance_Use_Treament_Facility_listing_2021_05_18_134647.csv"))

# Download All Census Tracts
states <- c(1:2, 4:6, 8:9, 10:13, 15:42, 44:51, 53:56, 72)
census_tracts <- get_acs(geography = "tract",
                         variable = "B19013_001",
                         state = c(as.numeric(states)),
                         geometry = TRUE) %>%
  mutate(county = substr(GEOID, 1, 5))

# Create Census Tract Variable for Thresholds & add missing Alaska Census Tract
thresh <- thresh %>%
  left_join(census_tracts, by = c("FIPS County Code" = "county")) 

thresh$GEOID <- thresh$GEOID %>%
  replace_na("02270000100")

# Create Census Tract FIP Variable and Select/Remove Relevant Variables 
ct <- ct %>%
  mutate(tract = paste0(STATEFP, COUNTYFP, TRACTCE)) %>%
  select(tract, POPULATION, LATITUDE, LONGITUDE) %>%
  relocate(tract, POPULATION, LATITUDE, LONGITUDE)

# Merge CT Population Variable with CT Distance Thresholds
ct <- ct %>%
  inner_join(thresh, by = c("tract" = "GEOID")) %>%
  select(c("tract", "POPULATION", "LATITUDE", "LONGITUDE", "Distance")) %>%
  relocate(c("tract", "LATITUDE", "LONGITUDE", "POPULATION", "Distance"))

# Select/Remove Relevant Variables in Facility Data
mh_fac <- mh_fac %>%
  select(name1, zip, county, latitude, longitude, type_facility)
sa_fac <- sa_fac %>%
  select(name1, zip, county, latitude, longitude, type_facility)

# Insert Census Tract Associated with Each Facility
# Pull Census Tract Data State by State
poly <- get_decennial(geography = "tract", state = 01,
                      variables = "P012001", year = 2010, geometry = T)

# Convert point data into workable column
sa_fac <- sa_fac %>%
  sf::st_as_sf(coords = c("latitude", "longitude"),
               crs = sf::st_crs(census_tracts))
mh_fac <- mh_fac %>%
  st_as_sf(coords = c("longitude", "latitude"),
           crs = st_crs(poly))

# Add Census Tracts to Facility Dataset
mh_fac <- st_join(mh_fac, poly)



