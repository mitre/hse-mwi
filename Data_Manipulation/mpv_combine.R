# Combining Mapping Police Violence Data
# Using Harmonic means, uniform weights between ZCTA and County
# By Karen Jiang
# Originated on: 6/11/21

library(dplyr)
library(tidyr)
library(purrr)
library(tidycensus)
library(lubridate)

# Read in MPV Datasets & Crosswalks----

source(file = "Processing_Pipeline/crosswalk_func.R")

# Set data folder to One Drive Path
data_folder <- file.path(
  gsub("\\\\","/", gsub("OneDrive - ","", Sys.getenv("OneDrive"))), 
  "Health and Social Equity - SJP - BHN Score Creation",
  "Data")


# Read & Clean Mapping Police Violence Data -----
# Data last pulled January 24, 2023
# https://mappingpoliceviolence.org/
mpv <- read.csv(file.path(data_folder,"Raw", 
                          "Mapping_Police_Violence_01_24_23.csv"), )

# Reformatting dates
mpv$date_orig <- mpv$date
mpv$date <- as.Date(mpv$date_orig, "%m/%d/%Y")
mpv$year <- year(mpv$date)
mpv$month <- month(mpv$date)

# Standardizing ZIP Codes 
mpv$zip <- str_pad(as.character(mpv$zip), width = 5, side = "left", pad = "0")

# Removing any rows with ZIP Codes that are longer than 5 digits
mpv$ziplength <- nchar(mpv$zip)
mpv <- mpv[mpv$ziplength == 5,]
mpv$ziplength <- NULL

# Fixed labeling in Race Category 
mpv <- mpv %>%
  mutate(`race` = case_when(`race` == "white" ~ "White",
                                     `race` == "Unknown Race" ~ "Unknown race",
                                     TRUE ~ as.character(`race`))) %>%
  # Select relevant vars
  select(`race`, year, zip, state, county) %>%
  # Remove rows with missing data
  na.omit() 


# Pull ACS denominators -----

# Getting ACS population data from 2017-2021 for total and black pop
years <- 2017:2021

get_pop_data <- function(year){
  get_acs(geography = "zcta", 
          survey = "acs5",
          variables = c(total_pop = "B01001_001E",
                        black_pop = "B01001B_001E"),
          year = year)
}

pop_data_years <- map_df(years, ~ get_pop_data(.x), .id = "year") %>%
  separate(NAME, into = c("label", "ZCTA")) %>%
  mutate(year = as.numeric(year) + 2016) 

all_pop_data_years <- pop_data_years %>% 
  filter(variable == "B01001_001") %>%
  select(year, ZCTA, estimate) 

black_pop_data_years <- pop_data_years %>% 
  filter(variable == "B01001B_001") %>%
  select(year, ZCTA, estimate) 


# Converting ZIPs to ZCTA & select relevant columns
mpv <- mpv %>%
  left_join(zip_cw, by = c("zip" = "ZIP_CODE")) %>%
  filter(year <= 2021,
         year >= 2017,
         !is.na(ZCTA))

# Clean Up MPV Data -------

# Count number of events per zip code per year in MPV
all_counts <- mpv %>%
  group_by(ZCTA, county, state, year) %>%
  count()

# Count number of events per zip code per year for Black Victims in MPV
black_counts <- mpv %>%
  filter(`race` == "Black") %>%
  group_by(ZCTA, county, state, year) %>%
  count()


# Join ACS Population denominators to MPV data
all_joined <- left_join(all_pop_data_years, all_counts, by = c("year", "ZCTA"))

black_joined <- left_join(black_pop_data_years, black_counts, by = c("year", "ZCTA"))

# Create Final Scores -----

# Function Description: Calculate the county-level numerator and denominator for
# police killings, aggregates using population weights 

# Input: Df of joined MPV counts with ACS population denominators 

# Output: Returns data_county df with numerator (counts) and denominator (py
# person-years) for County GEOID across 5 years
create_county_values <- function(data_joined){
  data_county <- data_joined %>%
    left_join(county_cw, by = c("ZCTA"= "ZCTA5")) %>%
    group_by(GEOID, year) %>%
    summarize(county_counts = sum(n*AREALAND_PART/AREALAND_ZCTA5_20*100/100, 
                                  na.rm = T),
              county_py = sum(estimate * AREALAND_PART/AREALAND_ZCTA5_20*100/100,
                              na.rm = T)) 
  
  return(data_county)
}  

# Function Description: Calculates the ZCTA level harmonic mean by combining ZCTA
# rates and County rates using fixed weights

# Inputs: 1) Df with county level numerator denominator
# 2) Df of joined MPV counts with ACS population denominators 

# Output: Returns score DF with harmonic weight values along with ZCTA / County
# numerators, denominators, and weights. Useful for validation 
create_harmonic_mean <- function(data_county, data_joined){
  scores <- data_joined %>%
    left_join(county_cw, by = c("ZCTA" = "ZCTA5")) %>%
    left_join(data_county, by = c("GEOID","year")) %>%
    group_by(ZCTA) %>%
    
    # Calculates the numerators and denominators for both ZCTA and County summarized across years 
    summarize(py_zcta = sum(estimate, na.rm = T),
              k_zcta = sum(n, na.rm = T),
              py_county = sum(county_py, na.rm = T),
              k_county = sum(county_counts, na.rm = T)
    ) %>%
    
    # Removes 167 PR ZCTAs where counties are smaller than the ZCTA?
    mutate(ind = py_zcta <= py_county) %>%
    filter(ind == T) %>% select(-ind) %>%
    
    # Create fixed county and ZCTA weights
    mutate(w_zcta = 0.75,
           w_county = 0.25,
           
           # Harmonic mean (fixed weights) 
           harmon_mean_f = case_when(k_zcta + k_county == 0 ~ 0,
                                     k_zcta == 0 & k_county != 0 ~ 1/(py_county/k_county),
                                     TRUE ~ 1/((w_zcta*py_zcta/k_zcta) + (w_county*py_county/k_county)))
    )
  
  return(scores)
}

all_county <- create_county_values(all_joined)  
black_county <- create_county_values(black_joined) 

all_scores <- create_harmonic_mean(all_county, all_joined)
black_scores <- create_harmonic_mean(black_county, black_joined)

all <- all_scores %>% select(ZCTA, harmon_mean_f)
black <- black_scores %>% select(ZCTA, harmon_mean_f)

colnames(all) <- c("ZCTA", "policeviolence_harmonicmean_pop")
colnames(black) <- c("ZCTA", "policeviolence_harmonicmean_black")

mpv_data <- full_join(all, black, by = "ZCTA")

# Write out ----

data_folder <- file.path(
  gsub("\\\\","/", gsub("OneDrive - ","", Sys.getenv("OneDrive"))), 
  "Health and Social Equity - SJP - BHN Score Creation",
  "Data", "Preprocessed")

write.csv(mpv_data, 
          file = file.path(
            data_folder,
            "MPV_ZCTA_policeviolence.csv"
          ), 
          row.names = F, 
          na = "")
