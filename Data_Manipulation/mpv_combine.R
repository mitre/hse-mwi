# Combining Mapping Police Violence Data
  # Using Harmonic means, uniform weights between ZCTA and County
# By Karen Jiang
# Originated on: 6/11/21

library(dplyr)
library(tidycensus)
library(lubridate)

# Read in MPV Datasets & Crosswalks----

# Set data folder to One Drive Path
data_folder <- file.path(
  gsub("\\\\","/", gsub("OneDrive - ","", Sys.getenv("OneDrive"))), 
  "Health and Social Equity - SJP - BHN Score Creation",
  "Data")

# Reading in crosswalk files
zip_to_zcta <- read.csv(paste0(data_folder, "/Resources/Zip_to_zcta_crosswalk_2020.csv"))
zip_to_zcta$ZIP_CODE = str_pad(zip_to_zcta$ZIP_CODE,
                               width = 5,
                               side = "left", 
                               pad = "0")

zcta_to_county <- read.csv(paste0(data_folder, "/Resources/zcta_county_rel_10.txt")) %>%
  select(ZCTA5, GEOID, ZPOPPCT)
zcta_to_county$ZCTA5 <- str_pad(zcta_to_county$ZCTA5,
                                width = 5,
                                side = "left",
                                pad = "0")
zcta_to_county$GEOID <- str_pad(zcta_to_county$GEOID,
                                width = 5,
                                side = "left",
                                pad = "0")


# Read & Clean Mapping Police Violence Data -----
    # Data last pulled June 25th, 2021
    # https://mappingpoliceviolence.org/
mpv <- readxl::read_xlsx(file.path(data_folder,"Raw", "MPVDatasetDownload.xlsx"))

# Reformatting dates
  mpv$`Date of Incident (month/day/year)` <- as.Date(mpv$`Date of Incident (month/day/year)`)
  mpv$year <- year(mpv$`Date of Incident (month/day/year)`)
  mpv$month <- month(mpv$`Date of Incident (month/day/year)`)

# Standardizing ZIP Codes 
  mpv$Zipcode <- str_pad(as.character(mpv$Zipcode), width = 5, side = "left", pad = "0")

# Removing any rows with ZIP Codes that are longer than 5 digits
  mpv$ziplength = nchar(mpv$Zipcode)
  mpv <- mpv[mpv$ziplength == 5,]
  mpv$ziplength <- NULL

# Fixed labeling in Race Category 
  mpv <- mpv %>%
    mutate(`Victim's race` = case_when(`Victim's race` == "white" ~ "White",
                                     `Victim's race` == "Unknown Race" ~ "Unknown race",
                                     TRUE ~ as.character(`Victim's race`))) %>%
  # Select relevant vars
    select(`Victim's race`, year, Zipcode, State, County) %>%
  # Remove rows with missing data
    na.omit() 


# Pull ACS denominators -----

# Getting ACS population data from 2013-2019 for total and black pop
  years <- 2015:2019
  
  get_pop_data <- function(year){
    get_acs(geography = "zcta", 
            survey = "acs5",
            variables = c(total_pop = "B01001_001E",
                          black_pop = "B01001B_001E"),
            year = year)
  }
  
  pop_data_years <- map_df(years, ~ get_pop_data(.x), .id = "year") %>%
    separate(NAME, into = c("label", "ZCTA")) %>%
    mutate(year = as.numeric(year) + 2012) 
  
  all_pop_data_years <- pop_data_years %>% 
    filter(variable == "B01001_001") %>%
    select(year, ZCTA, estimate) 
    
  black_pop_data_years <- pop_data_years %>% 
    filter(variable == "B01001B_001") %>%
    select(year, ZCTA, estimate) 
  

# Converting ZIPs to ZCTA & select relevant columns
  mpv <- mpv %>%
    left_join(zip_to_zcta, by = c("Zipcode" = "ZIP_CODE")) %>%
    filter(year <= 2019,
           year >= 2015,
           !is.na(ZCTA))
    
# Clean Up MPV Data -------

  # Count number of events per zip code per year in MPV
  all_counts <- mpv %>%
    group_by(ZCTA, County, State, year) %>%
    count()
  
  # Count number of events per zip code per year for Black Victims in MPV
  black_counts <- mpv %>%
    filter(`Victim's race` == "Black") %>%
    group_by(ZCTA, County, State, year) %>%
    count()
  
  
  # Join ACS Population denominators to MPV data
  all_joined <- left_join(all_pop_data_years, all_counts, by = c("year", "ZCTA"))
  
  black_joined <- left_join(black_pop_data_years, black_counts, by = c("year", "ZCTA"))
  
# Create Final Scores -----

  create_county_values <- function(data_joined){
  data_joined %>%
    left_join(zcta_to_county, by = c("ZCTA"= "ZCTA5")) %>%
    group_by(GEOID, year) %>%
    summarize(county_counts = sum(n*ZPOPPCT/100, na.rm = T),
              county_py = sum(estimate * ZPOPPCT/100, na.rm = T)) 
  }  
  
  create_harmonic_mean <- function(data_county, data_joined){
    data_joined %>%
      left_join(zcta_to_county, by = c("ZCTA" = "ZCTA5")) %>%
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
