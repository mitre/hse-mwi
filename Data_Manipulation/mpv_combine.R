# Combining Mapping Police Violence Data
# By Karen Jiang
# Originated on: 6/11/21

library(tidyverse)
library(tidycensus)
library(lubridate)


# Read in datasets ----

# Set data folder to One Drive Path
data_folder <- file.path(
  gsub("\\\\","/", gsub("OneDrive - ","", Sys.getenv("OneDrive"))), 
  "Health and Social Equity - SJP - BHN Score Creation",
  "Data")

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

# Read in crosswalk files -----
zip_to_zcta <- read.csv(file.path(data_folder, "Resources", "Zip_to_zcta_crosswalk_2020.csv"))
zip_to_zcta$ZIP_CODE = str_pad(zip_to_zcta$ZIP_CODE,
                               width = 5,
                               side = "left", 
                               pad = "0")

zcta_to_county <- read.csv(file.path(data_folder, "Resources", "zcta_county_rel_10.txt")) %>%
  select(ZCTA5, GEOID, ZPOPPCT)
zcta_to_county$ZCTA5 <- str_pad(zcta_to_county$ZCTA5,
                                width = 5,
                                side = "left",
                                pad = "0")
zcta_to_county$GEOID <- str_pad(zcta_to_county$GEOID,
                                width = 5,
                                side = "left",
                                pad = "0")

# Pull ACS denominators -----

# Getting ACS population data from 2013-2019 for total and black pop
  years <- 2013:2019
  
  get_pop_data <- function(year){
    get_acs(geography = "zcta", 
            survey = "acs5",
            variables = c(total_pop = "B01001_001E",
                          black_pop = "B01001B_001E"),
            year = year)
  }
  
  pop_data_years <- map_df(years, ~ get_pop_data(.x), .id = "year")
  
  pop_data_years <- pop_data_years %>% 
    separate(NAME, into = c("label", "ZCTA")) %>%
    select(-variable, -moe, -label, -GEOID) %>%
    mutate(year = as.numeric(year) + 2012) 

  


# Clean Up MPV Data -------
  ## Improving the precision of estimates of the frequency of rare events (2005)
  ## Method 4: Inclusion of aggregated data collected at larger spatial or temporal scales
  ## https://lib.dr.iastate.edu/cgi/viewcontent.cgi?article=1060&context=stat_las_pubs




# Write out ----

data_folder <- file.path(
  gsub("\\\\","/", gsub("OneDrive - ","", Sys.getenv("OneDrive"))), 
  "Health and Social Equity - SJP - BHN Score Creation",
  "Data", "Preprocessed")

write.csv(housing_stress, 
          file = file.path(
            data_folder,
            "HUD_CT_housingstress.csv"
          ), 
          row.names = F, 
          na = "")