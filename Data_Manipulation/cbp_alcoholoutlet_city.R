# Alcohol Outlet Density - Container Methods for CBP Data (County Business Patterns)
# By Karen Jiang
# Originated on: 6/8/21

# NOTE: keeping this as a separate script, since we may come back to this in future


library(dplyr)
library(tidycensus)
library(censusapi)
library(readxl)

source("Processing_Pipeline/crosswalk_func.R")

# Set Data Folder 
data_folder <- file.path(
  gsub("\\\\","/", gsub("OneDrive - ","", Sys.getenv("OneDrive"))), 
  "Health and Social Equity - SJP - BHN Score Creation",
  "Data", "Raw")

resource_folder <-file.path(
  gsub("\\\\","/", gsub("OneDrive - ","", Sys.getenv("OneDrive"))), 
  "Health and Social Equity - SJP - BHN Score Creation",
  "Data", "Resources")

grocery_states <- read_xlsx(file.path(resource_folder, "BWL_Grocery_Laws.xlsx"), skip = 1)
grocery_states[,3:5] <- ifelse(grocery_states[,3:5] == "Y", TRUE, FALSE)
grocery_states$any_alc_sales <- ifelse(rowSums(grocery_states[,3:5] , na.rm = T) > 0, TRUE, FALSE)

# Read in and clean zip code according to common mistakes
# inputs:
#   fn: file name (including path)
#   zip_cn: zip code column name
# outputs: read and cleaned data frame
read_zips <- function(fn, zip_cn){
  # read in specified file
  zip_df <- read.csv(fn,
                     colClasses = setNames("character", zip_cn))
  
  # remove all dashes
  zip_df[, zip_cn] <- gsub("-", "", zip_df[,zip_cn])
  
  # pad input zips with leading 0s
  zip_df[, zip_cn] <- str_pad(zip_df[, zip_cn], 5, pad = "0")
  
  # get rid of weirdly long ones
  zip_df[, zip_cn] <- substr(zip_df[, zip_cn], 1, 5)
  
  return(zip_df)
}

# Load all us cities
us_cities <- read_zips(
  file.path(resource_folder, "US_cities.csv"),
  "postal.code" 
)
# Remove duplicates (US cities) and add rownames
us_cities <- us_cities[!duplicated(us_cities$postal.code),]
rownames(us_cities) <- us_cities$postal.code

# Load additional US cities
addl_us_cities <- read_zips(
  file.path(resource_folder, "Additional_ZIP_US_Cities.csv"),
  "ZIP_CODE" 
)
rownames(addl_us_cities) <- addl_us_cities$ZIP_CODE


# Load in CBP data ----------
  # NAICS Codes https://www.naics.com/code-search/?naicstrms=liquor)
  # Beer, Wine, and Liquor Stores = 445310
  # Convenience Stores = 445120
  # Convenience Stores & Gas Stations = 447110
  # Grocery Stores = 445110

  # Drinking Places On Premise (not used) = 772410

get_zbp <- function(naics, name){
  dat <- getCensus(name = "zbp",
                   key = Sys.getenv("CENSUS_API_KEY"),
                   vintage = "2018",
                   vars = c("ESTAB","ZIPCODE"),
                   NAICS2017 = naics
  ) %>%
    
    # Grabbing the max establishment size from each ZIP Code group. 
    # Duplicate values are establishment counts by employee size.  
    # Double checked with the original data and was an exact match.
    group_by(ZIPCODE) %>%
    arrange(desc(ESTAB)) %>%
    filter(row_number() == 1) %>%
    select(ESTAB, ZIPCODE) 
  colnames(dat) <- c(name, "ZIPCODE")
  return(dat)
}

# Pull all counts for BWL, convenience stores, and convenience and gas stores
bwl <- get_zbp(445310, "bwl")
convenience <- get_zbp(445120, "convenience")
convenience_gas <- get_zbp(447110, "convenience_gas")

# Pull counts for grocery stores within allowed states
grocery <- get_zbp(445110, "grocery") #7052 zip codes

allowed_states <- sapply(grocery_states[grocery_states$any_alc_sale,"Abbrev"], 
                         as.character)
allowed_zips <- zip_cw %>% filter(STATE %in% allowed_states)
grocery <- grocery %>% filter(ZIPCODE %in% allowed_zips$ZIP_CODE) #6551 zip codes


# Combine outlets together into ESTAB count
all <- full_join(bwl, convenience, by = "ZIPCODE") %>%
  full_join(convenience_gas, by = "ZIPCODE") %>%
  full_join(grocery, by = "ZIPCODE") %>%
  mutate(ESTAB = sum(bwl, convenience, convenience_gas, grocery, na.rm = T)) 
  select(ESTAB, ZIPCODE)


# Using crosswalk function from zip to zcta, sums up many to one relationships.
cbp <-  zip_to_zcta(data.frame(all), 
                    geoid_col = "ZIPCODE", 
                    meas_col = "ESTAB", 
                    use_mean = F)

# [2021-12-21 20:33:33]: Converting ZIP code to ZCTA
# [2021-12-21 20:33:33]: 0.007981% (1) of ZIP codes not found in master crosswalk
# [2021-12-21 20:33:50]: Mapped ESTAB to ZCTA, 62.14% (20500) missing/not mapped


# Get population denominators from ACS ------
pop <- get_acs(geography = "zcta",
               output = "wide",
               year = 2019,
               survey = "acs5",
               variables = "B01001_001",
               geometry = F
) %>%
  select(GEOID, B01001_001E)

cbp <- full_join(cbp, pop, by = c("ZCTA" = "GEOID"))


cbp$City <- us_cities[cbp$ZCTA, "place.name"]
cbp$State <- us_cities[cbp$ZCTA, "state.code"]


cbp_city <- cbp %>% group_by(City, State) %>%
  summarize(ESTAB = sum(ESTAB, na.rm = T),
            pop = sum(B01001_001E, na.rm = T)) %>%
  # Calculating measure value = # outlets / population in ZCTA%
  mutate(alcoholoutlet_pop = ESTAB/pop,
         # Set NA alcohol outlets to 0
         alcoholoutlet_pop = ifelse(is.na(alcoholoutlet_pop), 0, alcoholoutlet_pop),
         # Set max for areas with greater than 100 outlets per 100,000 (10 Cities)
         alcoholoutlet_pop = ifelse(alcoholoutlet_pop > 0.01, 0.01, alcoholoutlet_pop))


final <- left_join(cbp, cbp_city, by = c("City", "State")) %>%
  select(ZCTA, alcoholoutlet_pop)


# write out ----
data_folder <- file.path(
  gsub("\\\\","/", gsub("OneDrive - ","", Sys.getenv("OneDrive"))), 
  "Health and Social Equity - SJP - BHN Score Creation",
  "Data", "Preprocessed")

write.csv(final, 
          file = file.path(
            data_folder,
            "CBP_ZCTA_AlcoholOutlet.csv"
          ), 
          row.names = F, 
          na = "")
