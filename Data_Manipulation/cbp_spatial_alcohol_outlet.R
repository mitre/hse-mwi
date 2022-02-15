# Alcohol Outlet Density - Spatial Accessibility Method for CBP Data (County Business Patterns)
# Library Load----
# Packages
library(dplyr)
library(tidycensus)
library(tidyr)
library(censusapi)
library(readxl)
library(sf)
library(raster)
library(ggplot2)
library(geosphere)
library(od)

# Data Import----
# Set system environment
# Sys.setenv("OneDrive" = "/Users/mstrahlman/The MITRE Corporation")

source("Processing_Pipeline/crosswalk_func.R")

# Set Data & Resource Folders
data_folder <- file.path(
  gsub("\\\\","/", gsub("OneDrive - ","", Sys.getenv("OneDrive"))), 
  "Health and Social Equity - SJP - BHN Score Creation",
  "Data", "Raw")

resource_folder <-file.path(
  gsub("\\\\","/", gsub("OneDrive - ","", Sys.getenv("OneDrive"))), 
  "Health and Social Equity - SJP - BHN Score Creation",
  "Data", "Resources")

# Import ZCTAs & polygons
load(file.path(resource_folder,"ZCTAs_shapefile_US.RData"))
zctas <- zips %>%
  st_as_sf(coords = c("longitude", "latitude"),
           crs = st_crs(poly)) %>%
  mutate(centroid = st_centroid(geometry)) %>%
  dplyr::select(ZCTA5CE10, centroid)
rm(zips)

# Spreadsheet with state by state Grocery Laws
grocery_states <- read_xlsx(file.path(resource_folder, "BWL_Grocery_Laws.xlsx"),
                            skip = 1)

# Data Cleaning----
# Create column with indicator if state allows ANY type of alc sales
grocery_states[,3:5] <- ifelse(grocery_states[,3:5] == "Y", TRUE, FALSE)
grocery_states$any_alc_sales <- ifelse(rowSums(grocery_states[,3:5] , na.rm = T) > 0, TRUE, FALSE)

# Function to get zbp code from api, and return df with zipcode FIPS and number of establishments   
get_zbp <- function(naics, name){
  dat <- getCensus(name = "cbp",
                   key = Sys.getenv("CENSUS_API_KEY"),
                   vintage = "2019",
                   vars = "ESTAB",
                   region = "zipcode:*",
                   NAICS2017 = naics
  ) %>%
    mutate(zip = paste0(zip_code)) %>%
    dplyr::select(zip, ESTAB) 
  colnames(dat) <- c("zip", name)
  return(dat)
}

# Pull all counts for BWL, convenience stores, and convenience and gas stores
bwl <- get_zbp(445310, "bwl") #4557 zip codes
convenience <- get_zbp(445120, "convenience") #3785 zip codes
convenience_gas <- get_zbp(447110, "convenience_gas") #11135 zip codes
grocery <- get_zbp(445110, "grocery") #7005 zip codes

# Pull counts for convenience, convenience/gas, grocery stores within allowed states
allowed_states <- sapply(grocery_states[grocery_states$any_alc_sales,"Abbrev"], 
                         as.character)
state_fips <- unique(fips_codes[,1:2])
allowed_zips <- zip_cw %>% 
  left_join(state_fips, by = c("STATE" = "state")) %>%
  filter(STATE %in% allowed_states)

convenience <- convenience %>% filter(zip %in% allowed_zips$ZIP_CODE) #3459 zip codes
convenience_gas <- convenience_gas %>% filter(zip %in% allowed_zips$ZIP_CODE) #10745 zip codes
grocery <- grocery %>% filter(zip %in% allowed_zips$ZIP_CODE) #6542 zip codes

# Combine outlets together into ESTAB count
all <- full_join(bwl, convenience, by = "zip") %>%
  full_join(convenience_gas, by = "zip") %>%
  full_join(grocery, by = "zip") 
all[is.na(all)] <- 0
all <- all %>%
  mutate(ESTAB = bwl + convenience + convenience_gas + grocery) %>%
  dplyr::select(ESTAB, zip)

# Convert zips to zctas
all <- all %>%
  left_join(zip_cw, by = c("zip" = "ZIP_CODE")) %>%
  left_join(zctas, by = c("ZCTA" = "ZCTA5CE10")) %>%
  mutate(centroid = st_centroid(geometry)) %>%
  dplyr::select(ESTAB, ZCTA, centroid) 
all <- st_as_sf(all)

# Combine ZCTAs with multiple zip codes
all <- all %>% 
  group_by(ZCTA) %>% 
  summarize(sum(ESTAB)) %>%
  rename(outlets = `sum(ESTAB)`)

# Merge ESTABs into zctas dataset
zctas <- zctas %>%
  left_join(as.data.frame(all), by = c("ZCTA5CE10" = "ZCTA")) %>%
  dplyr::select(-c(centroid.y)) %>%
  rename(centroid = centroid.x) %>%
  mutate(outlets = ifelse(is.na(outlets), 0, outlets))

# Calculate distances to the x (# of outlets) number of nearest zctas
# Temporarily Replace 0 outlets for 1 outlet
zctas$outlets <- as.numeric(plyr::revalue(as.character(zctas$outlets),
                                          c("0" = "1")))

# Add necessary amount of rows based on number of outlets
zctas_exp <- zctas %>%
  uncount(outlets, .remove = F)

# Replace 1 outlets with 0 outlets
zctas_exp$outlets <- as.numeric(plyr::revalue(as.character(zctas_exp$outlets),
                                          c("1" = "0")))

# Calculate Spatial Alcohol Outlet Density----
# Return x amount of closest distances
# Create Distance Dataset
distances <- data.frame(matrix(NA,
                               nrow = 0,
                               ncol = 2))
# Create Distance Variable
zctas <- cbind(zctas, d = NA)

for (i in 24398:nrow(zctas)) {
  # Only search zctas within +/-10 lat/long to save time and computational memory
  prox_log <- sfc_point_to_matrix(zctas$centroid)[,2] < zctas$centroid[[i]][2] + 10 &
    sfc_point_to_matrix(zctas$centroid)[,2] > zctas$centroid[[i]][2] - 10 &
    sfc_point_to_matrix(zctas$centroid)[,1] < zctas$centroid[[i]][1] + 10 &
    sfc_point_to_matrix(zctas$centroid)[,1] > zctas$centroid[[i]][1] - 10
  
  if (zctas$outlets[i] == 0) {
    distances <- rbind(distances, data.frame(X0 = 0,X1 = 0))
    
  } else {
    distances <- rbind(distances,
                       data.frame(X0 = sort(pointDistance(as_Spatial(zctas$centroid[i]),
                                          as_Spatial(zctas$centroid[prox_log]),
                                          lonlat = T))[2:(zctas$outlets[i] + 1)],
                                  X1 = zctas[prox_log,][order(zctas$d),]$outlets[2:(zctas$outlets[i] + 1)]))
  }
  # Preallocate and reuse distance values
  zctas$d[prox_log] <- NA
}

# Slot in nearest n (# of outlets) distances to each zcta
zctas_exp <- cbind(zctas_exp, distances) %>%
  rename(aod = X0,
         out_dist = X1) %>%
  mutate(aod = 1/aod) %>%
  mutate(aod = aod * out_dist) # multiply value by # of outlets

# Impute NaNs with 0s
zctas_exp$aod[is.nan(zctas_exp$aod)] <- 0

# Sum values per ZCTA
aod_values <- zctas_exp %>%
  group_by(ZCTA5CE10) %>%
  summarize(aod = sum(aod))

# Merge aod values into zcta dataset
zctas <- as.data.frame(zctas) %>%
  left_join(as.data.frame(aod_values), by = "ZCTA5CE10")

# Get population denominators from ACS 
pop <- get_acs(geography = "zcta",
               output = "wide",
               year = 2019,
               survey = "acs5",
               variables = "B01001_001",
               geometry = F
) %>%
  dplyr::select(GEOID, B01001_001E)

# Merge population values into zctas dataset
zctas <- left_join(zctas,
                   pop,
                   by = c("ZCTA5CE10" = "GEOID")) %>%
  rename(pop = "B01001_001E") %>%
  mutate(aod = aod * pop) # Account for population

# Impute NA values with 0
# This is due to no population value being reported and 0 outlets
zctas[is.na(zctas)] <- 0

# Distributions----
ggplot(zctas, aes(x = `^`(aod, 1/3))) +
  geom_density(color = "darkblue", fill = "lightblue") +
  labs(title = "Distribution of Alcohol Outlet Density by ZCTA",
       y = "Density",
       x = "Cubed Root of Alcohol Outlet Density")

# Write Out----
# Write Out Inverse Distances
data_folder <- file.path(
  gsub("\\\\","/", gsub("OneDrive - ","", Sys.getenv("OneDrive"))), 
  "Health and Social Equity - SJP - BHN Score Creation",
  "Data", "Preprocessed")

write.csv(zctas %>%
            rename(zcta = ZCTA5CE10) %>%
            dplyr::select(zcta, aod), 
          file = file.path(
            data_folder,
            "CBP_ZCTA_AlcoholOutlet_Spatial.csv"
          ), 
          row.names = F, 
          na = "")
