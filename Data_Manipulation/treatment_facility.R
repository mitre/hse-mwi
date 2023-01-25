# Load Libraries----
library(tidyverse)
library(readxl)
library(tidycensus) 
library(sf) 
library(geosphere) 
library(readr)
library(tibble)
library(tigris)

# Import and Crosswalk Data----
# SSACD to County FIPS Crosswalk
cw <- read_csv("https://data.nber.org/ssa-fips-state-county-crosswalk/2018/xwalk2018.csv")

# Census Tract Population and Point Data
ct <- read_csv("https://www2.census.gov/geo/docs/reference/cenpop2020/tract/CenPop2020_Mean_TR.txt") 

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

# Import Mental Health Facility Treatment Centers
mh_fac <- read_csv(file.path(data_folder,
                               "SAMHSA_Locator/Mental_Health_FindTreament_Facility_listing_2023_01_24_172825.csv"))

# Import Substance Abuse Facility Treatment Centers
sa_fac <- read_csv(file.path(data_folder,
                             "SAMHSA_Locator/Substance_Use_FindTreament_Facility_listing_2023_01_24_172855.csv"))

# Weights Folder
weights_folder <- file.path(
  gsub("\\\\","/", gsub("OneDrive - ","", Sys.getenv("OneDrive"))), 
  "Health and Social Equity - SJP - BHN Score Creation",
  "Data", "Raw", "SAMHSA_Locator")

# Download Weights
mh_weights <- read_csv(file.path(weights_folder,
                                  "SAMHSA_MHFacility_weights.csv"))

sa_weights <- read_csv(file.path(weights_folder,
                                 "SAMHSA_SUFacility_weights.csv"))

# Add weights in MH dataset
mh_fac <- tibble::add_column(mh_fac, weights = mh_weights$weight, .before = "psy")

# Add weights in SA dataset
sa_fac <- tibble::add_column(sa_fac, weights = sa_weights$weight, .before = "dt")

# Import census tracts & establish polygons
poly <- tigris::tracts(year = 2020, cb = T)
poly <- mutate(poly, county = substr(GEOID, 1, 5))

# Step 1 of Floating Catchment Area (FCA) Methodology----
# Build a Function to Calculate R value
# For each treatment center, search all CTs within the CT distance threshold
# and sum their populations to compute the treatment-to-popuation ratio (R)

# Build a function for Step 1 of the 2 Step FCA Method
# Inputs
#   data: dataset of facilities with specified point data
#   geo: dataset of geographical points
#   facil: the type of facility (MH, SA, etc) in character format
# Output  
#   Returns R value for each facility within the dataset of facilities

# Set the amount of rows to be iterated -- testing
# n <- 1000

step_1_fca <- function (data, geo, facil) {
  # Build a for loop while subseting by proximity - only look at CT centroids
  # within +/- 1 lat/long of facility
  # Create for loop
  geo <- geo %>%
    add_column(d = as.numeric(NA))
  # Keep the coordinates 
  geo_coord <- st_coordinates(geo)
  fac_coord <- st_coordinates(data)
  start <- Sys.time()
  for (i in 1:nrow(data)) {
    # print every 50
    if (i %% 50 == 0){
      print(paste0("[", Sys.time(), "] Currently on iteration: ", i))
    }
    # Output every 1000 values
    if (i %% 1000 == 0){
      write.csv(data, file = paste0("step1", facil, "_", i, ".csv"), row.names = F)
      }
    # Compute distances between facility and CT centroids within +/-1 proximity
    # for first 1000 MH treatment centers
    # Keep the logical 
    prox_log <- geo_coord[,2] < fac_coord[i,2] + 1 &
      geo_coord[,2] > fac_coord[i,2] - 1 &
      geo_coord[,1] < fac_coord[i,1] + 1 &
      geo_coord[,1] > fac_coord[i,1] - 1
    
    if (sum(prox_log) != 0) {
      geo$d[prox_log] <- 
      raster::pointDistance(
        data[i,], 
        geo[prox_log, ], lonlat = T) * .0006213712
    }
    # Calculate the r value
    # Step 1 of 2 Step Floating Catchment Area (FCA) Methodology
    data$R[i] <- 1/sum(filter(geo,
                              d < data$Distance[i])$POPULATION) * data$weights[i]
    
    # Preallocate and reuse distance values
    geo$d[prox_log] <- NA
  }
  end <- Sys.time()
  print(paste0("[", Sys.time(), "] Step 1 of the FCA took ", print(end - start),
               " seconds"))
  return(data)
}

# Step 2 of FCA Methodology----

# Build a function to Calculate A value
# Search all treatment centers that are within the distance threshold from 
# each CT and sum their r values per each CT 

# Inputs
#   data: dataset of faciltiies with specified r values
#   geo: dataset of geographical points
#   facil: the type of facility (MH, SA, etc) in character format
# Output  
#   Returns A value for each geography within the dataset of geographical points

step_2_fca <- function (data, geo, facil) {
  # Build a for loop while subseting by proximity - only look at facilities
  # within +/- 1 lat/long of CT Centroids
  data <- data %>%
    add_column(d = as.numeric(NA))
  # Keep coordinates
  geo_coord <- st_coordinates(geo)
  fac_coord <- st_coordinates(data)
  start <- Sys.time()
  for (i in 1:nrow(geo)) {
    # print every 50
    if (i %% 50 == 0){
      print(paste0("[", Sys.time(), "] Currently on iteration: ", i))
    }
    # Output every 1000 values
    if (i %% 1000 == 0){
      write.csv(data, file = paste0("step2", facil, "_", i, ".csv"), row.names = F)
    }
    # Compute distances between facility and CT centroids within +/-1 proximity
    # for first 1000 CT centroids
    # Keep the logical
    prox_log <- fac_coord[,2] < geo_coord[i,2] + 1 &
      fac_coord[,2] > geo_coord[i,2] - 1 &
      fac_coord[,1] < geo_coord[i,1] + 1 &
      fac_coord[,1] > geo_coord[i,1] - 1
    if (sum(prox_log) != 0) {
    data$d[prox_log] <- 
      raster::pointDistance(
        data[prox_log,], 
        geo[i, ], lonlat = T) * .0006213712
    }
    # Calculate the A value
    # Sum all r values within the distance threshold for each CT
    geo$A[i] <- sum(filter(data, d < geo[["Distance"]][i])$R)
    # Preallocate and reuse distance values
    data$d[prox_log] <- NA
  }
  end <- Sys.time()
  print(paste0("[", Sys.time(), "] Step 2 of the FCA took ", print(end - start),
               " seconds"))
  # Set A variable name based on dataset being used in function
  colnames(geo)[names(geo) == "A"] <- paste0("A",
                                                    "_",
                                                    facil)
  return(geo)
}

# Wrangle and Merge Data for Usability----
# Create Census Tract Variable for Thresholds 
thresh <- thresh %>%
  left_join(poly, by = c("FIPS County Code" = "county")) 

# Remove NAs created by Puerto Rico and one South Dakota county. The SD county
# is redundant of Fall River County
thresh <- na.omit(thresh)

# Create Census Tract FIP Variable and Select/Remove Relevant Variables 
ct <- ct %>%
  mutate(tract = paste0(STATEFP, COUNTYFP, TRACTCE)) %>%
  select(tract, POPULATION, LATITUDE, LONGITUDE) %>%
  relocate(tract, POPULATION, LATITUDE, LONGITUDE)

# Merge CT Population Variable with CT Distance Thresholds
# Convert coordinates to sf object
ct <- ct %>%
  inner_join(thresh, by = c("tract" = "GEOID")) %>%
  select(c("tract", "POPULATION", "LATITUDE", "LONGITUDE", "Distance")) %>%
  relocate(c("tract", "LATITUDE", "LONGITUDE", "POPULATION", "Distance")) %>%
  st_as_sf(coords = c("LONGITUDE", "LATITUDE"),
           crs = st_crs(poly))
# Remove duplicates from CT dataset
ct <- ct[!duplicated(ct$tract),]

# Select/Remove Relevant Variables in Facility Data
mh_fac <- mh_fac %>%
  select(c(name1, zip, county, latitude, longitude, type_facility), c(19:187))
sa_fac <- sa_fac %>%
  select(c(name1, zip, county, latitude, longitude, type_facility), c(19:239))

# Convert point data into workable column for MH Facilities
mh_fac <- mh_fac %>%
  st_as_sf(coords = c("longitude", "latitude"),
           crs = st_crs(poly)) %>%
  relocate(geometry, .after = type_facility)

# Add Census Tracts to MH Facility Dataset
mh_fac <- st_join(mh_fac, poly)

# Return rows with no Census tracts
mh_fac[is.na(mh_fac$GEOID),]

# Determine which rows contain NA values for the GEOID
which(is.na(mh_fac$GEOID), arr.ind=TRUE)

# remove them -- PR
mh_fac <- mh_fac[-which(is.na(mh_fac$GEOID), arr.ind=TRUE),]
 
# Manually add in Census Tracts in the 50 states for GEOIDs that were reported NA:
# mh_fac[10552, 175] <- "02016000200" # One for Alaska
# mh_fac[10583, 175] <- "15007040300" # One for Hawaii

# Remove irrelevant variables and incomplete cases
mh_fac <- mh_fac %>%
  select(c("name1", "zip", "county.y", "type_facility", "GEOID", "geometry"),
         c(6:174)) %>%
  rename("tract" = "GEOID", "county" = "county.y")

mh_fac <- mh_fac[complete.cases(mh_fac$tract),]

# Convert point data into workable column for SA Facilities
sa_fac <- sa_fac %>%
  st_as_sf(coords = c("longitude", "latitude"),
           crs = st_crs(poly)) %>%
  relocate(geometry, .after = type_facility)

# Add Census Tracts to SA Facility Dataset
sa_fac <- st_join(sa_fac, poly)

# Return rows with no Census tracts
sa_fac[is.na(sa_fac$GEOID),]

# Determine which rows contain NA values for the GEOID
which(is.na(sa_fac$GEOID), arr.ind=TRUE)

# remove them -- PR
sa_fac <- sa_fac[-which(is.na(sa_fac$GEOID), arr.ind=TRUE),]

# Manually add in Census Tracts in the 50 states for GEOIDs that were reported NA:
# sa_fac[6657, 227] <- "26033970600" # One for Michigan 
# sa_fac[12045, 227] <- "06037800506" # One for California
# sa_fac[13896, 227] <- "41007950300" # One for Oregon
# sa_fac[14114, 227] <- "02016000100" # One for Alaska
# sa_fac[14161, 227] <- "15009030902" # One for Hawaii
# sa_fac[14169, 227] <- "15009031700" # Another for Hawaii

# Remove irrelevant variables and incomplete cases
sa_fac <- sa_fac %>%
  select(c("name1", "zip", "county.y", "type_facility", "GEOID", "geometry"),
         c(6:226)) %>%
  rename("tract" = "GEOID", "county" = "county.y")

sa_fac <- sa_fac[complete.cases(sa_fac$tract),]

# Merge Distance Threshold into MH Dataset
mh_fac <- left_join(mh_fac, as.data.frame(ct), by = "tract")

# Determine which rows contain NA values for the Distance Thresholds/Population
which(is.na(mh_fac$Distance), arr.ind=TRUE)

# Remove sole row with missing distance/population - this census tract or FIPS
# county code (51515) was not available in the distance/population dataset
mh_fac <- mh_fac[complete.cases(mh_fac$Distance),] %>%
  rename(geometry = "geometry.x") %>%
  select(-c("geometry.y")) %>%
  relocate(POPULATION, .after = geometry) %>%
  relocate(Distance, .after = geometry)
           
# Merge Distance Threshold into SA Dataset
sa_fac <- left_join(sa_fac, as.data.frame(ct), by = "tract")

# Return rows with no Distance Thresholds/Population Variable
sa_fac[is.na(sa_fac$Distance),]

# Determine which rows contain NA values for the GEOID
which(is.na(sa_fac$Distance), arr.ind=TRUE)

# not manually adding in

# Manually add in Distance and Population values
# # Determined from US Census & Distance Thresholds of other CTs in same county
# sa_fac[3371, 228] <- 14177 # Population Value for Shannon County
# sa_fac[3371, 229] <- 60 # Distance Threshold for Shannon County

sa_fac <- sa_fac %>%
rename(geometry = "geometry.x") %>%
  select(-c("geometry.y")) %>%
  relocate(POPULATION, .after = geometry) %>%
  relocate(Distance, .after = geometry) %>%
  rename(c("ct_" = "ct")) # rename variable to avoid confusion with dataset

# Results----
# Create R values for MH dataset
mh_fac <- step_1_fca(mh_fac, ct, "mh") %>%
  relocate(R, .after = POPULATION) %>%
  mutate(R = ifelse(R < 0,0, R))

# Create R values for SA dataset
sa_fac <- step_1_fca(sa_fac, ct, "sa") %>%
  relocate(R, .after = POPULATION) %>%
  mutate(R = ifelse(R < 0, 0, R))

# Create A values using MH dataset
ct <- step_2_fca(mh_fac, ct, "mh")

# Create A values using SA dataset
ct <- step_2_fca(sa_fac, ct, "sa")

# Populate non-calculated R and A values as 0 -- test
# mh_fac$R[-c(1:n)] <- 0
# sa_fac$R[-c(1:n)] <- 0
# ct$A_mh[-c(1:n)] <- 0
# ct$A_sa[-c(1:n)] <- 0

# Distribution of R value for MH 
ggplot(mh_fac[1:nrow(mh_fac),], aes(R)) + 
  geom_histogram(aes(y = ..density..), color = "black", fill = "white") +
  geom_density(alpha = .2, fill = "#FF6666") 

# Distribution of R value for SA
ggplot(sa_fac[1:nrow(sa_fac),], aes(R)) + 
  geom_histogram(aes(y = ..density..), color = "black", fill = "white") +
  geom_density(alpha = .2, fill = "#FF6666") 

# Distribution of A value for MH
ggplot(ct[1:nrow(mh_fac),], aes(A_mh)) + 
  geom_histogram(aes(y = ..density..), color = "black", fill = "white") +
  geom_density(alpha = .2, fill = "#FF6666") 

# Distribution of A value for SA
ggplot(ct[1:nrow(sa_fac),], aes(A_sa)) + 
  geom_histogram(aes(y = ..density..), color = "black", fill = "white") +
  geom_density(alpha = .2, fill = "#FF6666") 

# Remove Geometry
ct <- ct %>%
  dplyr::select(-geometry) 
ct <- st_drop_geometry(ct)

# Export Facility Access Dataset
write.csv(ct,
           file = file.path(data_folder, "..", "Preprocessed",
                            "SAMHSA_CT_treatmentfacilityaccess.csv"),
           row.names = F, na = "")
