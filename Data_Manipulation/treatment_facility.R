# Library Download
library(tidyverse)
library(geosphere) # Computing distances between point data
library(readxl)
library(tidycensus)
library(cdlTools)
library(sp)
library(sf)
library(tigris)
options(tigris_use_cache = TRUE)

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

# Establish census tracts & polygons
states <- c(1:2, 4:6, 8:9, 10:13, 15:42, 44:51, 53:56)
poly <- get_decennial(geography = "tract", state = states,
                      variables = "P012001", year = 2010, geometry = T) %>%
  mutate(county = substr(GEOID, 1, 5))

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
ct <- ct %>%
  inner_join(thresh, by = c("tract" = "GEOID")) %>%
  select(c("tract", "POPULATION", "LATITUDE", "LONGITUDE", "Distance")) %>%
  relocate(c("tract", "LATITUDE", "LONGITUDE", "POPULATION", "Distance")) %>%
  st_as_sf(coords = c("LONGITUDE", "LATITUDE"),
           crs = st_crs(poly))

# Select/Remove Relevant Variables in Facility Data
mh_fac <- mh_fac %>%
  select(name1, zip, county, latitude, longitude, type_facility)
sa_fac <- sa_fac %>%
  select(name1, zip, county, latitude, longitude, type_facility)

# Convert point data into workable column for MH Facilities
mh_fac <- mh_fac %>%
  st_as_sf(coords = c("longitude", "latitude"),
           crs = st_crs(poly))

# Add Census Tracts to MH Facility Dataset
mh_fac <- st_join(mh_fac, poly)

# Return rows with no Census tracts
mh_fac[is.na(mh_fac$GEOID),]

# Determine which rows contain NA values for the GEOID
which(is.na(mh_fac$GEOID), arr.ind=TRUE)
 
# Manually add in Census Tracts in the 50 states for GEOIDs that were reported NA:
mh_fac[10552, 6] <- "02016000200" # One for Alaska
mh_fac[10583, 6] <- "15007040300" # One for Hawaii

# Remove irrelevant variables and incomplete cases
mh_fac <- mh_fac %>%
  select(c("name1", "zip", "county.y", "type_facility", "GEOID")) %>%
  rename("tract" = "GEOID", "county" = "county.y")

mh_fac <- mh_fac[complete.cases(mh_fac$tract),]

# Convert point data into workable column for SA Facilities
sa_fac <- sa_fac %>%
  st_as_sf(coords = c("longitude", "latitude"),
           crs = st_crs(poly))

# Add Census Tracts to SA Facility Dataset
sa_fac <- st_join(sa_fac, poly)

# Return rows with no Census tracts
sa_fac[is.na(sa_fac$GEOID),]

# Determine which rows contain NA values for the GEOID
which(is.na(sa_fac$GEOID), arr.ind=TRUE)

# Manually add in Census Tracts in the 50 states for GEOIDs that were reported NA:
sa_fac[6657, 6] <- "26033970600" # One for Michigan 
sa_fac[12045, 6] <- "06037800506" # One for California
sa_fac[13896, 6] <- "41007950300" # One for Oregon
sa_fac[14114, 6] <- "02016000100" # One for Alaska
sa_fac[14161, 6] <- "15009030902" # One for Hawaii
sa_fac[14169, 6] <- "15009031700" # Another for Hawaii

# Remove irrelevant variables and incomplete cases
sa_fac <- sa_fac %>%
  select(c("name1", "zip", "county.y", "type_facility", "GEOID")) %>%
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
  select(-c("geometry.y"))

# Merge Distance Threshold into SA Dataset
sa_fac <- left_join(sa_fac, as.data.frame(ct), by = "tract")

# Return rows with no Distance Thresholds/Population Variable
sa_fac[is.na(sa_fac$Distance),]

# Determine which rows contain NA values for the GEOID
which(is.na(sa_fac$Distance), arr.ind=TRUE)

# Manually add in Distance and Population values
# Determined from US Census & Distance Thresholds of other CTs in same county
sa_fac[3371, 7] <- 14177 # Population Value for Shannon County
sa_fac[3371, 8] <- 60 # Distance Threshold for Shannon County

sa_fac <- sa_fac %>%
rename(geometry = "geometry.x") %>%
  select(-c("geometry.y"))

# Build a for loop to create r values for first 1000 MH treatment centers
# without taking proximity into account
# Create r column in MH Facility data set
mh_fac <- mh_fac %>%
  add_column(r = 0)

# Step 1 of 2 Step Floating Catchment Area (FCA) Methodology
for (i in 1:1000) {
  mh_fac_man[i,]$r <- 
    1/sum(subset(cbind(
      ct,
      raster::pointDistance(
        mh_fac_man[i,],
        ct,
        lonlat = T) * .0006213712),
      raster::pointDistance(
        mh_fac_man[i,],
        ct,
        lonlat = T) * .0006213712 < mh_fac_man$Distance[[i]])$POPULATION)
}

# Build a function for Step 1 of the 2 Step FCA Method
step_1 <- function (x) {
  # Build a for loop while subseting by proximity - only look at CT centroids
  # within +/- 1 lat/long of facility
  # Create for loop
  ct <- ct %>%
    add_column(d = as.numeric(NA))
  # Keep the coordinates as well (no need to compute too many times)
  ct_coord <- st_coordinates(ct)
  fac_coord <- st_coordinates(x)
  start <- Sys.time()
  for (i in 1:1000) {
    # print every 50, so you know it's doing something :)
    if (i %% 50 == 0){
      print(paste0("[", Sys.time(), "] Currently on iteration: ", i))
    }
    
    # Compute distances between facility and CT centroids within +/-1 proximity
    # for first 1000 MH treatment centers
    # Keeping the logical so it only has to be done once
    prox_log <- ct_coord[,2] < fac_coord[i,2] + 1 &
      ct_coord[,2] > fac_coord[i,2] - 1 &
      ct_coord[,1] < fac_coord[i,1] + 1 &
      ct_coord[,1] > fac_coord[i,1] - 1
    
    ct$d[prox_log] <- 
      raster::pointDistance(
        x[i,], 
        ct[prox_log, ], lonlat = T) * .0006213712
    # Calculate the r value
    # Step 1 of 2 Step Floating Catchment Area (FCA) Methodology
    x$r[i] <- 1/sum(filter(ct, d < x$Distance[i])$POPULATION)
    
    # Preallocate and reuse distance values
    ct$d[prox_log] <- NA
  }
  end <- Sys.time()
  print(end - start)
  x
}

# Create R values for MH dataset
mh_fac <- step_1(mh_fac)

# Create R values for SA dataset
sa_fac <- step_1(sa_fac)
