# Create and Save Updated Shapefile
# By Hannah De los Santos
# Originated on: 12/22/21

# NOTE: this is also useful for Docker

# load libraries ----

library(readxl)
library(tigris)
library(sf)

# load data ----

# what indices are available?
index_types <- c("Population" = "pop",
                 "Black" = "black")

# folder where all the data and information for the pipeline is
data_folder <- file.path("Data")

# load measure registry -- first sheet
m_reg <- as.data.frame(
  read_excel(file.path(data_folder, "Metadata.xlsx"), sheet = 1)
)
# remove everything that doesn't have a numerator
m_reg <- m_reg[!is.na(m_reg$Numerator),]
rownames(m_reg) <- m_reg$Numerator

# load index weighting/output
info_df <- read.csv(
  file.path(data_folder, "Cleaned", "HSE_MWI_Data_Information.csv"),
  row.names = "Numerator"
)

# load county cw for zcta state county mapping
county_cw <- read.csv(
  file.path(data_folder, "Resources", "zcta_county_rel_10.txt"),
  colClasses = c(
    "ZCTA5" = "character",
    "STATE" = "character",
    "COUNTY" = "character",
    "GEOID" = "character"
  ))
# collapse so that there's one zcta for every row (states get collapsed by pipe)
cty_cw <- 
  aggregate(STATE ~ ZCTA5, data = county_cw, 
            FUN = function(x){paste(x, collapse = "|")})
rownames(cty_cw) <- cty_cw$ZCTA5
# get unique states
un_st <- lapply(
  strsplit(cty_cw$STATE, "|", fixed = T), unique)
# original state will be the first given -- FIX THIS BY GREATEST AREA LATER
cty_cw$STATE <- sapply(un_st, `[`, 1)
cty_cw$STATE_2 <- sapply(un_st, `[`, 2)
# add state name
data("fips_codes")
f_st <- setNames(unique(fips_codes$state_name),
                 unique(fips_codes$state_code))
cty_cw$STATE_NAME <- f_st[cty_cw$STATE]
# add all the counties and geoids as well
cty_cw$COUNTY <- aggregate(COUNTY ~ ZCTA5, data = county_cw, 
                           FUN = function(x){paste(x, collapse = "|")})[,2]
cty_cw$GEOID <- aggregate(GEOID ~ ZCTA5, data = county_cw, 
                          FUN = function(x){paste(x, collapse = "|")})[,2]

# MWI scores
# NOTE: may also save as RData for faster reading
mwi <- list()
mwi[["pop"]] <- read.csv(
  file.path(data_folder, "Cleaned", 
            "HSE_MWI_ZCTA_Mental_Wellness_Index_Population.csv"),
  colClasses = c("ZCTA" = "character")
)
# remove any empty zcta rows (miswriting?) -- TODO: fix in pipeline
mwi[["pop"]] <- mwi[["pop"]][mwi[["pop"]]$ZCTA != "",]
mwi[["black"]] <- read.csv(
  file.path(data_folder, "Cleaned", 
            "HSE_MWI_ZCTA_Mental_Wellness_Index_Black.csv"),
  colClasses = c("ZCTA" = "character")
)
# remove any empty zcta rows (miswriting?) -- TODO: fix in pipeline
mwi[["black"]] <- mwi[["black"]][mwi[["black"]]$ZCTA != "",]

# add cities/states to mwi
for (idx in index_types){
  mwi[[idx]][, colnames(cty_cw)[-1]] <- 
    cty_cw[mwi[[idx]]$ZCTA, -1]
}

# get zip code data -- ONLY ORIGINAL
# NOTE: cb = T will download a generalized file
zips <- zctas(cb = T)
zips <- st_transform(zips, crs = "+proj=longlat +datum=WGS84")

# create the geo data for leaflet
# NOTE: may want to do this ahead of time, if possible, when the base index is done
geodat <- list()
for (idx in index_types){
  geodat[[idx]] <-
    geo_join(zips, mwi[[idx]], by_sp = "GEOID10", by_df = "ZCTA", how = "left")

  # sort by state code
  geodat[[idx]] <- geodat[[idx]][order(geodat[[idx]]$STATE),]

  # add state and county (multiple counties for a zcta separated by pipes)
}

# saving for now, while things are stable
save(list = "geodat", 
     file = file.path(data_folder, 
                      "Cleaned", "HSE_MWI_ZCTA_full_shapefile_US.RData"))
