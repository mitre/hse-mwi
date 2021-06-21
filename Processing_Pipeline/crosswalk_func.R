# Convert Geographies to ZCTAs 
# By Hannah De los Santos
# Originated on: 6/21/21

# Note: using household based weighted averages

# load libraries and data ----

# load crosswalk files
county_cw <- read.csv(file.path("Crosswalk_Files", "zcta_county_rel_10.txt"),
                      colClasses = c(
                        "ZCTA5" = "character",
                        "STATE" = "character",
                        "COUNTY" = "character",
                        "GEOID" = "character"
                      ))
ct_cw <- read.csv(file.path("Crosswalk_Files", "zcta_tract_rel_10.txt"),
                  colClasses = c(
                    "ZCTA5" = "character",
                    "STATE" = "character",
                    "COUNTY" = "character",
                    "TRACT" = "character",
                    "GEOID" = "character"
                  ))

# also keep all unique zctas to generate a file
all_zctas <- unique(county_cw$ZCTA5)

