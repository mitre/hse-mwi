# Generating Financial Accessibility
# By Hannah De los Santos
# Originated on: 7/29/21

# load libraries ----

library(censusapi)
library(stringr)

# supporting functions ----

# function to read in and clean zip code according to common mistakes
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

# load supporting data ----

resource_folder <-file.path(
  gsub("\\\\","/", gsub("OneDrive - ","", Sys.getenv("OneDrive"))), 
  "Health and Social Equity - SJP - BHN Score Creation",
  "Data", "Resources")

# NOTE: commenting out zip code related ideas for future
# # load all zip codes
# zip_cw <- read_zips(
#   file.path(resource_folder, "Zip_to_zcta_crosswalk_2020.csv"),
#   "ZIP_CODE")
# # filter out territories
# territories <- c("AS", "FM", "GU", "MH", "MP", "PW", "PR", "VI")
# zip_cw <- zip_cw[!zip_cw$STATE %in% territories,]
# 
# # load all us cities
us_cities <- read.csv(
  file.path(resource_folder, "US_cities.csv"),
  colClasses = c(
    "postal.code" = "character",
    "county.code" = "character"
  )
)
us_cities$county.code <- str_pad(us_cities$county.code, 3, pad = "0")
us_cities$postal.code <- str_pad(us_cities$postal.code, 5, pad = "0")
# remove duplicates (US cities) and add rownames
us_cities <- us_cities[!duplicated(us_cities$postal.code),]
rownames(us_cities) <- us_cities$postal.code
# 
# # load additional US cities
# addl_us_cities <- read_zips(
#   file.path(resource_folder, "Additional_ZIP_US_Cities.csv"),
#   "ZIP_CODE" 
# )
# rownames(addl_us_cities) <- addl_us_cities$ZIP_CODE

# load county crosswalk
county_cw <- read.csv(file.path(resource_folder, "zcta_county_rel_10.txt"),
                      colClasses = c(
                        "ZCTA5" = "character",
                        "STATE" = "character",
                        "COUNTY" = "character",
                        "GEOID" = "character"
                      ))
# remove duplicated counties -- this is for a list of all counties
county_cw <- county_cw[!duplicated(county_cw$GEOID),]

# load county names to fips codes -- taken from usmap library
county_fips <- read.csv(file.path(resource_folder, "county_name_to_fips.csv"),
                        colClasses = c("fips" = "character"))

# load mainstream services data ----

data_folder <- file.path(
  gsub("\\\\","/", gsub("OneDrive - ","", Sys.getenv("OneDrive"))), 
  "Health and Social Equity - SJP - BHN Score Creation",
  "Data", "Raw", "FDIC_NCUA")

# federal banking institutions
fed_banks <- read_zips(
  file.path(data_folder, "FDIC_locations_10_4_2021.csv"),
  "ZIP"
)
# also pad state and county number
fed_banks$STCNTY <- str_pad(fed_banks$STCNTY, 5, pad = "0")

# credit unions
cu_banks <- read_zips(
  file.path(
    data_folder, 
    "NCUA_Credit_Union_Branch_Information_03_2021.csv"),
  "PhysicalAddressPostalCode"
)
# filter out anything not in the 50 states
cu_banks <- cu_banks[
  cu_banks$PhysicalAddressStateCode != "" &
    !cu_banks$PhysicalAddressStateCode %in% 
    c("AS", "FM", "GU", "MH", "MP", "PW", "PR", "VI"),
  ]
# add county number 
cu_banks$STCNTY <-
  sapply(1:nrow(cu_banks), function(x){
    st_code <- tolower(cu_banks$PhysicalAddressStateCode[x])
    cty_name <- tolower(gsub(
      "Saint ", "St. ", gsub("St ", "St. ", 
                             cu_banks$PhysicalAddressCountyName[x])
    ))
    
    poss_fips <- county_fips$fips[grepl(st_code, 
                                        tolower(county_fips$abbr)) &
                                    grepl(cty_name,
                                          tolower(county_fips$county))]
    
    # if multiple come up, we want the closest 
    if (length(poss_fips) > 1){
      cty_suffix <- if (st_code == "la"){
        "parish"
      } else {
        "county"
      }
      
      # don't add county suffix if it's a city
      cty_check <- if (!grepl("city", cty_name)){
        paste0(cty_name, " ", cty_suffix)
      } else {
        cty_name
      }
      
      poss_fips <- poss_fips[which.min(
        c(adist(cty_check,
                tolower(county_fips$county[county_fips$fips %in% poss_fips])))
      )]
    } else if (length(poss_fips) == 0){
      # name is coded slightly wrong
      wrong_name_map <- c(
        "Prince Georges" = "Prince George's",
        "De Kalb" = "DeKalb",
        "La Porte" = "LaPorte",
        "St. Marys" = "St. Mary's",
        "Matanuska Susitna" = "Matanuska-Susitna",
        "Prince Of Wales Hyder" = "Prince Of Wales-Hyder",
        "Hoonah Angoon" = "Hoonah-Angoon",
        "Obrien" = "O'Brien",
        "De Soto" = "DeSoto",
        "De Witt" = "DeWitt",
        "La Salle" = "LaSalle"
      )
      names(wrong_name_map) <- tolower(names(wrong_name_map))
      wrong_name_map <- tolower(wrong_name_map)
      
      
      poss_fips <- county_fips$fips[grepl(st_code, 
                                          tolower(county_fips$abbr)) &
                                      grepl(wrong_name_map[cty_name],
                                            tolower(county_fips$county))]
    }
    
    return(poss_fips)
  } )
# for all the empty county names, find by the zip code
cu_banks$STCNTY[cu_banks$PhysicalAddressCountyName == ""] <-
  sapply(1:sum(cu_banks$PhysicalAddressCountyName == ""), function(x){
    zip_search <- 
      us_cities$postal.code ==
      cu_banks$PhysicalAddressPostalCode[
        cu_banks$PhysicalAddressCountyName == ""][x]
    
    paste0(
      fips(us_cities$state.code[zip_search]),
      us_cities$county.code[zip_search]
    )
  })

# naics_codes
naics_codes <- read.csv(file.path(
  data_folder, 
  "Alternative_Financial_Institutions_2017_NAICS_Codes.csv"))

# pull county business practices data ----

# locations of alternative financial services
# 14 classifications from the North American Industry Classification System (NAICS)
# payday, installment, and other alternative loans (52229107, 52229111, 52229813)
# auto and other title loans (52222002, 52229109)
# pawn brokers and rent-to-own locations (53221015, 53221017, 53221018, 53229921)
# tax filing services (54121301)
# check cashing (52232003)
# money orders (52232008, 52232009, 52232010)

# can't pull multiple naics codes at once
# preallocate full alternative services 
alt_service <- data.frame(
  "STCNTY" = county_cw$GEOID, 
  "Num_Alt_Services" = 0
)
rownames(alt_service) <- alt_service$STCNTY
for (nc in unique(naics_codes$NAICS_2017)){
  # grab the services for all zip codes
  cbp_service <- getCensus(
    name = "cbp",
    vars = c("ESTAB", "STATE", "COUNTY"), 
    vintage = 2019,
    region = "county:*",
    show_call = F,
    NAICS2017 = nc
  )
  cbp_service$STCNTY <- paste0(cbp_service$STATE, cbp_service$COUNTY)
  
  # if there are counties with some amount of services, add to the total count
  if (nrow(cbp_service) > 0){
    in_df <- cbp_service$STCNTY %in% alt_service$STCNTY
    
    # first add ZIP codes that already exist
    alt_service[as.character(cbp_service$STCNTY[in_df]), "Num_Alt_Services"] <- 
      alt_service[as.character(cbp_service$STCNTY[in_df]), "Num_Alt_Services"] +
      cbp_service$ESTAB[in_df]
    
    alt_service[as.character(cbp_service$STCNTY[!in_df]), "Num_Alt_Services"] <- 
      cbp_service$ESTAB[!in_df]
    alt_service$STCNTY <- as.character(rownames(alt_service))
  }
}

# aggregate financial accessibility metrics ----

# create the financial accessibility overall df
# already contains alternative services
fin_access <- alt_service

# preallocate mainstream services
fin_access$Num_Mainstream_Services <- 0
# count and add FDIC banks
fdic_count <- as.data.frame(table(as.character(fed_banks$STCNTY)), 
                            stringsAsFactors = F)
in_df <- fdic_count$Var1 %in% fin_access
# first add the ones that do exist
fin_access[fdic_count$Var1[in_df], "Num_Mainstream_Services"] <-
  fin_access[fdic_count$Var1[in_df], "Num_Mainstream_Services"] +
  fdic_count$Freq[in_df]
# then add the ones that don't
fin_access[fdic_count$Var1[!in_df], "Num_Mainstream_Services"] <-
  fdic_count$Freq[!in_df]
fin_access$STCNTY <- as.character(rownames(fin_access))

# count and add credit unions
cu_count <- as.data.frame(
  table(as.character(cu_banks$STCNTY)), 
  stringsAsFactors = F
)
in_df <- cu_count$Var1 %in% fin_access
# first add the ones that do exist
fin_access[cu_count$Var1[in_df], "Num_Mainstream_Services"] <-
  fin_access[cu_count$Var1[in_df], "Num_Mainstream_Services"] +
  cu_count$Freq[in_df]
# then add the ones that don't
fin_access[cu_count$Var1[!in_df], "Num_Mainstream_Services"] <-
  cu_count$Freq[!in_df]
fin_access$STCNTY <- as.character(rownames(fin_access))

# # do some clean-up
# # remove zip 99999 -- doesn't exist
# fin_access <- fin_access[fin_access$ZIP_CODE != "99999",]
# # remove the ones that aren't in america (non numeric)
# fin_access <- 
#   fin_access[!is.na(suppressWarnings(as.numeric(fin_access$ZIP_CODE))),]
# make sure all na's are 0
fin_access[is.na(fin_access)] <- 0

# aggregate up to the city level ----

# NOTE: KEEPING COMMENTED OUT FOR FUTURE CITY EXPANSION

# # add city and state names
# fin_access$City <- us_cities[fin_access$ZIP_CODE, "place.name"]
# fin_access$State <- us_cities[fin_access$ZIP_CODE, "state.code"]
# # add additional US cities -- manually done from ZIP lookup
# fin_access$City[fin_access$ZIP_CODE %in% addl_us_cities$ZIP_CODE] <- 
#   addl_us_cities[
#     fin_access$ZIP_CODE[fin_access$ZIP_CODE %in% addl_us_cities$ZIP_CODE], 
#     "City"]
# fin_access$State[fin_access$ZIP_CODE %in% addl_us_cities$ZIP_CODE] <- 
#   addl_us_cities[
#     fin_access$ZIP_CODE[fin_access$ZIP_CODE %in% addl_us_cities$ZIP_CODE], 
#     "State"]
# 
# # get rid of everything else without a state -- not in US
# fin_access <- fin_access[!is.na(fin_access$State),]
# 
# # aggregate by city and state
# fin_access$City_State <- paste0(fin_access$City, "///", fin_access$State)
# # count alt services and mainstream
# agg_services <- aggregate(Num_Alt_Services ~ City_State, fin_access, sum)
# agg_services <- cbind(
#   agg_services,
#   "Num_Mainstream_Services" = 
#     aggregate(Num_Mainstream_Services ~ City_State, fin_access, sum)[,2]
# )
# # propegate out to the ZIP codes
# rownames(agg_services) <- agg_services$City_State
# fin_access$Num_Alt_Services_City <- 
#   agg_services[fin_access$City_State, "Num_Alt_Services"]
# fin_access$Num_Mainstream_Services_City <- 
#   agg_services[fin_access$City_State, "Num_Mainstream_Services"]

# fin_access$Financial_Accessibility_Ratio_City <-
#   fin_access$Unprocessed_Fin_Access <- 
#   fin_access$Num_Alt_Services_City/fin_access$Num_Mainstream_Services_City

# aggregate up to county level and create score ----

# create raw financial accessibility score (subtract)
# negative == bad, positive == good
fin_access$Financial_Accessibility_County <-
  fin_access$Num_Mainstream_Services - fin_access$Num_Alt_Services

# for 0/0, they have the worst financial access -- make them lowest number - 1
fin_access$Financial_Accessibility_County[
  fin_access$Num_Alt_Services == 0 & 
    fin_access$Num_Mainstream_Services == 0
] <- min(fin_access$Financial_Accessibility_County) - 1

# write out score ----

# # clean up -- remove city/state
# fin_access <- fin_access[, !colnames(fin_access) %in% "City_State"]

data_folder <- file.path(
  gsub("\\\\","/", gsub("OneDrive - ","", Sys.getenv("OneDrive"))), 
  "Health and Social Equity - SJP - BHN Score Creation",
  "Data", "Preprocessed")

write.csv(fin_access, 
          file = file.path(data_folder,
                           "New_America_Financial_Accessibility.csv"), 
          row.names = F, 
          na = "")
