# Generating Financial Accessibility
# By Hannah De los Santos
# Originated on: 7/29/21

# load data and libraries ----

library(censusapi)

data_folder <- file.path(
  gsub("\\\\","/", gsub("OneDrive - ","", Sys.getenv("OneDrive"))), 
  "Health and Social Equity - SJP - BHN Score Creation",
  "Data", "Raw", "FDIC_NCUA")

# federal banking institutions
fed_banks <- read.csv(file.path(data_folder, "FDIC_Institutions_7_29_2021.csv"))
# federal deposit
# ADDRESS street address
# CITY city
# COUNTY county
# STNAME state name
# STNUM state number
# ZIP zip code

# credit unions
cu_banks <- read.csv(file.path(
  data_folder, 
  "NCUA_Credit_Union_Branch_Information_03_2021.csv"))
# naics_codes
naics_codes <- read.csv(file.path(
  data_folder, 
  "Alternative_Financial_Institutions_2017_NAICS_Codes.csv"))

# pull county business practices data
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
alt_service <- data.frame()
for (nc in unique(naics_codes$NAICS_2017)){
  # grab the services for all zip codes
  zbp_service <- getCensus(
    name = "cbp",
    vars = c("ESTAB", "GEO_ID"), 
    vintage = 2019,
    region = "zipcode:*",
    show_call = T,
    NAICS2017 = nc
  )
  
  if (nrow(tmp) > 0){
    # STOP HERE
  }
}