# Generating Financial Accessibility
# By Hannah De los Santos
# Originated on: 7/29/21

# load data and libraries ----

library(censusapi)
library(stringr)

resource_folder <-file.path(
  gsub("\\\\","/", gsub("OneDrive - ","", Sys.getenv("OneDrive"))), 
  "Health and Social Equity - SJP - BHN Score Creation",
  "Data", "Resources")

# load all zip codes
zip_cw <- read.csv(file.path(resource_folder, "Zip_to_zcta_crosswalk_2020.csv"),
                   colClasses = c("ZIP_CODE" = "character"))
# filter out territories
territories <- c("AS", "FM", "GU", "MH", "MP", "PW", "PR", "VI")
zip_cw <- zip_cw[!zip_cw$STATE %in% territories,]

data_folder <- file.path(
  gsub("\\\\","/", gsub("OneDrive - ","", Sys.getenv("OneDrive"))), 
  "Health and Social Equity - SJP - BHN Score Creation",
  "Data", "Raw", "FDIC_NCUA")

# federal banking institutions
fed_banks <- read.csv(
  file.path(data_folder, "FDIC_Institutions_7_29_2021.csv"),
  colClasses = c("ZIP" = "character")
)

# credit unions
cu_banks <- read.csv(file.path(
  data_folder, 
  "NCUA_Credit_Union_Branch_Information_03_2021.csv"),
  colClasses = c("PhysicalAddressPostalCode" = "character"))
# clean up zip codes
cu_banks$PhysicalAddressPostalCode <- 
  gsub("-.*", "", cu_banks$PhysicalAddressPostalCode)
# pad with leading 0s
cu_banks$PhysicalAddressPostalCode <- 
  str_pad(cu_banks$PhysicalAddressPostalCode, 5, pad = "0")

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
alt_service <- data.frame("ZIP_CODE" = zip_cw$ZIP_CODE, "Num_Alt_Services" = 0)
rownames(alt_service) <- alt_service$ZIP_CODE
for (nc in unique(naics_codes$NAICS_2017)){
  # grab the services for all zip codes
  zbp_service <- getCensus(
    name = "cbp",
    vars = c("ESTAB"), 
    vintage = 2019,
    region = "zipcode:*",
    show_call = F,
    NAICS2017 = nc
  )
  
  # if there are zip codes with some amount of services, add to the total count
  if (nrow(zbp_service) > 0){
    alt_service[as.character(zbp_service$zip_code), "Num_Alt_Services"] <- 
      alt_service[as.character(zbp_service$zip_code), "Num_Alt_Services"] +
      zbp_service$ESTAB
  }
}

# create overall financial accessibility score ----

# create the financial accessibility overall df
# already contains alternative services
fin_access <- alt_service

# preallocate mainstream services
fin_access$Num_Mainstream_Services <- 0
# count and add FDIC banks
fdic_count <- as.data.frame(table(as.character(fed_banks$ZIP)), 
                            stringsAsFactors = F)
fin_access[fdic_count$Var1, "Num_Mainstream_Services"] <-
  fin_access[fdic_count$Var1, "Num_Mainstream_Services"] +
  fdic_count$Freq
# count and add credit unions
cu_count <- as.data.frame(
  table(as.character(cu_banks$PhysicalAddressPostalCode)), 
  stringsAsFactors = F
)
fin_access[cu_count$Var1, "Num_Mainstream_Services"] <-
  fin_access[cu_count$Var1, "Num_Mainstream_Services"] +
  cu_count$Freq

# create financial accessibility score
fin_access$Financial_Accessibility_Ratio <- 
  fin_access$Num_Alt_Services/fin_access$Num_Mainstream_Services

# write out score ----

data_folder <- file.path(
  gsub("\\\\","/", gsub("OneDrive - ","", Sys.getenv("OneDrive"))), 
  "Health and Social Equity - SJP - BHN Score Creation",
  "Data", "Preprocessed")

write.csv(fin_access, 
          file = file.path(data_folder,
                           "New_America_Financial_Accessibility.csv"), 
          row.names = F, 
          na = "")