# Generating Financial Accessibility for Cities
# By Hannah De los Santos
# Originated on: 7/29/21

# NOTE: keeping this as a separate script, since we may come back to this in future

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

# load data ----

resource_folder <-file.path(
  gsub("\\\\","/", gsub("OneDrive - ","", Sys.getenv("OneDrive"))), 
  "Health and Social Equity - SJP - BHN Score Creation",
  "Data", "Resources")

# load all zip codes
zip_cw <- read_zips(
  file.path(resource_folder, "Zip_to_zcta_crosswalk_2020.csv"),
  "ZIP_CODE")
# filter out territories
territories <- c("AS", "FM", "GU", "MH", "MP", "PW", "PR", "VI")
zip_cw <- zip_cw[!zip_cw$STATE %in% territories,]

# load all us cities
us_cities <- read_zips(
  file.path(resource_folder, "US_cities.csv"),
  "postal.code" 
)
# remove duplicates (US cities) and add rownames
us_cities <- us_cities[!duplicated(us_cities$postal.code),]
rownames(us_cities) <- us_cities$postal.code

# load additional US cities
addl_us_cities <- read_zips(
  file.path(resource_folder, "Additional_ZIP_US_Cities.csv"),
  "ZIP_CODE" 
)
rownames(addl_us_cities) <- addl_us_cities$ZIP_CODE

data_folder <- file.path(
  gsub("\\\\","/", gsub("OneDrive - ","", Sys.getenv("OneDrive"))), 
  "Health and Social Equity - SJP - BHN Score Creation",
  "Data", "Raw", "FDIC_NCUA")

# federal banking institutions
fed_banks <- read_zips(
  file.path(data_folder, "FDIC_locations_10_4_2021.csv"),
  "ZIP"
)

# credit unions
cu_banks <- read_zips(
  file.path(
    data_folder, 
    "NCUA_Credit_Union_Branch_Information_03_2021.csv"),
  "PhysicalAddressPostalCode"
)

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
alt_service <- data.frame(
  "ZIP_CODE" = zip_cw$ZIP_CODE, 
  "Num_Alt_Services" = 0
)
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
    in_df <- zbp_service$zip_code %in% alt_service
    
    # first add ZIP codes that already exist
    alt_service[as.character(zbp_service$zip_code[in_df]), "Num_Alt_Services"] <- 
      alt_service[as.character(zbp_service$zip_code[in_df]), "Num_Alt_Services"] +
      zbp_service$ESTAB[in_df]
    
    alt_service[as.character(zbp_service$zip_code[!in_df]), "Num_Alt_Services"] <- 
      zbp_service$ESTAB[!in_df]
    alt_service$ZIP_CODE <- as.character(rownames(alt_service))
  }
}

# aggregate financial accessibility metrics ----

# create the financial accessibility overall df
# already contains alternative services
fin_access <- alt_service

# preallocate mainstream services
fin_access$Num_Mainstream_Services <- 0
# count and add FDIC banks
fdic_count <- as.data.frame(table(as.character(fed_banks$ZIP)), 
                            stringsAsFactors = F)
in_df <- fdic_count$Var1 %in% fin_access
# first add the ones that do exist
fin_access[fdic_count$Var1[in_df], "Num_Mainstream_Services"] <-
  fin_access[fdic_count$Var1[in_df], "Num_Mainstream_Services"] +
  fdic_count$Freq[in_df]
# then add the ones that don't
fin_access[fdic_count$Var1[!in_df], "Num_Mainstream_Services"] <-
  fdic_count$Freq[!in_df]
fin_access$ZIP_CODE <- as.character(rownames(fin_access))

# count and add credit unions
cu_count <- as.data.frame(
  table(as.character(cu_banks$PhysicalAddressPostalCode)), 
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
fin_access$ZIP_CODE <- as.character(rownames(fin_access))

# do some clean-up
# remove zip 99999 -- doesn't exist
fin_access <- fin_access[fin_access$ZIP_CODE != "99999",]
# remove the ones that aren't in america (non numeric)
fin_access <- 
  fin_access[!is.na(suppressWarnings(as.numeric(fin_access$ZIP_CODE))),]
# make sure all na's are 0
fin_access[is.na(fin_access)] <- 0

# aggregate up to the city level ----

# add city and state names
fin_access$City <- us_cities[fin_access$ZIP_CODE, "place.name"]
fin_access$State <- us_cities[fin_access$ZIP_CODE, "state.code"]
# add additional US cities -- manually done from ZIP lookup
fin_access$City[fin_access$ZIP_CODE %in% addl_us_cities$ZIP_CODE] <- 
  addl_us_cities[
    fin_access$ZIP_CODE[fin_access$ZIP_CODE %in% addl_us_cities$ZIP_CODE], 
    "City"]
fin_access$State[fin_access$ZIP_CODE %in% addl_us_cities$ZIP_CODE] <- 
  addl_us_cities[
    fin_access$ZIP_CODE[fin_access$ZIP_CODE %in% addl_us_cities$ZIP_CODE], 
    "State"]

# get rid of everything else without a state -- not in US
fin_access <- fin_access[!is.na(fin_access$State),]

# aggregate by city and state
fin_access$City_State <- paste0(fin_access$City, "///", fin_access$State)
# count alt services and mainstream
agg_services <- aggregate(Num_Alt_Services ~ City_State, fin_access, sum)
agg_services <- cbind(
  agg_services,
  "Num_Mainstream_Services" = 
    aggregate(Num_Mainstream_Services ~ City_State, fin_access, sum)[,2]
)
# propegate out to the ZIP codes
rownames(agg_services) <- agg_services$City_State
fin_access$Num_Alt_Services_City <- 
  agg_services[fin_access$City_State, "Num_Alt_Services"]
fin_access$Num_Mainstream_Services_City <- 
  agg_services[fin_access$City_State, "Num_Mainstream_Services"]

# create raw financial accessibility score (subtract)
# negative == bad, positive == good
fin_access$Financial_Accessibility_City <-
  fin_access$Num_Mainstream_Services_City - fin_access$Num_Alt_Services_City

# for 0/0, they have the worst financial access -- make them lowest number - 1
fin_access$Financial_Accessibility_City[
  fin_access$Num_Alt_Services_City == 0 & 
    fin_access$Num_Mainstream_Services_City == 0
] <- min(fin_access$Financial_Accessibility_City) - 1

# fin_access$Financial_Accessibility_Ratio_City <-
#   fin_access$Unprocessed_Fin_Access <- 
#   fin_access$Num_Alt_Services_City/fin_access$Num_Mainstream_Services_City

# adjust financial accessibility ratio edges ----

# may delete depending on outcome

# # make some changes to edges to have the correct ranking
# eps <- 1e-5 # some epsilon
# # first, deal with 0 -- some mainstream services, no alternative
# # will be evenly distributed from:
# # [ min - .5 min, min - eps]
# min_val <- 
#   min(fin_access$Unprocessed_Fin_Access[fin_access$Unprocessed_Fin_Access > 0], 
#       na.rm = T)
# # divide range evenly by number of services
# only_main <- fin_access$Unprocessed_Fin_Access == 0 & 
#   !is.na(fin_access$Unprocessed_Fin_Access)
# max_serv <- 
#   max(fin_access$Num_Mainstream_Services_City[only_main])
# range_map <- data.frame(
#   "new" = seq(min_val - .5*min_val, min_val - eps, length.out = max_serv),
#   "old" = seq(max_serv, 1, by = -1)
# )
# # map the new values accordingly:
# fin_access$Financial_Accessibility_Ratio_City[only_main] <-
#   range_map$new[
#     match(
#       fin_access$Num_Mainstream_Services_City[only_main],
#       range_map$old
#     )]
# 
# # then, deal with Inf -- some alternative services, no mainstream
# # will be evenly distributed from:
# # [ max + eps, max + .5 max]
# max_val <- 
#   max(fin_access$Unprocessed_Fin_Access[fin_access$Unprocessed_Fin_Access < Inf], 
#       na.rm = T)
# # divide range evenly by number of services
# only_alt <- fin_access$Unprocessed_Fin_Access == Inf & 
#   !is.na(fin_access$Unprocessed_Fin_Access)
# max_serv <- 
#   max(fin_access$Num_Alt_Services_City[only_alt])
# range_map <- data.frame(
#   "new" = seq(max_val + eps, max_val + .5*max_val, length.out = max_serv),
#   "old" = seq(1, max_serv, by = 1)
# )
# # map the new values accordingly:
# fin_access$Financial_Accessibility_Ratio_City[only_alt] <-
#   range_map$new[
#     match(
#       fin_access$Num_Alt_Services_City[only_alt],
#       range_map$old
#     )]
# 
# # lastly, deal with NaN -- the worst, no access at all
# # -- put that one "unit" above highest Inf
# fin_access$Financial_Accessibility_Ratio_City[
#   is.nan(fin_access$Unprocessed_Fin_Access)]  <-
#   max(fin_access$Financial_Accessibility_Ratio_City[
#     !is.nan(fin_access$Unprocessed_Fin_Access)]) + max(diff(range_map$new))
# 
# # now, log the score
# fin_access$financialaccessibility_logratio_pop  <-
#   log(fin_access$Financial_Accessibility_Ratio_City)

# write out score ----

# clean up -- remove city/state
fin_access <- fin_access[, !colnames(fin_access) %in% "City_State"]

data_folder <- file.path(
  gsub("\\\\","/", gsub("OneDrive - ","", Sys.getenv("OneDrive"))), 
  "Health and Social Equity - SJP - BHN Score Creation",
  "Data", "Preprocessed")

write.csv(fin_access, 
          file = file.path(data_folder,
                           "New_America_Financial_Accessibility_City.csv"), 
          row.names = F, 
          na = "")
