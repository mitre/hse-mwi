# Download packages: dplyr for Cleaning and jsonlite for importing via API.
library(dplyr)
library(jsonlite)

# Import the Data 
# The URL needs to be this long lengthwise, or else the data will not import
raw_data <- 
  fromJSON("https://ephtracking.cdc.gov:443/apigateway/api/v1/getCoreHolder/428/137/1/1,2,4,5,6,8,9,10,12,13,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,44,45,46,47,48,49,50,51,53,54,55,56/2015/0/0")$tableResult

# Clean the Data: delete unnecessary variables and rename included variables
data <- raw_data %>%
  select(c(displayValue, parentGeoAbbreviation, geoId)) %>%
  rename("environment_withinhalfmilepark_pop" = displayValue,
         Census_Tract = geoId, State = parentGeoAbbreviation) %>%
  mutate(environment_withinhalfmilepark_pop = 
           as.numeric(gsub("%","",environment_withinhalfmilepark_pop)))

# Export CSV to our shared OneDrive
data_folder <- file.path(
  gsub("\\\\","/", gsub("OneDrive - ","", Sys.getenv("OneDrive"))), 
  "Health and Social Equity - SJP - BHN Score Creation",
  "Data", "Preprocessed")

write.csv(data, file = file.path(data_folder,
                                      "half_mile_park_pop.csv"), row.names = F, na = "")

