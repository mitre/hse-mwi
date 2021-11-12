# Life Expectancy Race Stratification
# By Hannah De los Santos
# Originated on: 10/13/21

# supporting functions ----

# function to map county names to their fips codes
# cty_df: dataframe with county and state columns
# st_code: column name of state codes
# cty_name: column name of county names
# returns vector of mapped numbers
cty_name_map <- function(cty_df, st_code_col, cty_name_col){
  geoid_res <-
    sapply(1:nrow(cty_df), function(x){
      st_code <- tolower(cty_df[x, st_code_col])
      cty_name <- tolower(gsub(
        "Saint ", "St. ", 
        gsub("St ", "St. ", 
             cty_df[x, cty_name_col])
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
  
  return(geoid_res)
}

# load data and libraries ----

library(usmap)

# supporting data -- resource folder
resource_folder <-file.path(
  gsub("\\\\","/", gsub("OneDrive - ","", Sys.getenv("OneDrive"))), 
  "Health and Social Equity - SJP - BHN Score Creation",
  "Data", "Resources")

# supporting data -- data folder
data_folder <- file.path(
  gsub("\\\\","/", gsub("OneDrive - ","", Sys.getenv("OneDrive"))), 
  "Health and Social Equity - SJP - BHN Score Creation",
  "Data", "Raw", "Life_Expectancy")

# load county names to fips codes -- taken from usmap library
county_fips <- read.csv(file.path(resource_folder, "county_name_to_fips.csv"),
                        colClasses = c("fips" = "character"))


# load life expectancy at county levels
cty_le <- read.csv(file.path(data_folder, 
                             "CHR_Life_Expectancy_2017-2019.csv"))
# make all the 100+ to 100, convert all necessary columns to numeric
cty_le[cty_le == "100+"] <- 100
cty_le[, c(3,5:ncol(cty_le))] <- 
  sapply(cty_le[, c(3,5:ncol(cty_le))],as.numeric)
# map all the names to geoid numbers
cty_le$ST_ABBR <- setNames(state.abb, state.name)[cty_le$State]
cty_le$ST_ABBR[cty_le$State == "District of Columbia"] <- "DC"
cty_le$CNTY_NUM <- cty_name_map(cty_le, "ST_ABBR", "County")

# read in life expectancy at census tract level
ct_le <- read.csv(file.path(data_folder, 
                            "USALEEP_Life_Expectancy.CSV"),
                  colClasses = c("Tract.ID" = "character",
                                 "STATE2KX" = "character",
                                 "CNTY2KX" = "character",
                                 "TRACT2KX" = "character"
                  ))
ct_le$CNTY_NUM <- paste0(ct_le$STATE2KX, ct_le$CNTY2KX)

# extrapolate life expectancy ----

# make it easy to map county expectancy
rownames(cty_le) <- cty_le$CNTY_NUM

# extrapolate life expectancy using county as baseline
ct_le$baseline <- cty_le[ct_le$CNTY_NUM, "County.Value"]
ct_le$black_le <- cty_le[ct_le$CNTY_NUM, "Black"]
ct_le$modifier <- ct_le$black_le/ct_le$baseline

# create modified life expectancy
ct_le$Life.Expectancy_black <- ct_le$estimate*ct_le$modifier
ct_le$Life.Expectancy_pop <- ct_le$estimate

# fill in where the county data is missing with baseline
ct_le$Life.Expectancy_black[is.na(ct_le$Life.Expectancy_black)] <-
  ct_le$estimate[is.na(ct_le$Life.Expectancy_black)]

# write out ----

data_folder <- file.path(
  gsub("\\\\","/", gsub("OneDrive - ","", Sys.getenv("OneDrive"))), 
  "Health and Social Equity - SJP - BHN Score Creation",
  "Data", "Preprocessed")

write.csv(ct_le, 
          file = file.path(data_folder,
                           "CHR_USALEEP_Life_Expectancy.csv"), 
          row.names = F, na = "")