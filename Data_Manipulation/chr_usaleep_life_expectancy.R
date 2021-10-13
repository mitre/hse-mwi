# Life Expectancy Race Stratification
# By Hannah De los Santos
# Originated on: 10/13/21

# load data and libraries ----

# supporting data -- data folder
data_folder <- file.path(
  gsub("\\\\","/", gsub("OneDrive - ","", Sys.getenv("OneDrive"))), 
  "Health and Social Equity - SJP - BHN Score Creation",
  "Data", "Raw", "Life_Expectancy")

# load life expectancy at county and census tract levels
cty_le <- read.csv(file.path(data_folder, 
                             "CHR_Alabama_Life_Expectancy_2017-2019.csv"))

ct_le <- read.csv(file.path(data_folder, 
                            "USALEEP_Life_Expectancy.CSV"),
                  colClasses = c("Tract.ID" = "character",
                                 "STATE2KX" = "character",
                                 "CNTY2KX" = "character",
                                 "TRACT2KX" = "character"
                  ))

# get the census shapefiles and mapping
cty_shp <- as.data.frame(
  get_acs("county",
          variables = "B01001_001E",
          year = 2019,
          state = "AL")
)
cty_shp$CNTY_NUM <- substring(cty_shp$GEOID,3,5)
# the below will not work outside this example
cty_shp$CNTY_NAME <- gsub(" County, Alabama", "", cty_shp$NAME)
rownames(cty_shp) <- cty_shp$NAME
# add number to counties
cty_le$CNTY_NUM <- cty_shp[cty_le$County, "CNTY_NUM"]

# extrapolate life expectancy ----

# make it easy to map county expectancy
rownames(cty_le) <- cty_le$CNTY_NUM

# extrapolate life expectancy using county as baseline
ct_shp$baseline <- cty_le[ct_shp$CNTY_NUM, "County.Value"]
ct_shp$black_le <- cty_le[ct_shp$CNTY_NUM, "Black"]
ct_shp$modifier <- ct_shp$black_le/ct_shp$baseline

ct_shp$Life.Expectancy_black <- ct_shp$Life.Expectancy*ct_shp$modifier
