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
# filter out territories
territories <- c(60, 66, 72, 78)
county_cw <- county_cw[!county_cw$STATE %in% territories,]
ct_cw <- ct_cw[!ct_cw$STATE %in% territories,]

# also keep all unique zctas to generate a file
all_zctas <- unique(county_cw$ZCTA5)

# county to zcta function ----

# function that converts county data to zcta
# input:
# df: dataframe with a geoid column for counties and a measure column
# geoid_col: column name corresponding to geoid
# meas_col: a vector of measure column names
# output:
# zcta_df: measures converted for each zcta
county_to_zcta <- function(df, geoid_col, meas_col){
  # first, preallocate the result
  zcta_df <- data.frame("ZCTA" = all_zctas)
  zcta_df[, meas_col] <- NA
  rownames(zcta_df) <- zcta_df$ZCTA
  
  # first, exactly assign the ones that are fully contained
  cty_sub <- county_cw[!county_cw$ZCTA5 %in% 
                         county_cw$ZCTA5[duplicated(county_cw$ZCTA5)],]
  # get the corresponding counties
  df_sub <- df[df[,geoid_col] %in% cty_sub$GEOID,]
  rownames(df_sub) <- df_sub$County.Code
  # add the var in the df to the county sub
  cty_sub[, meas_col] <- df_sub[cty_sub$GEOID, meas_col]
  # add the results to the main dataframe
  zcta_df[cty_sub$ZCTA5, meas_col] <- cty_sub[, meas_col]
  
  # now get a value for zctas with multiple mappings
  for (z in zcta_df$ZCTA[is.na(zcta_df[,meas_col])]){
    # get the sub crosswalk value
    cty_sub <- county_cw[county_cw$ZCTA5 == z,]
    # get the corresponding counties
    df_sub <- df[df[,geoid_col] %in% cty_sub$GEOID,]
    
    # if there are none in the data, leave as missing
    if (nrow(df_sub) > 0){
      # make sure that both are ordered in the same way
      cty_sub <- cty_sub[order(cty_sub$GEOID),]
      df_sub <- df_sub[order(df_sub[,geoid_col]),]
      
      # we need to do a population weighted average -- weights are based on
      # percentages in each zcta
      # remove NAs
      zcta_df[z, meas_col] <- 
        sapply(meas_col, function(x){
          weighted.mean(df_sub[,x], cty_sub$ZPOPPCT, na.rm = T)
        })
        
    }
  }
  
  return(zcta_df)
}
