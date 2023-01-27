# Convert Geographies to ZCTAs 
# By Hannah De los Santos
# Originated on: 6/21/21

# Note: using household based weighted averages

# load libraries and data ----

library(stringr)

cw_folder <- file.path("Data", "Resources")

# load crosswalk files
county_cw <- read.csv(file.path(cw_folder, "zcta_county_rel_20.csv"),
                      colClasses = c(
                        "ZCTA5" = "character",
                        "GEOID" = "character"
                      ))
county_cw$STATE <- substr(county_cw$GEOID, 1, 2)
county_cw$COUNTY <- substr(county_cw$GEOID, 3, 5)
ct_cw <- read.csv(file.path(cw_folder, "zcta_tract_rel_20.csv"),
                  colClasses = c(
                    "ZCTA5" = "character",
                    "GEOID" = "character"
                  ))
ct_cw$STATE <- substr(ct_cw$GEOID, 1, 2)
zip_cw <- read.csv(file.path(cw_folder, "Zip_to_zcta_crosswalk_2021.csv"),
                   colClasses = c(
                     "ZIP_CODE" = "character",
                     "ZCTA" = "character"
                   ))
# filter out territories
territories <- as.character(c(60, 64, 66, 68, 69, 70, 72, 74, 78))
county_cw <- county_cw[!substr(county_cw$GEOID, 1, 2) %in% territories,]
ct_cw <- ct_cw[!substr(ct_cw$GEOID, 1, 2) %in% territories,]
territories <- c("AS", "FM", "GU", "MH", "MP", "PW", "PR", "VI")
zip_cw <- zip_cw[!zip_cw$STATE %in% territories,]

# also keep all unique zctas to generate a file
all_zctas <- unique(county_cw$ZCTA5)
all_zctas <- all_zctas[all_zctas != ""]

# data quality function ----

# function to check whether or not geoids are the correct character length 
# (have dropped leading zeros)
# input:
# df: dataframe with a geoid column for counties and a measure column
# geoid_col: column name corresponding to geoid
# type: ZCTA, County, ZIP Code, or Census Tract
# output:
# df, with geoid_col as a character with padded leading 0s
check_geoid <- function(df, geoid_col, type){
  # first, convert to character
  df[,geoid_col] <- as.character(df[,geoid_col])
  
  # map for each type
  len_map <- c(
    "ZCTA" = 5,
    "County" = 5,
    "ZIP Code" = 5,
    "Census Tract" = 11
  )
  
  # pad with leading 0s if needed
  df[, geoid_col] <- str_pad(df[, geoid_col], len_map[type], pad = "0")
  
  return(df)
}

# county to zcta function ----

# function that converts county data to zcta
# input:
# df: dataframe with a geoid column for counties and a measure column
# geoid_col: column name corresponding to geoid
# meas_col: a vector of measure column names
# output:
# zcta_df: measures converted for each zcta
county_to_zcta <- function(df, geoid_col, meas_col){
  cat(paste0("[", Sys.time(), "]: Converting county to ZCTA\n"))
  
  cat(paste0(
    "[", Sys.time(), "]: ",
    signif(sum(!df[,geoid_col] %in% county_cw$GEOID)/nrow(df)*100, 4),
    "% (",
    sum(!df[,geoid_col] %in% county_cw$GEOID),
    ") of counties not found in master crosswalk\n"
  ))
  
  # first, preallocate the result
  zcta_df <- data.frame("ZCTA" = all_zctas)
  zcta_df[, meas_col] <- NA
  rownames(zcta_df) <- zcta_df$ZCTA
  
  # first, exactly assign the ones that are fully contained
  cty_sub <- county_cw[!county_cw$ZCTA5 %in% 
                         county_cw$ZCTA5[duplicated(county_cw$ZCTA5)],]
  # get the corresponding counties
  df_sub <- df[df[,geoid_col] %in% cty_sub$GEOID,]
  rownames(df_sub) <- df_sub[, geoid_col]
  # add the var in the df to the county sub
  cty_sub[, meas_col] <- df_sub[cty_sub$GEOID, meas_col]
  # add the results to the main dataframe
  zcta_df[cty_sub$ZCTA5, meas_col] <- cty_sub[, meas_col]
  
  # now subset to census tracts that are in the dataset at all
  cty_sub <- county_cw[county_cw$GEOID %in% df[, geoid_col],]
  zcta_iter <- zcta_df$ZCTA[
    rowSums(is.na(zcta_df[,meas_col,drop = F])) == length(meas_col) & 
      zcta_df$ZCTA %in% cty_sub$ZCTA5]
  
  # now get a value for zctas with multiple mappings
  for (z in zcta_iter){
    # get the sub crosswalk value, must be in both
    cty_sub <- county_cw[county_cw$ZCTA5 == z & 
                           county_cw$GEOID %in% df[,geoid_col],]
    # get the corresponding counties
    df_sub <- df[df[,geoid_col] %in% cty_sub$GEOID,]
    
    # if there are none in the data, leave as missing
    if (nrow(df_sub) > 0){
      # make sure that both are ordered in the same way
      cty_sub <- cty_sub[order(cty_sub$GEOID),]
      df_sub <- df_sub[order(df_sub[,geoid_col]),]
      
      # we need to do a population weighted average -- weights are based on
      # area percentages in each zcta
      # remove NAs
      zcta_df[z, meas_col] <- 
        sapply(meas_col, function(x){
          weighted.mean(df_sub[,x], 
                        cty_sub$AREALAND_PART/cty_sub$AREALAND_ZCTA5_20*100, 
                        na.rm = T)
        })
        
    }
  }
  
  for (mc in meas_col){
    cat(paste0(
      "[", Sys.time(), "]: Mapped ", mc, " to ZCTA, ", 
      signif(sum(is.na(zcta_df[, mc]))/nrow(zcta_df)*100,4), "% (",
      sum(is.na(zcta_df[, mc])), ") missing/not mapped\n"
    ))
  }
  
  return(zcta_df)
}

# census tract to zcta function ----

# function that converts census tract data to zcta
# input:
# df: dataframe with a geoid column for census tracts and a measure column
# geoid_col: column name corresponding to geoid
# meas_col: a vector of measure column names
# use_mean: TRUE by default, use a mean for combining ZIP codes; otherwise, use 
#   a sum for combination 
# output:
# zcta_df: measures converted for each zcta
ct_to_zcta <- function(df, geoid_col, meas_col, use_mean = TRUE){
  cat(paste0("[", Sys.time(), "]: Converting census tract to ZCTA\n"))
  
  cat(paste0(
    "[", Sys.time(), "]: ",
    signif(sum(!df[,geoid_col] %in% ct_cw$GEOID)/nrow(df)*100, 4),
    "% (",
    sum(!df[,geoid_col] %in% ct_cw$GEOID),
    ") of census tracts not found in master crosswalk\n"
  ))
  
  # first, preallocate the result
  zcta_df <- data.frame("ZCTA" = all_zctas)
  zcta_df[, meas_col] <- NA
  rownames(zcta_df) <- zcta_df$ZCTA
  
  # first, exactly assign the ones that are fully contained
  ct_sub <- ct_cw[!ct_cw$ZCTA5 %in%
                    ct_cw$ZCTA5[duplicated(ct_cw$ZCTA5)],]
  # get the corresponding census tracts
  df_sub <- df[df[,geoid_col] %in% ct_sub$GEOID,]
  rownames(df_sub) <- df_sub[, geoid_col]
  # add the var in the df to the county sub
  ct_sub[, meas_col] <- df_sub[ct_sub$GEOID, meas_col]
  # add the results to the main dataframe
  zcta_df[ct_sub$ZCTA5, meas_col] <- ct_sub[, meas_col]
  
  # now subset to census tracts that are in the dataset at all
  ct_sub <- ct_cw[ct_cw$GEOID %in% df[, geoid_col],]
  zcta_iter <- zcta_df$ZCTA[
    rowSums(is.na(zcta_df[,meas_col,drop = F])) == length(meas_col) & 
      zcta_df$ZCTA %in% ct_sub$ZCTA5]
  
  # now get a value for zctas with multiple mappings
  for (z in zcta_iter){
    # get the sub crosswalk value, make sure exists in both
    ct_sub <- ct_cw[ct_cw$ZCTA5 == z & ct_cw$GEOID %in% df[,geoid_col],]
    # get the corresponding census tracts
    df_sub <- df[df[,geoid_col] %in% ct_sub$GEOID,]
    
    # if there are none in the data, leave as missing
    if (nrow(df_sub) > 0){
      # make sure that both are ordered in the same way
      ct_sub <- ct_sub[order(ct_sub$GEOID),]
      df_sub <- df_sub[order(df_sub[,geoid_col]),]
      
      # we need to do a housing unit weighted average -- weights are based on
      # percentages in each zcta
      # remove NAs and missing tracts
      zcta_df[z, meas_col] <- 
        sapply(meas_col, function(x){
          if (use_mean){
            weighted.mean(
              df_sub[,x],
              ct_sub$AREALAND_PART/ct_sub$AREALAND_ZCTA5_20*100, 
              na.rm = T)
          } else {
            sum(df_sub[,x], na.rm = T)
          }
        })
    }
  }
  
  for (mc in meas_col){
    cat(paste0(
      "[", Sys.time(), "]: Mapped ", mc, " to ZCTA, ", 
      signif(sum(is.na(zcta_df[, mc]))/nrow(zcta_df)*100, 4), "% (",
      sum(is.na(zcta_df[, mc])), ") missing/not mapped\n"
    ))
  }
  
  return(zcta_df)
}

# zip code to zcta function ----

# function that converts zip to zcta
# input:
# df: dataframe with a geoid column for census tracts and a measure column
# geoid_col: column name corresponding to geoid
# meas_col: a vector of measure column names
# use_mean: TRUE by default, use a mean for combining ZIP codes; otherwise, use 
#   a sum for combination 
# output:
# zcta_df: measures converted for each zcta
zip_to_zcta <- function(df, geoid_col, meas_col, use_mean = TRUE){
  cat(paste0("[", Sys.time(), "]: Converting ZIP code to ZCTA\n"))
  
  cat(paste0(
    "[", Sys.time(), "]: ",
    signif(sum(!df[,geoid_col] %in% zip_cw$ZIP_CODE)/nrow(df)*100, 4),
    "% (",
    sum(!df[,geoid_col] %in% zip_cw$ZIP_CODE),
    ") of ZIP codes not found in master crosswalk\n"
  ))
  
  # first, preallocate the result
  zcta_df <- data.frame("ZCTA" = all_zctas)
  zcta_df[, meas_col] <- NA
  rownames(zcta_df) <- zcta_df$ZCTA
  
  # first, exactly assign the ones that are fully contained
  zip_sub <- zip_cw[!zip_cw$ZCTA %in%
                    zip_cw$ZCTA[duplicated(zip_cw$ZCTA)],]
  # get the corresponding zip codes
  df_sub <- df[df[,geoid_col] %in% zip_sub$ZIP_CODE,]
  rownames(df_sub) <- df_sub[, geoid_col]
  # add the var in the df to the county sub
  zip_sub[, meas_col] <- df_sub[zip_sub$ZIP_CODE, meas_col]
  # add the results to the main dataframe
  zcta_df[zip_sub$ZCTA, meas_col] <- zip_sub[, meas_col]
  
  # now subset to census tracts that are in the dataset at all
  zip_sub <- zip_cw[zip_cw$ZIP_CODE %in% df[, geoid_col],]
  zcta_iter <- zcta_df$ZCTA[
    rowSums(is.na(zcta_df[,meas_col,drop = F])) == length(meas_col) & 
      zcta_df$ZCTA %in% zip_sub$ZIP_CODE]
  
  # now get a value for zctas with multiple mappings
  for (z in zcta_iter){
    # get the sub crosswalk value, make sure exists in both
    zip_sub <- zip_cw[zip_cw$ZCTA == z & zip_cw$ZIP_CODE %in% df[,geoid_col],]
    # get the corresponding zip codes
    df_sub <- df[df[,geoid_col] %in% zip_sub$ZIP_CODE,]
    
    # if there are none in the data, leave as missing
    if (nrow(df_sub) > 0){
      # make sure that both are ordered in the same way
      df_sub <- df_sub[order(df_sub[,geoid_col]),]
      
      # we need to do an average 
      # remove NAs and missing tracts
      zcta_df[z, meas_col] <- 
        sapply(meas_col, function(x){
          if (use_mean){
            mean(df_sub[,x], na.rm = T)
          } else {
            sum(df_sub[,x], na.rm = T)
          }
        })
    }
  }
  
  for (mc in meas_col){
    cat(paste0(
      "[", Sys.time(), "]: Mapped ", mc, " to ZCTA, ", 
      signif(sum(is.na(zcta_df[, mc]))/nrow(zcta_df)*100, 4), "% (",
      sum(is.na(zcta_df[, mc])), ") missing/not mapped\n"
    ))
  }
  
  return(zcta_df)
}
