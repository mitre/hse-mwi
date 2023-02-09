# Convert Between 2010 to 2020 census values
# By Hannah De los Santos
# Originated on: 1/25/23

# load libraries and data ----

rel_folder <- file.path("Data", "Resources")

# load relationship files
ct_rel <- read.csv(file.path(rel_folder, "tab20_tract20_tract10_natl.txt"),
                   colClasses = c(
                     "GEOID_TRACT_20" = "character",
                     "GEOID_TRACT_10" = "character"
                   ),
                   sep = "|")

zcta_rel <- read.csv(file.path(rel_folder, "tab20_zcta510_zcta520_natl.txt"),
                     colClasses = c(
                       "GEOID_ZCTA5_20" = "character",
                       "GEOID_ZCTA5_10" = "character"
                     ),
                     sep = "|")
# get rid of zctas with no relationship between each other
zcta_rel <- zcta_rel[zcta_rel$GEOID_ZCTA5_10 != "" & 
                       zcta_rel$GEOID_ZCTA5_20 != "",]

# filter out territories
territories <- as.character(c(60, 64, 66, 68, 69, 70, 72, 74, 78))
ct_rel <- ct_rel[!substr(ct_rel$GEOID_TRACT_20, 1, 2) %in% territories,]

# census tracts: 2010 to 2020 ----

ct_10_to_20 <- function(df, geoid_col, meas_col){
  cat(paste0("[", Sys.time(), "]: Converting census tracts 2010 to census tracts 2020\n"))
  
  # first preallocate the result
  res_df <- data.frame("GEOID" = unique(ct_rel$GEOID_TRACT_20))
  res_df[, meas_col] <- NA
  rownames(res_df) <- res_df$GEOID
  
  # first, assign the ones that are fully contained
  # either ones that map one to one, or 2020 that are in 2010 fully
  contain_2010 <- !ct_rel$GEOID_TRACT_20 %in% 
    ct_rel$GEOID_TRACT_20[duplicated(ct_rel$GEOID_TRACT_20)]
  ct_rel_sub <- ct_rel[contain_2010,]
  # get the non duplicated rows
  df_sub <- df[df[,geoid_col] %in% ct_rel_sub$GEOID_TRACT_10,]
  rownames(df_sub) <- df_sub[, geoid_col]
  # assign the 2010 tracts to the 2020 tracts
  res_df[ct_rel_sub[, "GEOID_TRACT_20"], meas_col] <-
    df_sub[ct_rel$GEOID_TRACT_10[contain_2010], meas_col]
  
  # index results by the GEOID
  rownames(df) <- df[,geoid_col]
  
  ct_rel_sub <- ct_rel[ct_rel$GEOID_TRACT_20 %in% 
                         ct_rel$GEOID_TRACT_20[duplicated(ct_rel$GEOID_TRACT_20)],]
  ct_rel_sub$perc_land <- ct_rel_sub$AREALAND_PART/ct_rel_sub$AREALAND_TRACT_20
  
  # now deal with partials using weighted values
  for (v in  meas_col){
    
    # subset to only the ones that appear in the target and are not NA
    ct_rel_sub <- ct_rel_sub[ct_rel_sub$GEOID_TRACT_10 %in% df[!is.na(df[,v]),
                                                               geoid_col],]
    
    # find the denominator of weighted sums
    tmp <- aggregate(perc_land ~ GEOID_TRACT_20, ct_rel_sub, FUN = sum)
    rownames(tmp) <- tmp$GEOID_TRACT_20
    ct_rel_sub$weight_sum <- tmp[ct_rel_sub$GEOID_TRACT_20, "perc_land"]
    
    # find the multiplier for weighted sums
    ct_rel_sub$weight_perc <- ct_rel_sub$perc_land/ct_rel_sub$weight_sum
    
    ct_rel_sub[, v] <- df[ct_rel_sub$GEOID_TRACT_10, v]*ct_rel_sub$weight_perc
    
    # now sum the weighted values
    tmp <- aggregate(ct_rel_sub[, v], 
                     by = list(GEOID_TRACT_20 = ct_rel_sub$GEOID_TRACT_20),
                     FUN = sum, na.rm = T)
    
    res_df[tmp$GEOID_TRACT_20, v] <- tmp$x
  }
  
  # convert column name back
  colnames(res_df)[1] <- geoid_col
  
  for (mc in meas_col){
    cat(paste0(
      "[", Sys.time(), "]: Mapped ", mc, " from 2010 to 2020, ", 
      signif(sum(is.na(res_df[, mc]))/nrow(res_df)*100, 4), "% (",
      sum(is.na(res_df[, mc])), ") missing/not mapped\n"
    ))
  }
  
  return(res_df)
}

# ZCTA: 2010 to 2020 ----

zcta_10_to_20 <- function(df, geoid_col, meas_col){
  cat(paste0("[", Sys.time(), "]: Converting ZCTA 2010 to ZCTA 2020\n"))
  
  # first preallocate the result
  res_df <- data.frame("GEOID" = unique(zcta_rel$GEOID_ZCTA5_20))
  res_df[, meas_col] <- NA
  rownames(res_df) <- res_df$GEOID
  
  # first, assign the ones that are fully contained
  # either ones that map one to one, or 2020 that are in 2010 fully
  contain_2010 <- !zcta_rel$GEOID_ZCTA5_20 %in% 
    zcta_rel$GEOID_ZCTA5_20[duplicated(zcta_rel$GEOID_ZCTA5_20)]
  zcta_rel_sub <- zcta_rel[contain_2010,]
  # get the non duplicated rows
  df_sub <- df[df[,geoid_col] %in% zcta_rel_sub$GEOID_ZCTA5_10,]
  rownames(df_sub) <- df_sub[, geoid_col]
  # assign the 2010 tracts to the 2020 tracts
  res_df[zcta_rel_sub[, "GEOID_ZCTA5_20"], meas_col] <-
    df_sub[zcta_rel$GEOID_ZCTA5_10[contain_2010], meas_col]
  
  # index results by the GEOID
  rownames(df) <- df[,geoid_col]
  
  zcta_rel_sub <- zcta_rel[zcta_rel$GEOID_ZCTA5_20 %in% 
                         zcta_rel$GEOID_ZCTA5_20[duplicated(zcta_rel$GEOID_ZCTA5_20)],]
  zcta_rel_sub$perc_land <- zcta_rel_sub$AREALAND_PART/zcta_rel_sub$AREALAND_ZCTA5_20
  
  # now deal with partials using weighted values
  for (v in meas_col){
    
    # subset to only the ones that appear in the target and are not NA
    zcta_rel_sub <- zcta_rel_sub[zcta_rel_sub$GEOID_ZCTA5_10 %in% df[!is.na(df[,v]),
                                                               geoid_col],]
    
    # find the denominator of weighted sums
    tmp <- aggregate(perc_land ~ GEOID_ZCTA5_20, zcta_rel_sub, FUN = sum)
    rownames(tmp) <- tmp$GEOID_ZCTA5_20
    zcta_rel_sub$weight_sum <- tmp[zcta_rel_sub$GEOID_ZCTA5_20, "perc_land"]
    
    # find the multiplier for weighted sums
    zcta_rel_sub$weight_perc <- zcta_rel_sub$perc_land/zcta_rel_sub$weight_sum
    
    zcta_rel_sub[, v] <- df[zcta_rel_sub$GEOID_ZCTA5_10, v]*zcta_rel_sub$weight_perc
    
    # now sum the weighted values
    tmp <- aggregate(zcta_rel_sub[, v], 
                     by = list(GEOID_ZCTA5_20 = zcta_rel_sub$GEOID_ZCTA5_20),
                     FUN = sum, na.rm = T)
    
    res_df[tmp$GEOID_ZCTA5_20, v] <- tmp$x
  }
  
  # convert column name back
  colnames(res_df)[1] <- geoid_col
  
  for (mc in meas_col){
    cat(paste0(
      "[", Sys.time(), "]: Mapped ", mc, " from 2010 to 2020, ", 
      signif(sum(is.na(res_df[, mc]))/nrow(res_df)*100, 4), "% (",
      sum(is.na(res_df[, mc])), ") missing/not mapped/not available in 2020\n"
    ))
  }
  
  return(res_df)
}

