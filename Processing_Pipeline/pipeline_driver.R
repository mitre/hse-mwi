# Main Data Processing Pipeline File
# By HSE Team
# Originated on: 6/28/21

# load libraries and overarching data ----

library(readxl)

# folder where all the data and information for the pipeline is
data_folder <- file.path(
  gsub("\\\\","/", gsub("OneDrive - ","", Sys.getenv("OneDrive"))), 
  "Health and Social Equity - SJP - BHN Score Creation",
  "Data")

# folder where all the preprocessed data is
preprocessed_folder <- file.path(data_folder, "Preprocessed")

# folder where all the cleaned/output data goes
cleaned_folder <- file.path(data_folder, "Cleaned")

# load measure registry -- first sheet
m_reg <- read_excel(file.path(data_folder, "Metadata.xlsx"), sheet = 1)
# remove everything that doesn't have a numerator
m_reg <- m_reg[!is.na(m_reg$Numerator),]

# load supplemental data/functions ----

# load crosswalk functions and information
source(file.path("Processing_Pipeline", "crosswalk_func.R"))

# note -- we ignore missing data when doing ranking
# function to compute percentile ranking
perc.rank <- function(x) {
  return(rank(x, na.last = "keep")/length(x[!is.na(x)])*100)
}

# allocate overarching variables ----

geo_levels <- c(
  "ZCTA",
  "County",
  "Census Tract",
  "ZIP Code"
)

# first, go through and load/collate data ----

# TODO: use data tables?

cat(paste0("[", Sys.time(), "]: Loading and collating data\n"))

# where we're going to put all the data
level_data <- list()
for (gl in geo_levels){
  # subset to the data at the specific geographic level
  m_reg_sub <- m_reg[m_reg$`Geographic Level` == gl,]
  
  comb_df <- data.frame()
  for (fn in unique(m_reg_sub$Filename)){
    # correct geoid column 
    geoid_colname <- m_reg_sub$`GEOID Column`[m_reg_sub$Filename == fn][1]
    
    # read in data 
    curr_df <- read.csv(file.path(preprocessed_folder, fn),
                        colClasses = setNames(
                          "character",
                          geoid_colname
                        ))
    
    curr_df <- check_geoid(curr_df, geoid_colname, type = gl)
    
    # collate data
    if (nrow(comb_df) == 0){
      # add our data
      comb_df <- rbind(comb_df, curr_df)
      # rename geoid column
      colnames(comb_df)[colnames(comb_df) == geoid_colname] <- "GEOID"
      # add as rownames
      rownames(comb_df) <- as.character(comb_df$GEOID)
    } else { # add data to existing
      comb_df[as.character(curr_df[,geoid_colname]), 
              colnames(curr_df)[colnames(curr_df) != geoid_colname]] <-
        curr_df[,colnames(curr_df)[colnames(curr_df) != geoid_colname]]
      
      # update geoid and rownames
      comb_df$GEOID <- as.character(rownames(comb_df))
    }
  }
  
  # add to full list
  level_data[[gl]] <- comb_df
}

# get/output variable information ----

# TODO: ADD TRYCATCH/DATA QUALITY FILTERING

cat(paste0("[", Sys.time(), "]: Calculating data information\n"))

# preallocate output data -- this will also be useful later
info_dat <- as.data.frame(m_reg)
# add additional information
info_dat[, c("Is_Numeric", "Minimum", "Maximum", "Missing", "Number_Rows")] <-
  NA
# duplicate rows that are race stratified
info_dat <- rbind(info_dat, info_dat[m_reg$`Race Stratification`,])
# add pop for ones that are preprocessed but not duplicated
is_preprocessed <- info_dat$`Is Preprocessed`
not_dup_num <- !duplicated(info_dat$Numerator)
not_dup_den <- !duplicated(info_dat$Denominator) & !is.na(info_dat$Denominator)
info_dat$Numerator[is_preprocessed & not_dup_num] <-
  paste0(info_dat$Numerator[is_preprocessed & not_dup_num], "_pop")
info_dat$Denominator[is_preprocessed & not_dup_den] <-
  paste0(info_dat$Denominator[is_preprocessed & not_dup_den], "_pop")

# add black for ones that are preprocessed and are not pop
not_pop_num <- !grepl("_pop", info_dat$Numerator)
not_pop_den <- !grepl("_pop", info_dat$Denominator) & 
  !is.na(info_dat$Denominator)
info_dat$Numerator[is_preprocessed & not_pop_num] <-
  paste0(info_dat$Numerator[is_preprocessed & not_pop_num], "_black")
info_dat$Denominator[is_preprocessed & not_pop_den] <-
  paste0(info_dat$Denominator[is_preprocessed & not_pop_den], "_black")

rownames(info_dat) <- info_dat$Numerator

# where we're going to put all the data
for (gl in geo_levels){
  # subset to the data at the specific geographic level
  info_dat_sub <- info_dat[info_dat$`Geographic Level` == gl,]
  
  if (nrow(info_dat_sub) > 0){
    # we'll go through each column and compute information for each
    info_dat[info_dat_sub$Numerator, "Is_Numeric"] <- 
      sapply(level_data[[gl]][, info_dat_sub$Numerator],
             is.numeric)
    info_dat[info_dat_sub$Numerator, "Minimum"] <- 
      sapply(level_data[[gl]][, info_dat_sub$Numerator], 
             function(x){min(x, na.rm = T)})
    info_dat[info_dat_sub$Numerator, "Maximum"] <- 
      sapply(level_data[[gl]][, info_dat_sub$Numerator], 
             function(x){max(x, na.rm = T)})
    info_dat[info_dat_sub$Numerator, "Missing"] <- 
      sapply(level_data[[gl]][, info_dat_sub$Numerator], 
             function(x){sum(is.na(x))})
    info_dat[info_dat_sub$Numerator, "Number_Rows"] <- 
      sapply(level_data[[gl]][, info_dat_sub$Numerator], 
             function(x){sum(!is.na(x))})
    
    # output statistics about data
    for (num in info_dat_sub$Numerator){
      cat(paste0("Statistics for ", num, ":\n"))
      for (cn in c("Is_Numeric", "Minimum", "Maximum", "Missing" , 
                   "Number_Rows")){
        cat(paste0("\t", gsub("_", " ", cn),":", 
                   info_dat[num, cn]), "\n")
      }
    }
  }
}

# write out data information
cat(paste0("[", Sys.time(), "]: Writing out data information\n"))

write.csv(info_dat, 
          file.path(cleaned_folder,
                    "HSE_BHN_Data_Information.csv"),
          na = "",
          row.names = F)

# convert data to zcta ----

cat(paste0("[", Sys.time(), "]: Converting data to ZCTA level\n"))

# first, preallocate
zcta_df <-
  if (nrow(level_data[["ZCTA"]]) > 0){
    level_data[["ZCTA"]][, c(
      "GEOID",
      info_dat$Numerator[info_dat$`Geographic Level` == "ZCTA"],
      info_dat$Denominator[!is.na(info_dat$Denominator) &
        info_dat$`Geographic Level` == "ZCTA"]
      )
    ]
  } else {
    # use the data that was loaded in the crosswalk_func script
    data.frame(
      "GEOID" = as.character(all_zctas)
    )
  }
# set the rownames to be the zctas
rownames(zcta_df) <- zcta_df$GEOID

# now go through the rest of the geolevels and convert them to ZCTA
for (gl in geo_levels[geo_levels != "ZCTA"]){
  # subset to the data at the specific geographic level
  info_dat_sub <- info_dat[info_dat$`Geographic Level` == gl,]
  
  if (nrow(info_dat_sub) > 0){
    zcta_conversion <- 
      if (gl == "County"){
        county_to_zcta(
          level_data[[gl]], 
          "GEOID", 
          c(info_dat_sub$Numerator, 
            info_dat_sub$Denominator[!is.na(info_dat_sub$Denominator)])
          )  
      } else if (gl == "Census Tract"){
        ct_to_zcta(
          level_data[[gl]], 
          "GEOID", 
          c(info_dat_sub$Numerator, 
            info_dat_sub$Denominator[!is.na(info_dat_sub$Denominator)])
        )  
      } else if (gl == "ZIP Code"){
        ct_to_zcta(
          level_data[[gl]], 
          "GEOID", 
          c(info_dat_sub$Numerator, 
            info_dat_sub$Denominator[!is.na(info_dat_sub$Denominator)]),
          use_mean = T
        )  
      }
    
    # add to main
    zcta_df[zcta_conversion$ZCTA, 
            colnames(zcta_conversion)[colnames(zcta_conversion) != "ZCTA"]] <-
      zcta_conversion[, colnames(zcta_conversion) != "ZCTA"]
    # reupdate the rownames
    rownames(zcta_df) <- zcta_df$GEOID
    
  }
}

# need to filter out ZCTAs not in US -- filtered out in crosswalk file
zcta_df <- zcta_df[zcta_df$GEOID %in% all_zctas,]

# scale and combine each measure ----

cat(paste0("[", Sys.time(), "]: Combining and scaling measures\n"))

# TODO: CLEAN UP ENVIRONMENT
# TODO: ADD CHECKS

# first, allocate with only numerators
meas_df <- zcta_df[, !colnames(zcta_df) %in% info_dat$Denominator]

# then, we want to divide by denominators (if they have one)
meas_df[, info_dat$Numerator[!is.na(info_dat$Denominator)]] <-
  meas_df[, info_dat$Numerator[!is.na(info_dat$Denominator)]]/
  zcta_df[, info_dat$Denominator[!is.na(info_dat$Denominator)]]

# then we want to scale all of them (should have a scale)
meas_df[, info_dat$Numerator] <- 
  sweep(meas_df[, info_dat$Numerator], MARGIN = 2, info_dat$Scale, `*`)

cat(paste0("[", Sys.time(), "]: Write out converted, scaled data\n"))

write.csv(meas_df, 
          file.path(cleaned_folder,
                    "HSE_BHN_ZCTA_Converted_Measures.csv"),
          na = "",
          row.names = F)

# directionality and percentile scaling ----

cat(paste0("[", Sys.time(), "]: Percentile ranking measures\n"))

# allocate the percentile scaled dataframe
perc_meas_df <- meas_df

# then we want to put them in the correct directionality
# (higher score == higher need)
perc_meas_df[, info_dat$Numerator] <- 
  sweep(perc_meas_df[, info_dat$Numerator], 
        MARGIN = 2, 
        info_dat$Directionality, 
        `*`)

# do percentile ranking
perc_meas_df[,-1] <- apply(perc_meas_df[,-1], MARGIN = 2, perc.rank)

cat(paste0("[", Sys.time(), "]: Write out percentile ranked data\n"))

write.csv(perc_meas_df, 
          file.path(cleaned_folder,
                    "HSE_BHN_ZCTA_Percentile_Ranked_Measures.csv"),
          na = "",
          row.names = F)
