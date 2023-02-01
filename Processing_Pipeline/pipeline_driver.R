# Main Data Processing Pipeline File
# By HSE Team
# Originated on: 6/28/21

# load libraries and overarching data ----

library(readxl)

# folder where all the data and information for the pipeline is
data_folder <- file.path("Data")

# folder where all the preprocessed data is
preprocessed_folder <- file.path(data_folder, "Preprocessed")

# folder where all the cleaned/output data goes
cleaned_folder <- file.path(data_folder, "Cleaned")

# load measure registry -- first sheet
m_reg <- as.data.frame(
  read_excel(file.path(data_folder, "Metadata.xlsx"), sheet = 1)
)
# remove everything that doesn't have a numerator
m_reg <- m_reg[!is.na(m_reg$Numerator),]

# load supplemental data/functions ----

# load crosswalk functions and information
source(file.path("Processing_Pipeline", "crosswalk_func.R"))

# load relationship files (2010 to 2020) and information
source(file.path("Processing_Pipeline", "relationship_func.R"))

# load population totals
all_pop_df <- read.csv(
  file.path(data_folder, "Resources", "ACS_ZCTA_Total_Populations.csv"),
  colClasses = c("GEOID" = "character")
)

# note -- we ignore missing data when doing ranking
# function to compute percentile ranking
perc.rank <- function(x) {
  return(rank(x, na.last = "keep")/length(x[!is.na(x)])*100)
}

# function to get the final score for a given set of measures
# pm: percentile-ranked measure data frame, with GEOID as the first column
# type: "pop" or "black" is it full population or race stratified
# returns final score, combined with weights
get_final_score <- function(pm, type = "pop", info_dat = info_dat){
  # now created the weighted measures
  # first multiply the original weights
  tot_weighted_meas <- sweep(
    pm[, colnames(pm) != "GEOID"], 
    MARGIN = 2,
    info_dat[colnames(pm)[colnames(pm) != "GEOID"], "Effective_Weights"], 
    `*`)
  
  # create which weights should be included for each row (for missing data)
  wt_df <- data.frame(
    matrix(info_dat[colnames(pm)[colnames(pm) != "GEOID"], "Effective_Weights"],
           ncol = ncol(pm)-1)
  )
  wt_df <- rbind(wt_df, wt_df[rep(1, nrow(tot_weighted_meas)-1),])
  wt_df[is.na(tot_weighted_meas)] <- NA
  wt_sum <- rowSums(wt_df, na.rm = T)
  # NOTE: we want to return this scaled in the future
  
  # create the appropriately weighted measures
  weighted_meas <- sweep(
    tot_weighted_meas, 
    MARGIN = 1,
    wt_sum, 
    `/`)
  
  # preliminary score is weighted limited score
  prelim_score <- rowSums(weighted_meas, na.rm = T)
  # remove score for places under some threshold
  # NOTE: now, the place has to have at least 1 person
  prelim_score[
    pm$GEOID %in% 
      all_pop_df$GEOID[all_pop_df[,paste0("total_", type)] < 1]
    ] <- NA
  # remove score for places with over 50% of weights missing 
  prelim_score[wt_sum < 50] <- NA
  
  # then we rescale the score -- using 5% and 99.5%
  low_score <- quantile(prelim_score, probs = .005, na.rm = T)
  high_score <- quantile(prelim_score, probs = .995, na.rm = T)
  rescale_score <- 
    (prelim_score - low_score)/(high_score - low_score)*100
  # everything outside those percentiles goes to the edges
  rescale_score[prelim_score <= low_score] <- 0
  rescale_score[prelim_score >= high_score] <- 100
  
  return(rescale_score)
}

# allocate overarching variables ----

geo_levels <- c(
  "ZCTA",
  "County",
  "Census Tract",
  "ZIP Code"
)

# pipeline function ----

# inputs:
#  m_reg_custom: metadata file
#  custom_data: list of dataframes of custom data
#  run_custom: are we adding custom data?
#  z_enter: list of ZCTAs to subset to, if running custom
mwi_pipeline <- function(m_reg_custom = m_reg, custom_data = list(), 
                         run_custom = F, z_enter = c()){
  
  # saving for the overall output
  overall_out <- list()
  
  # custom data, replace
  if (run_custom){
    m_reg <- m_reg_custom
  }
  
  # if the custom data list is empty, we're just adjusting weights (and data
  # has already been processed) -- we can skip steps
  upd_weights <- length(custom_data) == 0 & run_custom
  
  # filter out measures with weight of 0 and an empty numerator
  m_reg <- m_reg[m_reg$Weights != 0,]
  m_reg <- m_reg[!is.na(m_reg$Numerator),]
  rownames(m_reg) <- m_reg$Numerator
  
  if (run_custom){
    overall_out[["m_reg"]] <- m_reg
  }
  
  if (!upd_weights){
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
        if (!fn %in% names(custom_data)){
          curr_df <- read.csv(file.path(preprocessed_folder, fn),
                              colClasses = setNames(
                                "character",
                                geoid_colname
                              ))
        } else {
          curr_df <- custom_data[[fn]]
        }
        
        curr_df <- check_geoid(curr_df, geoid_colname, type = gl)
        
        # getting columns that need to be converted 
        # (numerators and denominators)
        var_df <- rbind(m_reg_sub, m_reg_sub[m_reg_sub$`Race Stratification`,])
        
        is_preprocessed <- var_df$`Is Preprocessed`
        not_dup_num <- !duplicated(var_df$Numerator)
        not_dup_den <- !duplicated(var_df$Denominator) & !is.na(var_df$Denominator)
        var_df$Numerator[is_preprocessed & not_dup_num] <-
          paste0(var_df$Numerator[is_preprocessed & not_dup_num], "_pop")
        var_df$Denominator[is_preprocessed & not_dup_den] <-
          paste0(var_df$Denominator[is_preprocessed & not_dup_den], "_pop")
        
        # add black for ones that are preprocessed and are not pop
        not_pop_num <- !grepl("_pop", var_df$Numerator)
        not_pop_den <- !grepl("_pop", var_df$Denominator) & 
          !is.na(var_df$Denominator)
        var_df$Numerator[is_preprocessed & not_pop_num] <-
          paste0(var_df$Numerator[is_preprocessed & not_pop_num], "_black")
        var_df$Denominator[is_preprocessed & not_pop_den] <-
          paste0(var_df$Denominator[is_preprocessed & not_pop_den], "_black")
        
        meas_col <- c(var_df$Numerator, var_df$Denominator)
        meas_col <- meas_col[!is.na(meas_col)]
        meas_col <- meas_col[meas_col %in% colnames(curr_df)]
        
        # get rid of duplicate GEOID rows by averaging -- no GEOIDs should be
        # duplicated
        if (any(duplicated(curr_df[, geoid_colname]))){
          dup_vals <- curr_df[, geoid_colname] %in% 
            curr_df[, geoid_colname][duplicated(curr_df[, geoid_colname])]
          
          tmp <- aggregate(curr_df[dup_vals, meas_col], 
                           by = list("GEOID" = curr_df[, geoid_colname][dup_vals]),
                           FUN = mean, na.rm = T)
          colnames(tmp)[-1] <- meas_col
          
          curr_df <- curr_df[!duplicated(curr_df[, geoid_colname]),]
          rownames(curr_df) <- curr_df[, geoid_colname]
          curr_df[tmp$GEOID, meas_col] <- tmp[, meas_col]
        }
        
        # first, map from 2010 to 2020 if needed
        # applies to census tracts and ZCTAs -- counties have no change, zip
        # codes are only converted once converted to ZCTAs
        if (m_reg_sub$`Geographic Boundaries`[m_reg_sub$Filename == fn][1] ==
             2010){
          
          if (gl == "Census Tract"){
            
            curr_df <- ct_10_to_20(curr_df, 
                                   geoid_colname,
                                   meas_col)
          } else if (gl == "ZCTA"){
            curr_df <- zcta_10_to_20(curr_df, 
                                     geoid_colname,
                                     meas_col)
          }
        }
        
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
  }
  
  # get/output variable information ----
  
  # TODO: ADD TRYCATCH/DATA QUALITY FILTERING
  
  cat(paste0("[", Sys.time(), "]: Calculating data information\n"))
  
  # preallocate output data -- this will also be useful later
  info_dat <- as.data.frame(m_reg)
  # add additional information
  info_dat[, c("Is_Numeric", "Minimum", "Maximum", "Missing", "Number_Rows",
               "Total_Penalty","Effective_Weights")] <-
    NA
  
  # rescale weights put in "effective_weights"
  # empty weight are 0
  info_dat$Weights[is.na(info_dat$Weights)] <- 0
  info_dat$Effective_Weights <- info_dat$Weights/sum(info_dat$Weights)*100
  # now add overall penalties
  info_dat$Total_Penalty <- 0
  # georgaphic granularity
  geo_pen <- .1
  info_dat$Total_Penalty[!info_dat$`Geographically Granular`] <- 
    info_dat$Total_Penalty[!info_dat$`Geographically Granular`] + geo_pen
  # no race stratification possible
  race_pen <- .1
  info_dat$Total_Penalty[info_dat$`Race Stratification Not Available`] <- 
    info_dat$Total_Penalty[info_dat$`Race Stratification Not Available`] + race_pen
  # now update weights with overall penalty
  # first subtract the weight amount
  info_dat$Effective_Weights <- 
    info_dat$Effective_Weights - info_dat$Effective_Weights*info_dat$Total_Penalty 
  
  if (!run_custom){
    # now we need to rescale back to big bucket weights
    # amounts
    wt_amt <- c(
      "sdoh" = 60, # SDOH
      "ha" = 15, # healthcare access
      "hs" = 25 # health status
    )
    # names
    cat_names <- c(
      "sdoh" = "Social Determinants of Health",
      "ha" = "Healthcare Access",
      "hs" = "Health Status"
    )
    for (cn in names(cat_names)){
      cn_log <- info_dat$Category == cat_names[cn]
      
      # add the penalties
      info_dat$Effective_Weights[cn_log] <- 
        info_dat$Effective_Weights[cn_log]-
        (info_dat$Effective_Weights[cn_log]*info_dat$Total_Penalty[cn_log])
      
      # scale the weights to the whole category
      info_dat$Effective_Weights[cn_log] <- 
        info_dat$Effective_Weights[cn_log]/
        sum(info_dat$Effective_Weights[cn_log])*wt_amt[cn]
    }
  } else {
    # we don't impose categories on given data, just rescale to 100
    info_dat$Effective_Weights <- 
      info_dat$Effective_Weights/
      sum(info_dat$Effective_Weights)*100
  }
  
  
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
  
  
  if (!upd_weights){
    # where we're going to put all the data
    for (gl in geo_levels){
      # subset to the data at the specific geographic level
      info_dat_sub <- info_dat[info_dat$`Geographic Level` == gl,]
      
      if (nrow(info_dat_sub) > 0){
        # we'll go through each column and compute information for each
        info_dat[info_dat_sub$Numerator, "Is_Numeric"] <- 
          sapply(level_data[[gl]][, info_dat_sub$Numerator, drop = F],
                 is.numeric)
        info_dat[info_dat_sub$Numerator, "Minimum"] <- 
          sapply(level_data[[gl]][, info_dat_sub$Numerator, drop = F], 
                 function(x){min(x, na.rm = T)})
        info_dat[info_dat_sub$Numerator, "Maximum"] <- 
          sapply(level_data[[gl]][, info_dat_sub$Numerator, drop = F], 
                 function(x){max(x, na.rm = T)})
        info_dat[info_dat_sub$Numerator, "Missing"] <- 
          sapply(level_data[[gl]][, info_dat_sub$Numerator, drop = F], 
                 function(x){sum(is.na(x))})
        info_dat[info_dat_sub$Numerator, "Number_Rows"] <- 
          sapply(level_data[[gl]][, info_dat_sub$Numerator, drop = F], 
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
  } else {
    # we will fill in this info from the original
    info_orig <- read.csv(file.path(cleaned_folder,
                                    "HSE_MWI_Data_Information.csv"),
                          check.names = F)
    rownames(info_orig) <- info_orig$Numerator
    
    cn <- c("Is_Numeric", 
            "Minimum", 
            "Maximum", 
            "Missing", 
            "Number_Rows")
    info_dat[, cn] <- info_orig[info_dat$Numerator, cn]
    
  }
  
  # write out data information
  cat(paste0("[", Sys.time(), "]: Writing out data information\n"))
  
  # save if we're running original data, keep inside if we're running custom data
  if (run_custom){
    overall_out[["info_dat"]] <- info_dat
  } else {
    write.csv(info_dat, 
              file.path(cleaned_folder,
                        "HSE_MWI_Data_Information.csv"),
              na = "",
              row.names = F)
  }
  
  # convert data to zcta ----
  
  if (!upd_weights){
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
            # for zip code, we need to map from zip to zcta 2010 and then to
            # zcta 2022 (no 2022 direct mappings are out yet)
            zcta_res <- 
              zip_to_zcta(
                level_data[[gl]], 
                "GEOID", 
                c(info_dat_sub$Numerator, 
                  info_dat_sub$Denominator[!is.na(info_dat_sub$Denominator)]),
                use_mean = T
              )
            zcta_res <- zcta_res[!is.na(zcta_res$ZCTA),]
            
            zcta_10_to_20(
              zcta_res, 
              "ZCTA",
              c(info_dat_sub$Numerator, 
                info_dat_sub$Denominator[!is.na(info_dat_sub$Denominator)]))
          }
        
        # add to main
        zcta_df[zcta_conversion$ZCTA, 
                colnames(zcta_conversion)[colnames(zcta_conversion) != "ZCTA"]] <-
          zcta_conversion[, colnames(zcta_conversion) != "ZCTA", drop = F]
        # reupdate the rownames
        rownames(zcta_df) <- zcta_df$GEOID
        
      }
    }
    
    # need to filter out ZCTAs not in US -- filtered out in crosswalk file
    zcta_df <- zcta_df[zcta_df$GEOID %in% all_zctas,]
    
    # if using custom data, we need to filter out all ZCTAs with no data for our
    # custom data
    if (run_custom & length(custom_data) > 0){
      for (fn in names(custom_data)){
        # get the numerators that correspond to the file
        all_num <- m_reg$Numerator[m_reg$Filename == fn]
        
        # go through each and filter to only zctas that are nonmissing in the
        # custom data
        for (nm in all_num){
          if (m_reg$`Is Preprocessed`[m_reg$Numerator == nm]){
            nm <- paste0(nm, "_pop")
          }
          
          zcta_df <- zcta_df[!is.na(zcta_df[,nm]),]
        }
      }
      
      # subset all_pop_df to ZCTAs in zcta_df
      all_pop_df <- all_pop_df[all_pop_df$GEOID %in% zcta_df$GEOID,]
    }
  }
  
  # scale and combine each measure ----
  
  if (!upd_weights){
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
    
    # for all data, if the population is 0, mark as missing
    # if not already in missing
    rownames(meas_df) <- as.character(meas_df$GEOID)
    meas_df[all_pop_df$GEOID[all_pop_df$total_black < 1],
            !grepl("_pop", colnames(meas_df)) & 
              colnames(meas_df) %in% 
              info_dat$Numerator[info_dat$`Race Stratification`]] <- NA
    meas_df[all_pop_df$GEOID[all_pop_df$total_pop < 1],
            grepl("_pop", colnames(meas_df)) | 
              colnames(meas_df) %in% 
              info_dat$Numerator[!info_dat$`Race Stratification`]] <- NA
    
    cat(paste0("[", Sys.time(), "]: Write out converted, scaled data\n"))
    
    if (run_custom){
      overall_out[["meas_df"]] <- meas_df
    } else {
      write.csv(meas_df, 
                file.path(cleaned_folder,
                          "HSE_MWI_ZCTA_Converted_Measures.csv"),
                na = "",
                row.names = F)
    }
  }
  
  # directionality and percentile scaling ----
  
  # percentile rank/scale depending on if there are zctas we want to subset to
  if (upd_weights){
    # if it's custom, we need to save all in the overall output vector
    meas_df <- read.csv(file.path(cleaned_folder,
                                  "HSE_MWI_ZCTA_Converted_Measures.csv"),
                        check.names = F,
                        colClasses = c(
                          "GEOID" = "character"
                        ))
    
    if (length(z_enter) > 0){
      meas_df <- meas_df[meas_df$GEOID %in% unname(z_enter),]
    }
    
    meas_df <- meas_df[meas_df$GEOID != "",]
    rownames(meas_df) <- meas_df$GEOID
    
    overall_out[["meas_df"]] <- meas_df
  }
  
  if (!upd_weights |
      (upd_weights & length(z_enter) > 0)){
    cat(paste0("[", Sys.time(), "]: Percentile ranking measures\n"))
    
    # allocate the percentile scaled dataframe
    perc_meas_df <- meas_df
    
    # keep ranking of original directionality -- "more means more"
    # for visualization purposes
    no_dir_perc_meas_df <- perc_meas_df
    no_dir_perc_meas_df[,-1] <- apply(no_dir_perc_meas_df[,-1], MARGIN = 2, perc.rank)
    no_dir_perc_meas_df <- no_dir_perc_meas_df[no_dir_perc_meas_df$GEOID != "",]
    
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
    
    if (run_custom){
      no_dir_perc_meas_df <- 
        no_dir_perc_meas_df[no_dir_perc_meas_df$GEOID != "",]
      rownames(no_dir_perc_meas_df) <- no_dir_perc_meas_df$GEOID
      colnames(no_dir_perc_meas_df)[colnames(no_dir_perc_meas_df) == "GEOID"] <- "ZCTA"
      
      overall_out[["perc_meas_df"]] <- perc_meas_df
      overall_out[["no_dir_perc_meas_df"]] <- no_dir_perc_meas_df
    } else {
      write.csv(perc_meas_df, 
                file.path(cleaned_folder,
                          "HSE_MWI_ZCTA_Percentile_Ranked_Measures.csv"),
                na = "",
                row.names = F)
      
      write.csv(no_dir_perc_meas_df, 
                file.path(cleaned_folder,
                          "HSE_MWI_ZCTA_No_Directionality_Percentile_Ranked_Measures.csv"),
                na = "",
                row.names = F)
    }
  } else {
    # if we're just changing the weights, we can just read the processed data 
    # here
    perc_meas_df <- read.csv(
      file.path(cleaned_folder,
                "HSE_MWI_ZCTA_Percentile_Ranked_Measures.csv"),
      check.names = F,
      colClasses = c(
        "GEOID" = "character"
      )
    )
    
    no_dir_perc_meas_df <- read.csv(
      file.path(cleaned_folder,
                "HSE_MWI_ZCTA_No_Directionality_Percentile_Ranked_Measures.csv"),
      check.names = F,
      colClasses = c(
        "GEOID" = "character"
      ))
    
    
    if (run_custom){
      no_dir_perc_meas_df <- 
        no_dir_perc_meas_df[no_dir_perc_meas_df$GEOID != "",]
      rownames(no_dir_perc_meas_df) <- no_dir_perc_meas_df$GEOID
      colnames(no_dir_perc_meas_df)[colnames(no_dir_perc_meas_df) == "GEOID"] <- "ZCTA"
      
      perc_meas_df <- 
        perc_meas_df[perc_meas_df$GEOID != "",]
      rownames(perc_meas_df) <- perc_meas_df$GEOID
      
      overall_out[["perc_meas_df"]] <- perc_meas_df
      overall_out[["no_dir_perc_meas_df"]] <- no_dir_perc_meas_df
    }
  }
  
  # create final scores for population and black ----
  
  cat(paste0("[", Sys.time(), "]: Create final scores\n"))
  
  # now is where we separate out the population/black/both measures
  pop_pm <- perc_meas_df[, !grepl("_black", colnames(perc_meas_df))]
  black_pm <- 
    perc_meas_df[, !grepl("_pop", colnames(perc_meas_df)) | 
                   colnames(perc_meas_df) %in% 
                   info_dat$Numerator[!info_dat$`Race Stratification`]]
  
  # get final, scaled scores
  pop_pm$Mental_Wellness_Index  <- get_final_score(pop_pm, type = "pop", info_dat = info_dat)
  black_pm$Mental_Wellness_Index  <- get_final_score(black_pm, type = "black", info_dat = info_dat)
  
  # reorder columns, for ease of reading
  pop_pm <- pop_pm[,c(
    "GEOID", "Mental_Wellness_Index", 
    colnames(pop_pm)[!colnames(pop_pm) %in% c("GEOID", "Mental_Wellness_Index")])]
  black_pm <- black_pm[,c(
    "GEOID", "Mental_Wellness_Index", 
    colnames(black_pm)[!colnames(black_pm) %in% c("GEOID", "Mental_Wellness_Index")])]
  # rename GEOID column to ZCTA
  colnames(pop_pm)[1] <- "ZCTA"
  colnames(black_pm)[1] <- "ZCTA"
  
  cat(paste0("[", Sys.time(), "]: Write out final indices\n"))
  
  if (run_custom){
    overall_out[["pop_pm"]] <- pop_pm
    overall_out[["black_pm"]] <- black_pm
  } else {
    write.csv(pop_pm, 
              file.path(cleaned_folder,
                        "HSE_MWI_ZCTA_Mental_Wellness_Index_Population.csv"),
              na = "",
              row.names = F)
    
    write.csv(black_pm, 
              file.path(cleaned_folder,
                        "HSE_MWI_ZCTA_Mental_Wellness_Index_Black.csv"),
              na = "",
              row.names = F)
  }
  
  return(overall_out)
}