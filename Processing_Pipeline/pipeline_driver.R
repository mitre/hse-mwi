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

# preallocate output data
info_dat <- as.data.frame(
  m_reg[, c("Measure", "Measure Type", "Source", "Numerator", 
            "Is Preprocessed", "Race Stratification", "Geographic Level")]
)
# add additional information
info_dat[, c("Is_Numeric", "Minimum", "Maximum", "Missing", "Number_Rows")] <-
  NA
# duplicate rows that are race stratified
info_dat <- rbind(info_dat, info_dat[m_reg$`Race Stratification`,])
# add pop for ones that are preprocessed but not duplicated
info_dat$Numerator[
  info_dat$`Is Preprocessed` & !duplicated(info_dat$Numerator)
  ] <-
  paste0(info_dat$Numerator[
    info_dat$`Is Preprocessed` & !duplicated(info_dat$Numerator)
  ], "_pop")
# add black for ones that are preprocessed and are not pop
info_dat$Numerator[
  info_dat$`Is Preprocessed` & !grepl("_pop", info_dat$Numerator)
] <-
  paste0(info_dat$Numerator[
    info_dat$`Is Preprocessed` & !grepl("_pop", info_dat$Numerator)
  ], "_black")
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