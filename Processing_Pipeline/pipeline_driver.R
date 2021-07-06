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
