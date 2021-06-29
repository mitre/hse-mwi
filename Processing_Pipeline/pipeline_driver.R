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
preprocessed_folder <- file.path(data_folder, "Preprocessing")

# load measure registry


# load supplemental data/functions ----

# load crosswalk functions and information
source(file.path("Processing_Pipeline", "crosswalk_func.R"))