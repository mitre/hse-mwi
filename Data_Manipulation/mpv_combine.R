# Combining HUD data
# By Karen Jiang
# Originated on: 6/11/21

library(tidyverse)

# load data and combine ----
data_folder <- file.path(
  gsub("\\\\","/", gsub("OneDrive - ","", Sys.getenv("OneDrive"))), 
  "Health and Social Equity - SJP - BHN Score Creation",
  "Data", "Raw", "HUD")


# write out ----

data_folder <- file.path(
  gsub("\\\\","/", gsub("OneDrive - ","", Sys.getenv("OneDrive"))), 
  "Health and Social Equity - SJP - BHN Score Creation",
  "Data", "Preprocessed")

write.csv(housing_stress, 
          file = file.path(
            data_folder,
            "HUD_CT_housingstress.csv"
          ), 
          row.names = F, 
          na = "")