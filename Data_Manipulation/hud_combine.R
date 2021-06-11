# Combining HUD data
# By Karen Jiang
# Originated on: 6/11/21

library(tidyverse)
# load data and combine ----

data_folder <- file.path(
  gsub("\\\\","/", gsub("OneDrive - ","", Sys.getenv("OneDrive"))), 
  "Health and Social Equity - SJP - BHN Score Creation",
  "Data", "Raw", "HUD")


df <- read.csv(paste0(data_folder, "/Table1.csv"))

housing_stress <- df %>% 
  # Converting existing geoid into 11-digit ID
  separate(geoid, into = c("country code", "geoid"), sep = "US") %>%
  
  # T1_est3 :  Owner Occupied - has 1 or more of the 4 housing unit problems 
  # (lacks kitchen or plumbing, more than 1 person per room, or cost burden greater than 30%)
  # T1_est127: Renter Occupied - has 1 or more of the 4 housing unit problems 
  # (lacks kitchen or plumbing, more than 1 person per room, or cost burden greater than 30%)
  # T1_est2: All Owner Occupied
  # T1_est126: All Renter Occupied
  mutate(housing_stress = (T1_est3 + T1_est127)/(T1_est2 + T1_est126) * 100) %>%
  select(geoid, housing_stress)
  

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