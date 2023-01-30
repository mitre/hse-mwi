# Creating Treatment Facility Weights File

# Author: Karen Jiang & Emily Pantalone
# Version: 2021-11-02

# Packages
library(tidyverse)
library(readxl)
# Parameters

#===============================================================
# Code


## Reading in Data Files -----
data_folder <- file.path(
  gsub("\\\\","/", gsub("OneDrive - ","", Sys.getenv("OneDrive"))), 
  "Health and Social Equity - SJP - BHN Score Creation",
  "Data", "Raw", "SAMHSA_Locator")

weights <- read_xlsx(file.path(data_folder, "Documentation_Behavioral_Health_Treament_Facility_listing_COUNTS_NEW.xlsx"), "Sheet1")

mh <- read.csv(file.path(data_folder, "Mental_Health_FindTreament_Facility_listing_2023_01_24_172825.csv"))
su <- read.csv(file.path(data_folder, "Substance_Use_FindTreament_Facility_listing_2023_01_24_172855.csv"))


## Functions -----
# Function to select the weights for each group
get_weights_codes <- function(subject = c("mh", "sa"),
                              grouping = c("Access", 
                                           "Special Groups of Focus", 
                                           "Continuum of Treatment", 
                                           "Continuum of Care")){
  
  # Conditionals based on whether MH facilities are used or SA 
  if (subject == "mh") {
    names <- weights %>%
      filter(`Proposed Grouping` == grouping,
             mh_code == TRUE,
             `Use for MH?` == "Y") %>%
      select(service_name, service_code, `Grouped ID`, thresh) %>%
      mutate(service_code = tolower(service_code))
  } else if (subject == "sa") {
    names <- weights %>%
      filter(`Proposed Grouping` == grouping,
             sa_code == TRUE,
             `Use for SA?` == "Y") %>%
      select(service_name, service_code, `Grouped ID`, thresh) %>%
      mutate(service_code = tolower(service_code))
  }
  
  names
}



# Plot distributions based on selected variables 
generate_facility_scores <- function(subject = c("mh", "sa"),
                                     grouping = c("Access", 
                                                  "Special Groups of Focus", 
                                                  "Continuum of Treatment", 
                                                  "Continuum of Care"),
                                     printnames = FALSE){
  
  # Get the names of the columns for each dimension of quality 
  names <- get_weights_codes(subject, grouping)
  
  if(subject == "mh"){
    dat <- mh
  } else if (subject == "sa"){
    dat <- su
  }
  
  # Creating new columns with for any grouped variables (i.e. offers non-english / non-spanish)
  for (col in unique(names$`Grouped ID`)) {
    
    # Only runs if there are grouped columns 
    if (!is.na(col)) {
      cn <- names %>%
        filter(`Grouped ID` == col) %>%
        as.data.frame()
      
      # filter out service codes that no longer exist (some languages)
      cn <- cn[cn$service_code %in% colnames(mh),]
      
      # If facility provides any of the services listed in the group, facility receives point, otherwise 0.
      dat[, col] <- ifelse(rowSums(dat[, cn$service_code, drop = F], na.rm = T) >= unique(cn$thresh), 1, 0)
    }
  }
  
  # Renames service_code value with the Grouped ID
  names$service_code <- ifelse(is.na(names$`Grouped ID`), 
                               names$service_code,
                               names$`Grouped ID`)
  
  # Selects columns used to create score
  facility <- dat %>% 
    select(unique(names$service_code)) %>%
    mutate(points = rowSums(., na.rm = T))
  facility$name <- paste(dat$name1, dat$name2)
  
  # Return the names of the columns used
  
  if (printnames) {
    return(names %>%
             select(-`Grouped ID`) %>%
             arrange(service_code)) 
  }
  # Otherwise return the facilities
  return(facility)
  
}


## Combine Weights -------

# Mental Health Facility Weights
mh_access <- generate_facility_scores("mh", "Access") %>% 
  mutate(access = points / max(points)) %>%
  select(name, access)

mh_spgrp <- generate_facility_scores("mh", "Special Groups of Focus") %>%
  mutate(spgrp = points / max(points)) %>%
  select(spgrp)

mh_cot <- generate_facility_scores("mh", "Continuum of Treatment") %>%
  mutate(cot = points / max(points)) %>%
  select(cot)

mh_coc <- generate_facility_scores("mh", "Continuum of Care") %>%
  mutate(coc = points / max(points)) %>%
  select(coc)

mh_facility <- cbind(mh_access, mh_spgrp, mh_cot, mh_coc) %>%
  mutate(weight = (access+spgrp+cot+coc)/4) %>%
  select(name, weight)


# Substance Use Facility Weights
su_access <- generate_facility_scores("sa", "Access") %>% 
  mutate(access = points / max(points)) %>%
  select(name, access)

su_spgrp <- generate_facility_scores("sa", "Special Groups of Focus") %>%
  mutate(spgrp = points / max(points)) %>%
  select(spgrp)

su_cot <- generate_facility_scores("sa", "Continuum of Treatment") %>%
  mutate(cot = points / max(points)) %>%
  select(cot)

su_coc <- generate_facility_scores("sa", "Continuum of Care") %>%
  mutate(coc = points / max(points)) %>%
  select(coc)

su_facility <- cbind(su_access, su_spgrp, su_cot, su_coc) %>%
  mutate(weight = (access+spgrp+cot+coc)/4) %>%
  select(name, weight)


# write out ----

data_folder <- file.path(
  gsub("\\\\","/", gsub("OneDrive - ","", Sys.getenv("OneDrive"))), 
  "Health and Social Equity - SJP - BHN Score Creation",
  "Data", "Raw", "SAMHSA_Locator")

write.csv(mh_facility, 
          file = file.path(
            data_folder,
            "SAMHSA_MHFacility_weights.csv"
          ), 
          row.names = F, 
          na = "")

write.csv(su_facility, 
          file = file.path(
            data_folder,
            "SAMHSA_SUFacility_weights.csv"
          ), 
          row.names = F, 
          na = "")
