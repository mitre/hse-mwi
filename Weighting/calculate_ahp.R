# Calculate Weights from AHP

# Author: Karen Jiang
# Version: 2021-12-09

# Packages
library(dplyr)
library(readxl)

# References
# https://training.fws.gov/courses/references/tutorials/geospatial/CSP7306/Readings/AHP-Technique.pdf

#===============================================================
# Code


# Read in data from excel
data_folder <- file.path(
  gsub("\\\\","/", gsub("OneDrive - ","", Sys.getenv("OneDrive"))), 
  "Health and Social Equity - SJP - BHN Score Creation",
  "Data")
ahp_form <- read_xlsx(file.path(data_folder, "Mental Wellness Index™ (MWI) Weighting.xlsx"))



convert_response <- function(x) {
  recode(x,
       "A is much more important than B" = 9,
       "A is moderately more important than B" = 5,
       "A and B are equally important" = 1,
       "B is moderately more important than A" = 1/5,
       "B is much more important than A" = 1/9,
       .default = 0)
}

ahp_recode <- ahp_form %>% mutate_at(vars(contains("vs")), convert_response)
respondent <- ahp_recode %>% tail(1)

# Create Matricies -----

    ## Matrix 1: Domains
domains <- diag(3)
colnames(domains) <- rownames(domains) <- c("SDOH", "HC", "HS")
domains[1,2] <- respondent$`Social Determinants of Health (A) vs. Healthcare Access (B)`
domains[1,3] <- respondent$`Social Determinants of Health (A) vs Health Status (B)`
domains[2,3] <- respondent$`Healthcare Access (A) vs. Health Status (B)`

domains[2,1] <- 1/domains[1,2]
domains[3,1] <- 1/domains[1,3]
domains[3,2] <- 1/domains[2,3]


    ## Matrix 2: SDOH
sdoh <- diag(7)
colnames(sdoh) <- rownames(sdoh) <- c("env", "edu", "inc", "hous", "social", "trauma", "fin")
# Fill Responses into matrix
sdoh[1,2] <- respondent$`Built Environment (A) vs. Education Success (B)`
sdoh[3,4] <- respondent$`Income & Employment (A) vs. Housing (B)`
sdoh[5,6] <- respondent$`Social Capital (A) vs. Safety & Community Trauma (B)`
sdoh[1,7] <- 1/respondent$`Financial Access (A) vs. Built Environment (B)`
sdoh[2,3] <- respondent$`Education Success (A) vs. Income & Employment (B)`
sdoh[4,5] <- respondent$`Housing (A) vs. Social Capital (B)`
sdoh[6,7] <- respondent$`Safety & Community Trauma (A) vs. Financial Access (B)`

# Transitive Property the top right
sdoh[1,3] <- sdoh[1,2]/sdoh[2,3]
sdoh[2,4] <- sdoh[2,3]/sdoh[3,4]
sdoh[3,5] <- sdoh[3,4]/sdoh[4,5]
sdoh[4,6] <- sdoh[4,5]/sdoh[5,6]
sdoh[5,7] <- sdoh[5,6]/sdoh[6,7]

sdoh[1,4] <- sdoh[1,3]/sdoh[3,4]
sdoh[2,5] <- sdoh[2,4]/sdoh[4,5]
sdoh[3,6] <- sdoh[3,5]/sdoh[5,6]
sdoh[4,7] <- sdoh[4,6]/sdoh[6,7]

sdoh[1,5] <- sdoh[1,4]/sdoh[4,5]
sdoh[2,6] <- sdoh[2,5]/sdoh[5,6]
sdoh[3,7] <- sdoh[3,6]/sdoh[6,7]

sdoh[1,6] <- sdoh[1,5]/sdoh[5,6]
sdoh[2,7] <- sdoh[2,6]/sdoh[6,7]

# Find inverse for rest of matrix
for(i in 2:nrow(sdoh)){
  for(j in 1:i){
    print(paste(i,j))
    sdoh[i,j] <- 1/sdoh[j,i]
  }
}


## Matrix 3: Healthcare Access
healthcare <- diag(3)
colnames(healthcare) <- rownames(healthcare) <- c("mh", "su", "ins")

healthcare[1,2] <- respondent$`Mental Health Treatment Facilities (A) vs. Substance Use Treatment Facilities (B)`
healthcare[1,3] <- respondent$`Mental Health Treatment Facilities (A) vs. Uninsured`
healthcare[2,3] <- respondent$`Substance Use Treatment Facilities (A) vs. Uninsured (B)`

healthcare[2,1] <- 1/healthcare[1,2]
healthcare[3,1] <- 1/healthcare[1,3]
healthcare[3,2] <- 1/healthcare[2,3]

## Matrix 4: Health Status
status <- diag(3)
colnames(status) <- rownames(status) <- c("mh", "su", "other")
status[2,1] <- respondent$`Substance Use morbidity and mortality (A) vs. Mental Health morbidity and mortality (B)`
status[2,3] <- respondent$`Substance Use morbidity and mortality (A) vs. Other morbidity and mortality (B)`
status[1,3] <- respondent$`Mental Health morbidity and mortality (A) vs. Other morbidity and mortality (B)`

status[1,2] <- 1/status[2,1]
status[3,2] <- 1/status[2,3]
status[3,1] <- 1/status[1,3]
# Function Consistency Ratio (check that CR is not > 0.1, otherwise judgments are untrustworthy )


# Calculate Relataive Value Vector (RVV)