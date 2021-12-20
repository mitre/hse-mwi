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
data_info <- read.csv(file.path(data_folder, "Cleaned", "HSE_MWI_Data_Information.csv"))


convert_response <- function(x) {
  recode(x,
       "A >>> B" = 7,
       "A >> B" = 5,
       "A > B" = 3,
       "A = B" = 1,
       "A < B" = 1/3,
       "A << B" = 1/5,
       "A <<< B" = 1/7,
       .default = 0)
}

ahp_recode <- ahp_form %>% mutate_at(vars(contains("vs")), convert_response)
respondent <- ahp_recode[7,]

# Create Matricies -------

    ## Matrix 1: Domains -----
domains <- diag(3)
colnames(domains) <- rownames(domains) <- unique(data_info$Category)
domains[1,2] <- respondent$`Social Determinants of Health (A) vs. Healthcare Access (B)`
domains[1,3] <- respondent$`Social Determinants of Health (A) vs Health Status (B)`
domains[2,3] <- respondent$`Healthcare Access (A) vs. Health Status (B)`

domains[2,1] <- 1/domains[1,2]
domains[3,1] <- 1/domains[1,3]
domains[3,2] <- 1/domains[2,3]


    ## Matrix 2: SDOH -------
sdoh <- diag(7)
nm <- unique(data_info[data_info$Category == "Social Determinants of Health", "Subcategory"])
colnames(sdoh) <- rownames(sdoh) <- nm[c(2,5,3,4,7,6,1)]
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
    sdoh[i,j] <- 1/sdoh[j,i]
  }
}

    ## Matrix 3: Healthcare Access -----
healthcare <- diag(3)
colnames(healthcare) <- rownames(healthcare) <- unique(data_info[data_info$Category == "Healthcare Access", "Measure"])

healthcare[1,2] <- respondent$`Mental Health Treatment Facilities (A) vs. Substance Use Treatment Facilities (B)`
healthcare[1,3] <- respondent$`Mental Health Treatment Facilities (A) vs. Uninsured`
healthcare[2,3] <- respondent$`Substance Use Treatment Facilities (A) vs. Uninsured (B)`

healthcare[2,1] <- 1/healthcare[1,2]
healthcare[3,1] <- 1/healthcare[1,3]
healthcare[3,2] <- 1/healthcare[2,3]

    ## Matrix 4: Health Status -------
status <- diag(3)
nm <- unique(data_info[data_info$Category == "Health Status", "Subcategory"])
colnames(status) <- rownames(status) <- nm[c(3,1,2)]
status[2,1] <- respondent$`Substance Use morbidity and mortality (A) vs. Mental Health morbidity and mortality (B)`
status[2,3] <- respondent$`Substance Use morbidity and mortality (A) vs. Other morbidity and mortality (B)`
status[1,3] <- respondent$`Mental Health morbidity and mortality (A) vs. Other morbidity and mortality (B)`

status[1,2] <- 1/status[2,1]
status[3,2] <- 1/status[2,3]
status[3,1] <- 1/status[1,3]

# Function Consistency Ratio (check that CR is not > 0.1, otherwise judgments are untrustworthy )

# # Example matrix
# A <- c(1,1/3, 1/9,1/5)
# B <- c(3,1,1,1)
# C <- c(9,1,1,3)
# D <- c(5,1,1/3,1)
# 
# test <- rbind(A,B,C,D)

# Calculate Eigen ----

calculate_weights <- function(matrix){
  nroot <- apply(matrix, 1, prod)^(1/ncol(matrix))
  eigen <- nroot / sum(nroot)
  mat <- cbind(matrix, nroot, eigen)
  return(mat)
}


calculate_consistency <- function(matrix) {
  weights <- calculate_weights(matrix)
  consistency <- rep(NA,nrow(weights))
  
  for(i in 1:nrow(weights)){
    consistency[i] <- sum(weights[i,1:(ncol(weights)-2)] * weights[,"eigen"])
  }
  
  gamma <- consistency / weights[,"eigen"]
  
  maxgamma <- mean(gamma)

  consistency_index <- (maxgamma-nrow(weights))/(nrow(weights)-1)
  
  judgement_table <- c(0, 0, 0.58, 0.90, 1.12, 1.24, 1.32, 1.41)
  
  ci <- consistency_index/judgement_table[nrow(matrix)]
  
  if (ci > 0.1) {
    cat(paste("The Consistency Index for", deparse(substitute(matrix)), "matrix is >0.1, which indicates that preferences may not be consistent. \n CI =", ci))
  } else {
    cat(paste("The Consistency Index for", deparse(substitute(matrix)), "is <=0.1. \n CI =", ci))
  }
}



weights_domains <- calculate_weights(domains)
weights_sdoh <- calculate_weights(sdoh)
weights_healthcare <- calculate_weights(healthcare)
weights_status <- calculate_weights(status)

meas_weights <- c(weights_sdoh[,"eigen"] * weights_domains["Social Determinants of Health", "eigen"],
                  weights_healthcare[,"eigen"] * weights_domains["Healthcare Access", "eigen"],
                  weights_status[,"eigen"] * weights_domains["Health Status", "eigen"])

calculate_consistency(domains)
calculate_consistency(sdoh)
calculate_consistency(healthcare)
calculate_consistency(status)
