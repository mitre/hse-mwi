# Preprocessing Mortgage Security Data
# By Hannah De los Santos
# Originated on: 8/13/21

# load data and libraries ----

# may also need to install bit64
library(data.table)

# import crosswalk functions
source(file.path("Processing_Pipeline", "crosswalk_func.R"))

# save space, don't load things that we don't need
rm(county_cw)
rm(zip_cw)

data_folder <- file.path(
  gsub("\\\\","/", gsub("OneDrive - ","", Sys.getenv("OneDrive"))), 
  "Health and Social Equity - SJP - BHN Score Creation",
  "Data", "Raw")

# load mortgage data
mort_df <- fread(file.path(
  data_folder,
  "HMDA",
  "HMDA_actions_taken_2-3-6-7-8_year_2021.csv" # national
),
colClasses = c("census_tract" = "character"))
# make sure the census tracts are correct
mort_df[, census_tract := str_pad(census_tract, 11, pad = "0")]

# only keep relevant columns
selected_cols <- c(
  "census_tract", "action_taken",
  "applicant_ethnicity-1", "applicant_ethnicity-2", "applicant_ethnicity-3", 
  "applicant_ethnicity-4", "applicant_ethnicity-5",
  "applicant_race-1", "applicant_race-2", "applicant_race-3", 
  "applicant_race-4", "applicant_race-5"
)
mort_df <- mort_df[, ..selected_cols]

# calculate mortgage outcomes ----

# i guess I don't need to match up beginning and end -- I just need to tally the outcome
# Action Taken:     
# 2 -- Application approved but not accepted -- approved
# 3 -- Application denied by financial institution  -- denied  
# 6 -- Loan purchased by the institution    -- approved??
# 7 -- Preapproval request denied by financial institution -- denied
# 8 -- Preapproval request approved but not accepted -- accepted

mort_df <- mort_df[action_taken %in% c(2,3,6,7,8),]

# add columns to aggregate/decide on
mort_df[, approved := action_taken %in% c(2,6,8)]
# mort_df[, denied := action_taken %in% c(3,7)] # we don't need denied
mort_df[, all_results := T]
mort_df[, is_black := 
          `applicant_race-1` == 3 |
          `applicant_race-2` == 3 |
          `applicant_race-3` == 3 |
          `applicant_race-4` == 3 |
          `applicant_race-5` == 3 
        ]
# to get nonwhite, we're going to get white
# there are no duplicates in race columns, so we only need to look at the first
# if they're only white
mort_df[, is_white_or_NA := 
          (`applicant_race-1` == 5 & `applicant_ethnicity-1` == 2) |
          (`applicant_race-1` == 5 & `applicant_ethnicity-1` == 3) |
          (`applicant_race-1` == 7 & `applicant_ethnicity-1` == 4) |
          (`applicant_race-1` == 6 & `applicant_ethnicity-1` == 3) 
]
mort_df[is.na(is_black), is_black := F]
mort_df[is.na(is_white_or_NA), is_white_or_NA := F]
mort_df[, is_nonwhite := !is_white_or_NA]

# aggregate all outcome measures
# population
mort_out <- mort_df[, .(
  all_results_pop = sum(all_results),
  approved_pop = sum(approved)
  ), by = census_tract]

# add black
mort_out <- merge(
  mort_out,
  mort_df[, .(
    all_results_black = sum(all_results[is_black]),
    approved_black = sum(approved[is_black])
  ), by = census_tract],
  all = T
)
# add nonwhite
mort_out <- merge(
  mort_out,
  # calculate nonwhite
  mort_df[, .(
    all_results_nonwhite = sum(all_results[is_nonwhite]),
    approved_nonwhite = sum(approved[is_nonwhite])
  ), by = census_tract],
  all = T
)
# get rid of "total" row
mort_out <- mort_out[!is.na(census_tract),]

# remove mort_df, as we don't need it and it's huge
rm(mort_df)

# aggregate up to ZCTA
mort_out <- ct_to_zcta(
  as.data.frame(mort_out), "census_tract", colnames(mort_out)[-1], 
  use_mean = F)

# create approval ratings
# population: reference rating
mort_out$approval_rate_pop <- 
  mort_out$approved_pop/mort_out$all_results_pop*100

# black: difference between population and black
mort_out$approval_rate_black <- 
  mort_out$approved_black/mort_out$all_results_black*100
mort_out$approval_difference_black <- 
  mort_out$approval_rate_black - mort_out$approval_rate_pop

# population: difference between population and nonwhite
mort_out$approval_rate_nonwhite <- 
  mort_out$approved_nonwhite/mort_out$all_results_nonwhite*100
mort_out$approval_difference_pop <- 
  mort_out$approval_rate_nonwhite - mort_out$approval_rate_pop

# subset to rows where there is some data
mort_out <- mort_out[!is.na(mort_out$all_results_pop),]

# write out result ----

data_folder <- file.path(
  gsub("\\\\","/", gsub("OneDrive - ","", Sys.getenv("OneDrive"))), 
  "Health and Social Equity - SJP - BHN Score Creation",
  "Data", "Preprocessed")

write.csv(mort_out,
          file = file.path(data_folder,
                           "HMDA_Mortgage_Approval_2021.csv"),
          row.names = F, na = "")
