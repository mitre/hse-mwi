# Preprocessing Mortgage Security Data
# By Hannah De los Santos
# Originated on: 8/13/21

# load data and libraries ----

# may also need to install bit64
library(data.table)

data_folder <- file.path(
  gsub("\\\\","/", gsub("OneDrive - ","", Sys.getenv("OneDrive"))), 
  "Health and Social Equity - SJP - BHN Score Creation",
  "Data", "Raw")

# there's an API, but it's probably easier to download and use?
# load mortgage data
# using alaska for ease of processing, will use national data in the end
mort_df <- fread(file.path(
  data_folder,
  "HMDA",
  "hmda_2020_state_AK.csv"
),
colClasses = c("census_tract" = "character"))

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

# some questions: for black, do we want black alone? the only race? 
# do we not want hispanic/latino?

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
mort_df[, denied := action_taken %in% c(3,7)]
mort_df[, all_results := T]
mort_df[, is_black := 
          `applicant_race-1` == 3 |
          `applicant_race-2` == 3 |
          `applicant_race-3` == 3 |
          `applicant_race-4` == 3 |
          `applicant_race-5` == 3 
        ]
mort_df[is.na(is_black), is_black := F]

# aggregate all outcome measures
mort_pop <-
  aggregate(cbind(all_results, approved, denied) ~ census_tract, 
            mort_df, FUN = sum)
mort_pop$approval_rate <- mort_pop$approved/mort_pop$all_results
colnames(mort_pop)[-1] <- paste0(colnames(mort_pop)[-1], "_pop")
# aggregate for black applicants
mort_black <- 
  aggregate(cbind(all_results, approved, denied) ~ census_tract, 
          mort_df, FUN = sum, subset = mort_df$is_black)
mort_black$approval_rate <- mort_black$approved/mort_black$all_results
colnames(mort_black)[-1] <- paste0(colnames(mort_black)[-1], "_black")

# merge outcomes together
mort_out <- merge(mort_pop, mort_black, by = "census_tract", all = T)

# NOTE: we might just want to aggregate this up to the zip code before we 
# propose these percentages (since they'll be averaged, not summed)
