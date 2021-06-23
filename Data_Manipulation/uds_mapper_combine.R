# Combining UDS Mapper Data
# By Hannah De los Santos
# Originated on: 6/3/21

# load data and combine ----

data_folder <- file.path(
  gsub("\\\\","/", gsub("OneDrive - ","", Sys.getenv("OneDrive"))), 
  "Health and Social Equity - SJP - BHN Score Creation",
  "Data", "Raw", "UDS_Mapper")

full_df <- data.frame()
for (fn  in list.files(data_folder)){
  df <- read.csv(file.path(data_folder, fn))
  # remove the summary row 
  df <- df[df$ZCTA != "Summary:",]
  # rename last column
  colnames(df)[ncol(df)] <- "mortality_allcausemortalityrate_pop"
  
  full_df <- rbind(full_df, df)
}

# write out ----

data_folder <- file.path(
  gsub("\\\\","/", gsub("OneDrive - ","", Sys.getenv("OneDrive"))), 
  "Health and Social Equity - SJP - BHN Score Creation",
  "Data", "Preprocessed")

write.csv(full_df, 
          file = file.path(
            data_folder,
            "UDSMapper_ZCTA_all_cause_mortality.csv"
          ), 
          row.names = F, 
          na = "")
