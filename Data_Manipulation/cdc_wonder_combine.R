# Combining CDC WONDER Data
# By Hannah De los Santos
# Originated on: 6/3/21

# load data and combine ----

data_folder <- file.path(
  gsub("\\\\","/", gsub("OneDrive - ","", Sys.getenv("OneDrive"))), 
  "Health and Social Equity - SJP - BHN Score Creation",
  "Data", "Raw", "CDC_WONDER")

# choosing only 10 year counts
selected_fn <- list.files(data_folder)
selected_fn <- selected_fn[grepl("2011_2020", selected_fn)]

# map file names to what their output names will be
fn_colname_map <- c(
  "cdc_wonder_alcohol_all_2011_2020_allpop.txt" = "alcoholmortality_pop",   
  "cdc_wonder_alcohol_all_2011_2020_race.txt" = "alcoholmortality_black",
  "cdc_wonder_drug_poisonings_all_2011_2020_allpop.txt" = "drugpoisoningmortality_pop",
  "cdc_wonder_drug_poisonings_all_2011_2020_race.txt"  = "drugpoisoningmortality_black",
  "cdc_wonder_suicide_all_2011_2020_allpop.txt"  = "suicidemortality_pop",      
  "cdc_wonder_suicide_all_2011_2020_race.txt"  = "suicidemortality_black"
)

fn_colname_denom_map <- c(
  "cdc_wonder_alcohol_all_2011_2020_allpop.txt" = "alcoholmortality_denom_pop",   
  "cdc_wonder_alcohol_all_2011_2020_race.txt" = "alcoholmortality_denom_black",
  "cdc_wonder_drug_poisonings_all_2011_2020_allpop.txt" = "drugpoisoningmortality_denom_pop",
  "cdc_wonder_drug_poisonings_all_2011_2020_race.txt"  = "drugpoisoningmortality_denom_black",
  "cdc_wonder_suicide_all_2011_2020_allpop.txt"  = "suicidemortality_denom_pop",      
  "cdc_wonder_suicide_all_2011_2020_race.txt"  = "suicidemortality_denom_black"
)

# adding county and county code to start
full_df <- read.delim(file.path(data_folder, selected_fn[1]),
                 colClasses = c("County.Code" = "character"))
full_df <- full_df[, c("County", "County.Code")]
# remove all the empty rows
full_df <- full_df[full_df$County != "",]
rownames(full_df) <- full_df$County.Code

for (fn  in selected_fn){
  df <- read.delim(file.path(data_folder, fn),
                   colClasses = c("County.Code" = "character"))
  # remove all the empty rows -- notes and totals
  df <- df[df$County != "",]
  # replace "suppressed" and convert to numeric
  df[df == "Suppressed"] <- 4.5
  df[df == "Missing"] <- NA
  df$Deaths <- as.numeric(df$Deaths)
  df$Population <- as.numeric(df$Population)
  
  full_df[df$County.Code, fn_colname_map[fn]] <- df$Deaths
  full_df[df$County.Code, fn_colname_denom_map[fn]] <- df$Population
}

# write out ----

data_folder <- file.path(
  gsub("\\\\","/", gsub("OneDrive - ","", Sys.getenv("OneDrive"))), 
  "Health and Social Equity - SJP - BHN Score Creation",
  "Data", "Preprocessed")

write.csv(full_df, 
          file = file.path(
            data_folder,
            "CDC_WONDER_County_mortality.csv"
          ), 
          row.names = F, 
          na = "")
