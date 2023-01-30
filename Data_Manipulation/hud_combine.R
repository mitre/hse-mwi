# Combining HUD data
# By Karen Jiang
# Originated on: 6/11/21

library(readxl)
library(dplyr)
library(tidyr)

# load data and combine ----
data_folder <- file.path(
  gsub("\\\\","/", gsub("OneDrive - ","", Sys.getenv("OneDrive"))), 
  "Health and Social Equity - SJP - BHN Score Creation",
  "Data", "Raw", "HUD")


# Read in Table
df <- read.csv(file.path(data_folder, "Table1.csv"))

# Read in Data Dictionary
dictionary <- readxl::read_xlsx(file.path(data_folder, 
                                          "CHAS data dictionary 15-19.xlsx"),
                                sheet = "Table 1")


# Using Data dictionary to identify the column numbers for numerator and denom
black_num <- dictionary %>%
  filter(`Race/ethnicity` == "Black or African-American alone, non-Hispanic",
         `1 of 4 housing problems` == "has 1 or more of the 4 housing unit problems (lacks kitchen or plumbing, more than 1 person per room, or cost burden greater than 30%)")
black_denom <- dictionary %>%
  filter(`Race/ethnicity` == "Black or African-American alone, non-Hispanic")

black_num_col_nums <- unlist(which(colnames(df) %in% black_num$`Column Name`))
black_denom_col_nums <- unlist(which(colnames(df) %in% black_denom$`Column Name`))



housing_stress <- df %>% 
  # Converting existing geoid into 11-digit ID
  separate(geoid, into = c("country code", "geoid"), sep = "US") %>%
         # Calculate housing stress for general pop
         # Defined as all pop with housing stress / all renters & owners
  mutate(housing_stress_pop = (T1_est3 + T1_est127)/(T1_est2 + T1_est126) * 100,
         
         # Calculate housing stress for black pop
         # Defined as black pop with housing stress / black renters & owners
         black_num = select(., all_of(black_num_col_nums)) %>% rowSums(na.rm = T),
         black_denom = select(., all_of(black_denom_col_nums)) %>% rowSums(na.rm = T),
         housing_stress_black = black_num/black_denom * 100) %>%
  
  select(geoid, housing_stress_pop, housing_stress_black)

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
