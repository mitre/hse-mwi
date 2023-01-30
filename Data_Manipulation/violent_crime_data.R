# Preprocessing Violent Crime Data (Community Violence)
# By Emily Pantalone
# Originated on: 6/14/2021

# Load data, read csv, call packages

data_folder <- file.path(
  gsub("\\\\","/", gsub("OneDrive - ","", Sys.getenv("OneDrive"))), 
  "Health and Social Equity - SJP - BHN Score Creation",
  "Data", "Raw")

vc_data<- read.csv(file.path(data_folder,"analytic_data2022.csv"))


#delete first two rows (they are aggregate, not county-specific)
vc_data <- vc_data[-c(1,2),]

#delete unneeded columns
vc_data <- vc_data[c(1,2,3,4,5,233,234,235)]


#rename columns
colnames(vc_data)<- c("stateFIPS","countyFIPS","fullFIPS","state","county",
                      "violentcrime_incidentsper100000_pop","violentcrime_incidents_pop", 
                      "violentcrime_totalpopulation_pop")

#remove state aggregate rows
vc_data <- vc_data[vc_data$countyFIPS != '000', ] 

#change violent crime values to numbers (currently is a character)
vc_data$violentcrime_incidents_pop <- as.numeric(vc_data$violentcrime_incidents_pop)
vc_data$violentcrime_totalpopulation_pop <- as.numeric(vc_data$violentcrime_totalpopulation_pop)
vc_data$violentcrime_incidentsper100000_pop <- as.numeric(vc_data$violentcrime_incidentsper100000_pop)

#create column with raw ratio of violent crimes per population (num/denom), 
vc_data$violentcrime_incidenttopopulationratio_pop <- vc_data$violentcrime_incidents_pop/vc_data$violentcrime_totalpopulation_pop


# write out ----

data_folder <- file.path(
  gsub("\\\\","/", gsub("OneDrive - ","", Sys.getenv("OneDrive"))), 
  "Health and Social Equity - SJP - BHN Score Creation",
  "Data", "Preprocessed")

write.csv(vc_data, 
          file = file.path(
            data_folder,
            "CHR_County_violent_crime.csv"
          ), 
          row.names = F, 
          na = "")






