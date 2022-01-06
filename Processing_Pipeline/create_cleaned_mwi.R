# Pipeline Prep for MWI App
# By Hannah De los Santos
# Originated on: 1/4/2022

# Run this script to process data for default MWI tool. Make sure to delete any
# remaining RData files in the Cleaned folder

# load pipeline file ----

source(file.path("Processing_Pipeline", "pipeline_driver.R"))

# run pipeline ----

# load measure registry -- first sheet
m_reg <- as.data.frame(
  read_excel(file.path(data_folder, "Metadata.xlsx"), sheet = 1)
)
# remove everything that doesn't have a numerator
m_reg <- m_reg[!is.na(m_reg$Numerator),]

# run the pipeline!
mwi_pipeline(m_reg_custom = m_reg)

# make sure to delete any remaining RData files in the Cleaned folder
fn <- file.path(data_folder, "Cleaned", "HSE_MWI_ZCTA_full_shapefile_US.RData")
# Check its existence
if (file.exists(fn)) {
  # Delete file if it exists
  file.remove(fn)
}
