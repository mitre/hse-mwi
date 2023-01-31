# Cleaned README

Note: all information for mappings from measure columns and files to measure names can be found in Data/Metadata.xlsx. See Data README.md for more information. See Documentation folder for information on each measure.

Folders in "Cleaned" are as follows:
- 2021_release: contains cleaned files used in 2021_release, if they have been updated. Not updated files remain in main folder.

Files:
- HSE_MWI_Data_Information.csv: Information about each measure (a modified version of the Metadata.xlsx file), also stratified by race. Includes information about missingness, total rows in data, the penalty placed for each measure, and weights for each measure aftr including the penalty.
- HSE_MWI_ZCTA_Converted_Measures.csv: All measure values, aligned to ZCTA ("GEOID" in file).
- HSE_MWI_ZCTA_Mental_Wellness_Index_Black.csv: Mental Wellness Index for Black Populations for each ZCTA, and corresponding measure rankings.
- HSE_MWI_ZCTA_Mental_Wellness_Index_Population.csv: Mental Wellness Index for Overall Populations for each ZCTA, and corresponding measure rankings.
- HSE_MWI_ZCTA_No_Directionality_Percentile_Ranked_Measures.csv: Measures ranked by each ZCTA, and not aligned to MWI requirements (where are all measures are aligned based on whether the measure is an obstacle or an asset to mental wellness). Rankings for each measure here are such that a higher measure value gives a higher ranking.
- HSE_MWI_ZCTA_Percentile_Ranked_Measures.csv: Measures ranked by each ZCTA, and aligned to MWI requirements (where are all measures are aligned based on whether the measure is an obstacle or an asset to mental wellness). Obstacles have the opposite orientation in the MWI, such that higher rankings indicate less obstacles.