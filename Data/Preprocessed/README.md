# Preprocessed README

Note: all information for mappings from measure columns and files to measure names can be found in Data/Metadata.xlsx. See Data README.md for more information. All data in the Preprocessed folder is organized by data source. See Documentation folder for information on each measure.

Folders in "Preprocessed" are as follows:
- 2021_release: contains older versions of resource files used in prior 2021 MWI release. Files without data updates remain in main folder.

Files:
- ACS_API_Data.csv: ACS measure data (such as broadband access, unemployment), by ZCTA. Uses API_Data_Pulls/acs_data_pull.R to generate.
- CBP_County_AlcoholOutlet.csv: County alcohol outlet census data from County Business Practices. Uses Data_Manipulation/cbp_alcoholoutlet.R to generate.
- CDC_WONDER_County_mortality.csv: CDC WONDER mortality data, by county. Uses Data_Manipulation/cdc_wonder_combine.R to generate.
- CHR_County_violent_crime.csv: County Health Rankings violent crime data, by county. Uses Data_Manipulation/violent_crime_data.R to generate.
- CHR_USALEEP_Life_Expectancy.csv: Life expectancy from USALEEP project and extrapolation for race from County Health Rankings, by census tract. Uses Data_Manipulation/chr_usaleep_life_expectancy.R to generate.
- half_mile_park_pop.csv: Percent of population living within a half mile of a park, from CDC EPHTN, by census tract. Uses API_Data_Pulls/Half_Mile_Park_Population.R to generate.
- HMDA_Mortgage_Approval_2021.csv: Mortgage approval rate data, from HMDA, FDIC, NCUA, County Business Patterns, by ZCTA. Uses Data_Manipulation/HMDA_Mortgage_Approval_2021.csv to generate.
- HUD_CT_housingstress.csv: HUD housing stress data, by census tract. Uses Data_Manipulation/hud_combine.R to generate.
- MPV_ZCTA_policeviolence.csv: Police killings data from Mapping Police Violence. Uses Data_Manipulation/mpv_combine.R to generate.
- NAICS_Third_Spaces.csv: Third places data, from NAICS codes and County Business Patterns, by county. API_Data_Pulls/naics_third_spaces.R to generate.
- New_America_Financial_Accessibility.csv: Financial accessibility data, using the New America Methodology, by county. Uses Data_Manipulation/financial_access.R to generate.
- NSDUH_CT_suicideideation.csv: Suicide idea data from NSDUH, by census tract. Uses Data_Manipulation/ideation_at_CT.R to generate.
- PLACES__ZCTA_Data__GIS_Friendly_Format___2022_release.csv: CDC PLACES health data, by ZCTA. Directly downloaded.
- SAMHSA_CT_treatmentfacilityaccess.csv: SAMHSA treatment facility access data, by census tract. Uses Data_Manipulation/treatment_facility_weights.R and Data_Manipulation/treatment_facility.R to generate.
- voter_participation.csv: Voter participation data from NYT and US Election Project. Uses Data_Manipulation/voter_participation_crosswalk.R to generate.