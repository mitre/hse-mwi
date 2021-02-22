# HSE Behavioral Health Needs Score

This processes and cleans data to create the Behavioral Health Needs Score.

data is in BSE Score > Data > 
  Raw = raw data
  Cleaned = cleaned measures

# finalize measures

measures to add
START WITH:
- unemployment (ACS)
- no high school diploma (ACS)
- associate's degree or higher (ACS)
- below 200% FPL (ACS)

# pulling data 

either: download from site OR api

# registration (classifying metadata)

defini all
- define the orginal geo level
- define the directionality of score (high v low)
- range of data (perc, prevalence, etc.) - values what they take
- "Source", - data source
"Measure", - name of measure
"Column", - which column in measure files
"Geography", - geo level
"Orientation", - firectionality of score +/- 1
"Scale", - counts v percent
"Minimum", - min num
"Maximum", - max num
"Filename", - file name
"Formula", - if data needs to be processed (i.e. ICE), manipulation w/in file
"Created", - date created?
"Notes", - notes
"Values", - number of data states
"Selection",
"Race Stratification" - y/n
"Which Races" - which races specified by
"Ethnicity Stratification" - y/n
"Which Ethnicity" - which races specified by
"include" t/f

IN FUTURE: VALIDATE

# processing

go through all data files in registry and process according formula
NOTE: what to do with NAs
NOTE: suppress score for < 50 people -- zctas you don't want to consider

write script:
- county -> zcta
- zip code -> zcta
- census tract -> zcta

(parens do here)
-> row is a zcta, column is ALL measures (percentile scaled and oriented correctly -- high score = higher need)

# create weights

create weight file (we'll divide evenly and change later)
NOTE: naming conventions
note: documentation - measure definitions and provenance
NOTES: what to do with NAs -- vary denominator

# score creation

weights and processed file -> multiply, sum, rescale from 0 to 100