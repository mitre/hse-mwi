# Crosswalk of 2020 Precinct Election Results to ZCTAs 
# By Caroline Mills 
# Originated on 12/01/2021 
# Progress update: 50 states and DC are completed. 

library(sf)
library(geojsonsf)
library(tidyverse)
library(dplyr)
library(tigris)
library(areal)
library(rgeos)
library(pals)
library(data.table)
library(s2)
library(rgdal)
library(readxl)
library(censable)
library(dataverse)
library(datasets)


############ Functions to import the data needed to crosswalk the precinct level to the ZCTAs level ############ 
# Function imports the 3 sources of 2020 precinct level presidential vote data 
import_precinct_vote_data <- function(state_abv, raw_data_folder){
  # imports data for the primary vote data source 
  if(toupper(state_abv) %in% list.dirs(path = paste0(raw_data_folder,"/UF 2020 election data"), full.names = FALSE)) {
    vote_sf <- st_read(paste0(raw_data_folder,"/UF 2020 election data/",toupper(state_abv),"/",state_abv,"_2020.shp")) %>%
      # uncomment for maryland 
      # st_zm() %>% 
      sf::st_as_sf() %>% 
      sf::st_transform(26915) %>%
      st_make_valid() %>%
      mutate(geo_id = as.character(row_number())) %>%
      rowwise() %>%
      mutate(presidential_votes = sum(c_across(starts_with("G20PRE")))) %>%
      select(geo_id,geometry,presidential_votes)
    if(all(st_is_valid(vote_sf))!= "TRUE") warning("The 2020 Election file for the state contains invalid geometries.")
    return(vote_sf)
  } else if (state_abv %in% c('ms', 'wy','sd')) {
    # imports data for the secondary primary vote data source at the precinct level 
    vote_sf <-geojsonsf::geojson_sf(paste0(raw_data_folder, "/NYT 2020 election data/precincts-with-results.geojson")) %>% 
      filter(substr(GEOID,1,2) %in% c('21','28','46','54')) %>%
      mutate(state = ifelse(substr(GEOID,1,2) %in% '21','ky',
                            ifelse(substr(GEOID,1,2) %in% '28','ms',
                                   ifelse(substr(GEOID,1,2) %in% '46','sd',
                                          ifelse(substr(GEOID,1,2) %in% '54','wv','na'))))) %>%
      mutate(presidential_votes = votes_total) %>%
      select(state,presidential_votes, geometry) %>%
      filter(state == state_abv) %>%
      sf::st_as_sf() %>% 
      sf::st_transform(26915) %>%
      st_make_valid() 
  } else if (state_abv %in% 'ky'){
    # import vote data on a FIPS county level since it's not available at the precinct level in the NYT data 
    vote_counts <-  read.csv(paste0(raw_data_folder,"/MIT 2020 election data/countypres_2000-2020-2.csv"))%>%
      filter(year == 2020,
             state_po == toupper(state_abv)) %>%
      select(county_fips,
             presidential_votes = totalvotes) %>%
      unique() 
    vote_counts$county_fips <- as.character(vote_counts$county_fips)
    # import fips shape file for geometries 
    fips_sh <- counties(state = state_abv, cb = FALSE, resolution = "500k", year = 2020) %>%
      mutate(county_fips = paste(STATEFP,COUNTYFP, sep=""))
    # join shape file to vote data 
    vote_sf <- vote_counts %>% left_join(fips_sh, by = "county_fips") %>%
      select(geo_id = county_fips,
             presidential_votes,
             geometry) %>% 
      sf::st_as_sf() %>% 
      sf::st_transform(26915) %>%
      st_make_valid()
  }
}

# Function imports the ZCTAs shape files for each state 
import_zctas <-function(state_abv){
  zctas_sf <- zctas(cb = FALSE, year = 2010, state = toupper(state_abv)) %>% 
    sf::st_as_sf()  %>% 
    sf::st_transform(26915)%>% 
    sf::st_make_valid() %>%
    rename(zcta = ZCTA5CE10 )%>%
    select(zcta,geometry) 
  if(all(st_is_valid(zctas_sf))!= "TRUE") warning("The ZCTA shape file for the state contains invalid geometries.")
  return(zctas_sf)
}

# Function imports the block level census data 
import_block_census <- function(state_abv){
  blocks_sf <- get_decennial(geography ="block",
                             variables = "P001001",
                             state = toupper(as.character(state_abv)),
                             year = 2010,
                             geometry = TRUE) %>%
    mutate(pop10 = value) %>%
    rename(block_id = GEOID) %>%
    select(block_id,pop10,geometry) %>%
    st_transform(26915) %>%
    st_make_valid()
  if(all(st_is_valid(blocks_sf))!= "TRUE") warning("The block level population file for the state contains invalid geometries")
  return(blocks_sf)
}
# Function imports the ZCTAs level census voting population data 
import_voting_population <- function(state_abv){
  over_18_pop <-get_acs(geography ="zcta",
                        variables = "DP05_0021",
                        state = toupper(state_abv),
                        year = 2019,
                        geometry = TRUE) %>%
    as.data.frame()%>%
    rename(pop19 = estimate,
           zcta = GEOID) %>%
    select(zcta,pop19)
}

############ Function crosswalks ############ 
# function generates a crosswalk from the precinct level results to the zcta level boundaries 
crosswalk_votes_to_zctas <- function(raw_data_folder,data_folder, state_abv){
  #### Step 1: Read in data  #### 
  # precinct vote level data
  vote_sf <- import_precinct_vote_data(state_abv, raw_data_folder)
  
  # zctas shape file 
  zctas_sf <- import_zctas(state_abv)
  
  # block level census population data 
  blocks_sf <- import_block_census(state_abv)
  
  # zcta level census population data 
  over_18_pop <- import_voting_population(state_abv)
  
  #### Step 2: Return all intersections between the precinct boundaries and the ZCTA boundaries  #### 
  # create intersections 
  zctas_overlapping_polys <- st_buffer(st_intersection(zctas_sf,vote_sf),dist=0) %>%
    mutate(precinct_zctas =  paste(geo_id, zcta, sep="_")) %>%
    select(precinct_zctas,geo_id, zcta,geometry)
  
  
  if(all(st_is_valid(zctas_overlapping_polys))!= "TRUE") warning("The zctas_sf contains invalid geometries")
  if(length(unique(zctas_overlapping_polys$zcta)) != length(unique(zctas_sf$zcta))) warning("Some ZCTAs were dropped when creating the intersecting polygons") 
  
  #### Step 3: Attribute populations to each of the intersections by areal interpolation using block level census data  #### 
  pop_of_intersection <- aw_interpolate(zctas_overlapping_polys,
                                        tid = precinct_zctas,
                                        source =  blocks_sf,
                                        sid = block_id,
                                        weight = "sum",
                                        output = "sf",
                                        extensive = "pop10") %>%
    mutate(pop10= round(pop10,0))  %>%
    sf::st_make_valid() %>%
    na.omit()
  if(sum(pop_of_intersection$pop10) != sum(blocks_sf$pop10)) warning(paste0("Not all the population has been attributed to the state. ",sum(pop_of_intersection$pop10), " is the sum of the population that has been attributed and ", sum(blocks_sf$pop10)," is the total population from the census file"))
  
  
  #### Step 4: Create a dataframe showing the percent population  #### 
  crosswalk <- pop_of_intersection %>%
    separate(precinct_zctas, c("precinct", "zcta"), sep = "_") %>%
    group_by(precinct, zcta) %>%
    summarise(pop10 = sum(pop10)) %>%
    group_by(precinct) %>%
    mutate(frac_of_precinct = {if(sum(pop10) > 0) pop10/sum(pop10) else 1/n()}) %>%
    group_by(zcta) %>%
    mutate(frac_of_zctas = {if(sum(pop10) > 0) pop10/sum(pop10) else 1/n()}) %>%
    group_by()
  
  #### Step 5: Apportion the votes by population weighting for each ZCTA  #### 
  general_zctas_crosswalked <- as.data.frame(vote_sf) %>%
    left_join(crosswalk, by = c("geo_id"= "precinct")) %>%
    group_by(zcta) %>%
    summarise(allocated_presidential_votes = round(sum(presidential_votes * frac_of_precinct)),0) %>%
    as.data.frame() %>%
    mutate(zcta = str_sub(zcta, start = -5))
  
  
  #### Step 6: Add the denominator - 18+ population to the final file  #### 
  final_df <- left_join(general_zctas_crosswalked,over_18_pop, by = "zcta") %>%
    as.data.frame() %>%
    mutate(voter_percent_of_population = ifelse((allocated_presidential_votes/pop19)>1,1,allocated_presidential_votes/pop19)) %>%
    select(zcta,pop19,allocated_presidential_votes,voter_percent_of_population) %>%
    rename(adult_population = pop19,
           presidential_votes = allocated_presidential_votes)
  
  if(length(unique(general_zctas_crosswalked$zctas)) != length(unique(general_zctas_crosswalked$zctas_id))) warning(paste0("Some ZCTAs have been dropped from the final data frame. ",length(unique(zctas_sf$zctas)), " is the number of unique zctas in the original shpae files and ", length(unique(final_df$zctas))," is the number of unique zctas in the final dataframe."))
  
  # write each state file out to the prepossessed folder to save a copy 
  write.csv(final_df, 
            file = file.path(
              paste0(
              data_folder,
              "/",state_abv,"_voter_participation.csv"
            ), 
            row.names = F, 
            na = ""))
  
  return(final_df) 
}

############  Run crosswalk function on all states ############ 
raw_data_folder <- file.path(
  gsub("\\\\","/", gsub("OneDrive - ","", Sys.getenv("OneDrive"))), 
  "Health and Social Equity - SJP - BHN Score Creation",
  "Data", "Raw", "Voter_Participation")

data_folder <- file.path(
  gsub("\\\\","/", gsub("OneDrive - ","", Sys.getenv("OneDrive"))), 
  "Health and Social Equity - SJP - BHN Score Creation",
  "Data", "Preprocessed")

datalist = list()

state_abv <-tolower(state.abb)

for (i in state_abv){
  state_file <- crosswalk_votes_to_zctas(state_abv,raw_data_folder,data_folder)
}

vote_data = do.call(rbind, datalist)

# export to preprocessed folder with date generated in file name 
write.csv(vote_data, 
          file = file.path(
            paste0(
              data_folder,
              "/",Sys.Date(),"_voter_participation_.csv"
            ), 
            row.names = F, 
            na = ""))
