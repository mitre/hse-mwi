# Crosswalk of 2020 Precinct Election Results to ZCTAs 
# By Caroline Mills 
# Originated on 12/01/2021 
# Progress update: 45 states are completed. 

# TODO: Incorporate the geojson files for the 4 states not included in the primary data folder (KY,MS,SD,WV )
# TODO: Fix AR and IN issues (returning invalid geometries) and then apply the function to all states 

library(sf)
library(geojsonsf)
library(tidyverse)
library(dplyr)
library(tigris)
library(areal)
library(tidycensus)
library(rgeos)
library(pals)
library(data.table)
library(s2)
library(rgdal)
library(tidycensus)

# function generates a crosswalk from the precinct level results to the zcta level boundaries 
generate_crosswalk <- function(input_data_pathway,output_data_pathway, state_abr, api_key = "edf70320cb9ed5f442d446ed1d6e7d2d9f4ec6bb"){
  ############# Step 1: Read in data ############# 
  # precinct vote level data
  vote_sf <- st_read(paste0(input_data_pathway,"/",toupper(state_abr),"/",state_abr,"_2020.shp")) %>%
    # uncomment for maryland 
    # st_zm() %>% 
    sf::st_as_sf() %>% 
    sf::st_transform(26915) %>%
    st_make_valid() %>%
    mutate(precinct_id = as.character(row_number())) %>%
    rowwise() %>%
    mutate(presidential_votes = sum(c_across(starts_with("G20PRE")))) %>% 
    select(precinct_id,geometry,presidential_votes) 
  
  if(all(st_is_valid(vote_sf))!= "TRUE") warning("The 2020 Election file for the state contains invalid geometries.")
  
  # zctas shape file 
  census_api_key(api_key)
  zctas_sf <- zctas(cb = FALSE, year = 2010, state = toupper(state_abr)) %>% 
    sf::st_as_sf()  %>% 
    sf::st_transform(26915)%>% 
    sf::st_make_valid() %>%
    rename(zcta = ZCTA5CE10 )%>%
    select(zcta,geometry) 
  
  if(all(st_is_valid(zctas_sf))!= "TRUE") warning("The ZCTA shape file for the state contains invalid geometries.")
  
  # block level census population data 
  blocks_sf <- get_decennial(geography ="block",
                             variables = "P001001",
                             state = toupper(as.character(state_abr)),
                             year = 2010,
                             geometry = TRUE) %>%
    mutate(pop10 = value) %>%
    rename(block_id = GEOID) %>%
    select(block_id,pop10,geometry) %>%
    st_transform(26915) %>%
    st_make_valid()
  
  if(all(st_is_valid(blocks_sf))!= "TRUE") warning("The block level population file for the state contains invalid geometries")
  
  # zcta level census population data 
  over_18_pop <-get_acs(geography ="zcta",
                        variables = "DP05_0021",
                        state = toupper(state_abr),
                        year = 2019,
                        geometry = TRUE) %>%
    as.data.frame()%>%
    rename(pop19 = estimate,
           zcta = GEOID) %>%
    select(zcta,pop19)
  
  ############# Step 2: Return all intersections between the precinct boundaries and the ZCTA boundaries  ############# 
  # create intersections 
  zctas_overlapping_polys <- st_intersection(zctas_sf,vote_sf) %>%
    mutate(precinct_zctas =  paste(precinct_id, zcta, sep="_")) %>%
    select(precinct_zctas,precinct_id, zcta,geometry)
  
  if(all(st_is_valid(zctas_overlapping_polys))!= "TRUE") warning("The zctas_sf contains invalid geometries")
  #  if(length(unique(zctas_overlapping_polys$zcta)) != length(unique(zctas_sf$ZCTA5CE10))) warning("Some ZCTAs were dropped when creating the intersecting polygons") 
  
  ############# Step 3: Attribute populations to each of the intersections by areal interpolation using block level census data #############   
  pop_of_intersection <- aw_interpolate(st_buffer(zctas_overlapping_polys,dist = 0),
                                        tid = precinct_zctas,
                                        source =  blocks_sf,
                                        sid = block_id,
                                        weight = "total",
                                        output = "sf",
                                        extensive = "pop10") %>%
    mutate(pop10= round(pop10,0))  %>%
    sf::st_make_valid() %>%
    na.omit()
  if(sum(pop_of_intersection$pop10) != sum(blocks_sf$pop10)) warning(paste0("Not all the population has been attributed to the state.",sum(pop_of_intersection$pop10), " is the sum of the population that has been attributed and ", sum(blocks_sf$pop10)," is the total population from the census file"))
  
  
  ############# Step 4: Create a dataframe showing the percent population #############   
  crosswalk <- pop_of_intersection %>%
    separate(precinct_zctas, c("precinct", "zcta"), sep = "_") %>%
    group_by(precinct, zcta) %>%
    summarise(pop10 = sum(pop10)) %>%
    group_by(precinct) %>%
    mutate(frac_of_precinct = {if(sum(pop10) > 0) pop10/sum(pop10) else 1/n()}) %>%
    group_by(zcta) %>%
    mutate(frac_of_zctas = {if(sum(pop10) > 0) pop10/sum(pop10) else 1/n()}) %>%
    group_by()
  
  ############# Step 5: Apportion the votes by population weighting for each ZCTA #############   
  general_zctas_crosswalked <- as.data.frame(vote_sf) %>%
    left_join(crosswalk, by = c("precinct_id"= "precinct")) %>%
    group_by(zcta) %>%
    summarise(allocated_presidential_votes = round(sum(presidential_votes * frac_of_precinct)),0) %>%
    as.data.frame() %>%
    mutate(zcta = str_sub(zcta, start = -5))
  
  
  ############# Step 6: Add the denominator - 18+ population to the final file  #############   
  final_df <- left_join(general_zctas_crosswalked,over_18_pop, by = "zcta") %>%
    as.data.frame() %>%
    mutate(voter_percent_of_population = ifelse((allocated_presidential_votes/pop19)>1,1,allocated_presidential_votes/pop19)) %>%
    select(zcta,pop19,allocated_presidential_votes,voter_percent_of_population) %>%
    rename(adult_population = pop19,
           presidential_votes = allocated_presidential_votes)
  
  
  if(length(unique(general_zctas_crosswalked$zctas)) != length(unique(general_zctas_crosswalked$zctas_id))) warning(paste0("Some ZCTAs have been dropped from the final data frame. ",length(unique(zctas_sf$zctas)), " is the number of unique zctas in the original shpae files and ", length(unique(final_df$zctas))," is the number of unique zctas in the final dataframe."))
  
  
  write.csv(final_df,paste0(output_data_pathway,state_abr,"_votes.csv"), row.names = FALSE)
  
  return(final_df) 
}

# test function on Rhode Island 
vote_data_folder <- file.path(
  gsub("\\\\","/", gsub("OneDrive - ","", Sys.getenv("OneDrive"))), 
  "Health and Social Equity - SJP - BHN Score Creation",
  "Data", "Raw", "Voter_Participation", "UF 2020 election data")

output_data_folder <- "C:/Users/CMILLS/OneDrive - The MITRE Corporation/SJP Shiny App Project/Redo Cleaned Data/"

api_key = "edf70320cb9ed5f442d446ed1d6e7d2d9f4ec6bb"

ri_votes <-  generate_crosswalk(state_abr = "ri", input_data_pathway = vote_data_folder ,output_data_pathway = output_data_folder)
