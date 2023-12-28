# Mental Wellness Index Tool
# By HSE Team
# Originated on: 10/20/2021

# NOTE: Styling by Sarah Ober of Case Study for using health equity framework
# in population health team.

# load libraries ----

library(readxl)
library(writexl)
library(htmltools)
library(shiny)
library(tigris)
library(leaflet)
library(RColorBrewer)
library(sf)
library(plotly)
library(ggbeeswarm)
library(shinyWidgets)
library(sass)
library(shinycssloaders)
library(shinyBS)
library(DT)
# library(formattable)
library(dplyr)

options(shiny.maxRequestSize=300*1024^2)

source("app_config.R")

# styling/resources ----

# Applies css to rShiny app
sass(
  sass_file("www/stylesheets/app.scss"),
  output = "www/stylesheets/app.css"
)

# Generates css used on the about page
# See app.scss file for manual steps needed here
sass(
  sass_file("www/stylesheets/app.scss"),
  output = "about/app.css"
)

addResourcePath("mwi-toolkit", "mwi-toolkit")

# order for tabs
mwi_toolkit_order <- c(
  "MWI_Overview",
  "MWI_Populations_of_Focus",
  "MWI_Framework",
  "MWI_Measures_and_Data",
  "MWI_Tool_Videos_and_Guides",
  "Share_the_MWI_With_Others",
  "The_Science_Behind_the_MWI",
  "MWI_in_Action",
  "Frequently_Asked_Questions",
  "Contact"
)

# function for app preprocessing ----

app_preprocess <- function(m_reg, info_df, mwi, app_start = T){
  
  # create measure name to overall category
  meas_col_to_type <- setNames(m_reg$Category, m_reg$Measure)
  meas_col_to_type["Mental Wellness Index"] <- "Mental Wellness Index"
  
  # create measure names
  avail_measures <- measure_to_names <- avail_meas_w_weights <- 
    list()
  # group into list for display
  avail_meas_list <- m_to_type <- list()
  for (idx in index_types){
    avail_measures[[idx]] <- colnames(mwi[[idx]])[-1]
    names(avail_measures[[idx]]) <- 
      c("Mental Wellness Index", 
        m_reg[
          gsub("*_pop$","",
               gsub("*_black$","",colnames(mwi[[idx]])[-c(1:2)])), "Measure"
        ]
      )
    
    measure_to_names[[idx]] <-
      setNames(names(avail_measures[[idx]]), avail_measures[[idx]])
    
    # group into list for display
    avail_meas_list[[idx]] <- list()
    
    # measure column to type
    m_to_type[[idx]] <- 
      meas_col_to_type[measure_to_names[[idx]][avail_measures[[idx]]]]
    
    # add the weights to the name
    avail_meas_w_weights[[idx]] <- avail_measures[[idx]]
    # rownames(measure_to_type) <- measure_to_type$Name
    names(avail_meas_w_weights[[idx]]) <- 
      paste0(names(avail_measures[[idx]]),
             " (Weight: ", 
             round(info_df[avail_measures[[idx]], "Effective_Weights"], 2),
             ")")
    # unmet need score doesn't have a weight
    names(avail_meas_w_weights[[idx]])[1] <- names(avail_measures[[idx]])[1]
    # add them to the list
    for (t in unique(m_to_type[[idx]])){
      avail_meas_list[[idx]][[t]] <- 
        avail_meas_w_weights[[idx]][m_to_type[[idx]] == t]
    }
  }
  
  if ((app_start &
       !"HSE_MWI_ZCTA_full_shapefile_US.RData" %in% 
       list.files(file.path(data_folder, "Cleaned")))){
    
    # add counties/states to mwi
    for (idx in index_types){
      mwi[[idx]][, colnames(cty_cw)[-1]] <- 
        cty_cw[mwi[[idx]]$ZCTA, -1]
    }
    
    # get zip code data
    # NOTE: cb = T will download a generalized file
    zips <- zctas(cb = T, year = 2020)
    colnames(zips)[colnames(zips) == "GEOID20"] <- "GEOID"
    zips <- zips[zips$GEOID %in% mwi$pop$ZCTA,]
    zips <- st_transform(zips, crs = "+proj=longlat +datum=WGS84")
    
    # create the geo data for leaflet
    # NOTE: may want to do this ahead of time, if possible, when the base index is done
    geodat <- geopts <- list()
    for (idx in index_types){
      geodat[[idx]] <-
        left_join(zips, mwi[[idx]], by = c("GEOID" = "ZCTA"))
      
      # sort by state code and zcta
      geodat[[idx]] <- geodat[[idx]][order(geodat[[idx]]$STATE,
                                           geodat[[idx]]$GEOID),]
      
      # convert to points for US visualization -- ignore warnings
      geopts[[idx]] <- st_centroid(geodat[[idx]])
    }
    
    if (app_start &
         !"HSE_MWI_ZCTA_full_shapefile_US.RData" %in% 
         list.files(file.path(data_folder, "Cleaned"))){
      save(list = c("geodat", "geopts"), 
           file = file.path(data_folder, 
                            "Cleaned", "HSE_MWI_ZCTA_full_shapefile_US.RData"))
    }
  } else if ("HSE_MWI_ZCTA_full_shapefile_US.RData" %in% 
              list.files(file.path(data_folder, "Cleaned"))){
    # this will exist for the pipeline as well -- can only be done if started
    
    # load geodat data (should be much faster)
    load(file.path(data_folder, "Cleaned", "HSE_MWI_ZCTA_full_shapefile_US.RData"))
  }
  
  # get available zctas -- both will have the same
  avail_zctas <- geodat[["pop"]]$GEOID
  names(avail_zctas) <- paste0(geodat[["pop"]]$GEOID, 
                               " (State: ", geodat[["pop"]]$STATE_NAME, ")")
  
  # if we're running a custom pipeline, we're not going to save the geo data, 
  # it's very big and it's already in the app
  if (app_start){
    return(list(
      meas_col_to_type = meas_col_to_type,
      avail_measures = avail_measures,
      measure_to_names = measure_to_names,
      avail_meas_list = avail_meas_list,
      geodat = geodat,
      geopts = geopts
    ))
  } else {
    return(list(
      meas_col_to_type = meas_col_to_type,
      avail_measures = avail_measures,
      measure_to_names = measure_to_names,
      avail_meas_list = avail_meas_list
    ))
  }
  
}

# load data ----

# what indices are available?
index_types <- c("Population" = "pop",
                 "Black" = "black")

# folder where all the data and information for the pipeline is
data_folder <- file.path("Data")

# load measure registry -- first sheet
m_reg <- as.data.frame(
  read_excel(file.path(data_folder, "Metadata.xlsx"), sheet = 1)
)
# remove everything that doesn't have a numerator
m_reg <- m_reg[!is.na(m_reg$Numerator),]
rownames(m_reg) <- m_reg$Numerator
# create a subset for the "create your own MWI" part
sub_m <- m_reg[, c("Measure", "Category", "Weights", "Weights")]
colnames(sub_m)[ncol(sub_m)-1] <- "Original Weights"
colnames(sub_m)[ncol(sub_m)] <- "Updated Weights"
rownames(sub_m) <- rownames(m_reg)

# load index weighting/output
info_df <- read.csv(
  file.path(data_folder, "Cleaned", "HSE_MWI_Data_Information.csv"),
)
rownames(info_df) <- info_df$Numerator

# load mapped measure data excat values
meas_df <- read.csv(
  file.path(data_folder, "Cleaned", "HSE_MWI_ZCTA_Converted_Measures.csv"),
  colClasses = c("GEOID" = "character")
)
meas_df <- meas_df[meas_df$GEOID != "",]
rownames(meas_df) <- meas_df$GEOID

# load zip codes to zcta
zip_cw <- read.csv(
  file.path(data_folder, "Resources", "Zip_to_zcta_crosswalk_2021.csv"),
  colClasses = c(
    "ZIP_CODE" = "character",
    "ZCTA" = "character"
  ))
territories <- c("AS", "FM", "GU", "MH", "MP", "PW", "PR", "VI")
zip_cw <- zip_cw[!zip_cw$STATE %in% territories,]
# we need to align these ZIP codes to ZCTAs in 2020 (in future)
# NOTE: in future, it may be a good idea to get the map between zip and ZCTA2020
# proper -- the code below is an attempt to do that, but did not work. since
# most did not change wily, the legacy version should work mostly fine.
# zcta_rel <- read.csv(file.path(data_folder, "Resources",
#                                "tab20_zcta510_zcta520_natl.txt"),
#                      colClasses = c(
#                        "GEOID_ZCTA5_20" = "character",
#                        "GEOID_ZCTA5_10" = "character"
#                      ),
#                      sep = "|")
# # get rid of zctas with no relationship between each other
# zcta_rel <- zcta_rel[zcta_rel$GEOID_ZCTA5_10 != "" & 
#                        zcta_rel$GEOID_ZCTA5_20 != "",]
# zcta_rel <- zcta_rel[order(zcta_rel$GEOID_ZCTA5_10),]
# zcta_rel$perc_land <- zcta_rel$AREALAND_PART/zcta_rel$AREALAND_ZCTA5_20
# # chose zcta to zip code with the largest area in 2020
# zcta_most <- aggregate(perc_land ~ GEOID_ZCTA5_10, zcta_rel, FUN = which.max)
# zcta_most$GEOID_ZCTA5_20 <- zcta_rel$GEOID_ZCTA5_20[which(!duplicated(zcta_most$GEOID_ZCTA5_10))+zcta_most$perc_land-1]
# rownames(zcta_most) <- zcta_most$GEOID_ZCTA5_10
# # keep the old, map to the new
# zip_cw$ZCTA10 <- zip_cw$ZCTA
# zip_cw$ZCTA <- zcta_most[zip_cw$ZCTA, "GEOID_ZCTA5_20"]

zip_to_zcta <- setNames(zip_cw$ZCTA, zip_cw$ZIP_CODE)
zcta_to_zip <- aggregate(ZIP_CODE ~ ZCTA, data = zip_cw, 
                         FUN = function(x){paste(x, collapse = ", ")})
zcta_to_zip <- setNames(zcta_to_zip$ZIP_CODE, zcta_to_zip$ZCTA)

# load crosswalk files
county_cw <- read.csv(file.path(data_folder, "Resources",
                                "zcta_county_rel_20.csv"),
                      colClasses = c(
                        "ZCTA5" = "character",
                        "GEOID" = "character"
                      ))
county_cw$STATE <- substr(county_cw$GEOID, 1, 2)
county_cw$COUNTY <- substr(county_cw$GEOID, 3, 5)
# collapse so that there's one zcta for every row (states get collapsed by pipe)
cty_cw <- 
  aggregate(STATE ~ ZCTA5, data = county_cw, 
            FUN = function(x){paste(x, collapse = "|")})
rownames(cty_cw) <- cty_cw$ZCTA5
# get unique states
un_st <- lapply(
  strsplit(cty_cw$STATE, "|", fixed = T), unique)
# original state will be the first given -- FIX THIS BY GREATEST AREA LATER
cty_cw$STATE <- sapply(un_st, `[`, 1)
cty_cw$STATE_2 <- sapply(un_st, `[`, 2)
# add state name
data("fips_codes")
f_st <- setNames(unique(fips_codes$state_name),
                 unique(fips_codes$state_code))
# subset to 50 states + dc
f_st <- f_st[f_st %in% c(state.name, "District of Columbia")]
# swap for the future
st_to_fips <- setNames(names(f_st), f_st)
cty_cw$STATE_NAME <- f_st[cty_cw$STATE]
# add all the counties and geoids as well
cty_cw$COUNTY <- aggregate(COUNTY ~ ZCTA5, data = county_cw, 
                           FUN = function(x){paste(x, collapse = "|")})[,2]
cty_cw$GEOID <- aggregate(GEOID ~ ZCTA5, data = county_cw, 
                          FUN = function(x){paste(x, collapse = "|")})[,2]

# no directionality percentile ranking
no_dir_perc_meas_df <- read.csv(
  file.path(data_folder, "Cleaned", 
            "HSE_MWI_ZCTA_No_Directionality_Percentile_Ranked_Measures.csv"),
  colClasses = c("GEOID" = "character")
)
colnames(no_dir_perc_meas_df)[colnames(no_dir_perc_meas_df) == "GEOID"] <- "ZCTA"
no_dir_perc_meas_df <- no_dir_perc_meas_df[no_dir_perc_meas_df$ZCTA != "",]
rownames(no_dir_perc_meas_df) <- no_dir_perc_meas_df$ZCTA

# MWI scores
# NOTE: may also save as RData for faster reading
mwi <- list()
mwi[["pop"]] <- read.csv(
  file.path(data_folder, "Cleaned", 
            "HSE_MWI_ZCTA_Mental_Wellness_Index_Population.csv"),
  colClasses = c("ZCTA" = "character")
)
# remove any empty zcta rows (miswriting?) -- TODO: fix in pipeline
mwi[["pop"]] <- mwi[["pop"]][mwi[["pop"]]$ZCTA != "",]
mwi[["black"]] <- read.csv(
  file.path(data_folder, "Cleaned", 
            "HSE_MWI_ZCTA_Mental_Wellness_Index_Black.csv"),
  colClasses = c("ZCTA" = "character")
)
# remove any empty zcta rows (miswriting?) -- TODO: fix in pipeline
mwi[["black"]] <- mwi[["black"]][mwi[["black"]]$ZCTA != "",]

# create a pretty set of names for later
st_abbrev_to_full <- c(state.name, "District of Columbia", "All States")
names(st_abbrev_to_full) <- c(state.abb, "DC", "All")

# load population totals
all_pop_df <- read.csv(
  file.path(data_folder, "Resources", "ACS_ZCTA_Total_Populations.csv"),
  colClasses = c("GEOID" = "character")
)
all_pop_df$perc_black <- all_pop_df$total_black/all_pop_df$total_pop*100
all_pop_df$perc_pop <- 100
rownames(all_pop_df) <- all_pop_df$GEOID

# plotting information ----

# get measure colors
meas_colors <- c(
  "purples", # SDOH
  "greens", # health status
  "blues", # healthcare acess
  "purple_blue_green" # MWI
)
names(meas_colors) <- c(unique(m_reg$Category), "Mental Wellness Index")

# get max/min colors for each palette
meas_max_colors <- c(
  "#5d499e", # SDOH
  "#157ba7", # healthcare status
  "#70ad47", # health access
  "#00441b" #"#70ad47" # MWI
)

#   sapply(1:length(meas_colors), function(x){
#   brewer.pal(3, meas_colors[x])[3]
# })
meas_min_colors <-  c(
  "#fcfbfd", # SDOH
  "#f7fbff", # health status
  "#f7fcf5", # health access
  "#3f157d" #"#5d499e" # MWI
)
#   sapply(1:length(meas_colors), function(x){
#   brewer.pal(3, meas_colors[x])[1]
# })
names(meas_max_colors) <- names(meas_min_colors) <- names(meas_colors)

# make meas_colors into a color function
meas_colors <- lapply(1:length(meas_min_colors), function(x){
  if (x != length(meas_min_colors)){
    colorRamp(c(meas_min_colors[x], meas_max_colors[x]), interpolate = "linear")
  } else { # MWI has something in the middle
    colorRamp(c(meas_min_colors[x], "#c6dbef", meas_max_colors[x]), interpolate = "linear")
  }
})
meas_colors_pal <- lapply(1:length(meas_min_colors), function(x){
  if (x != length(meas_min_colors)){
    colorRampPalette(c(meas_min_colors[x], meas_max_colors[x]), interpolate = "linear")
  } else { # MWI has something in the middle
    colorRampPalette(c(meas_min_colors[x], "#c6dbef", meas_max_colors[x]), interpolate = "linear")
  }
})
names(meas_colors) <- names(meas_colors_pal) <- names(meas_max_colors)

# replace min and max colors for MWI
meas_max_colors["Mental Wellness Index"] <-
  meas_colors_pal[["Mental Wellness Index"]](7)[6]
meas_min_colors["Mental Wellness Index"] <-
  meas_colors_pal[["Mental Wellness Index"]](7)[2]

overall <- app_preprocess(m_reg, info_df, mwi, app_start = T)
# add other specific data
overall[["m_reg"]] <- m_reg
overall[["meas_df"]] <- meas_df
overall[["info_dat"]] <- info_df
overall[["no_dir_perc_meas_df"]] <- no_dir_perc_meas_df

# add counties/states to mwi
for (idx in index_types){
  mwi[[idx]][, colnames(cty_cw)[-1]] <- 
    cty_cw[mwi[[idx]]$ZCTA, -1]
}
overall[["mwi"]] <- mwi

# plot functions ----

# plot the overall map, filled by measure/score (LEAFLET)
plot_map <- function(fill, geodat, idx, ol, is_all = F, is_com = F,
                     fill_opacity = .7,
                     add_poly = F, us_proxy = NA, zcta_choose = NA){
  # subset map for easy plotting
  gd_map <- geodat[,c(fill, "GEOID", "STATE", "STATE_NAME", "geometry")]
  colnames(gd_map)[1] <- "Fill"
  if (fill != "Mental_Wellness_Index"){
    # replace data with no directionality data
    gd_map$Fill <- ol$no_dir_perc_meas_df[gd_map$GEOID, fill]
  }
  gd_map[, colnames(all_pop_df)[-c(1:2)]] <- all_pop_df[gd_map$GEOID, -c(1:2)]
  
  # get rid of empty polygons
  gd_map <- gd_map[!is.na(gd_map$GEOID),]
  
  # create palette
  pal <- colorNumeric(
    palette = meas_colors[[ol$meas_col_to_type[ol$measure_to_names[[idx]][fill]]]],
    domain = c(0, gd_map$Fill, 100),
    na.color = "transparent",
    reverse = ifelse(fill == "Score", T, F)
  )
  pal_wo_na <- colorNumeric(
    palette = meas_colors[[ol$meas_col_to_type[ol$measure_to_names[[idx]][fill]]]],
    domain = c(0, gd_map$Fill, 100),
    na.color=rgb(0,0,0,0),
    reverse = ifelse(fill == "Score", T, F)
  )
  
  # labels 
  full_name <- ol$measure_to_names[[idx]][fill]
  if (full_name != "Mental Wellness Index"){
    full_name <- paste0(
      # ifelse(ol$info_dat[fill, "Directionality"] > 0, "Higher ", "Lower "), 
      full_name, 
      " Ranking"
    )
  }
  
  labels <- 
    paste0(
      "State: ", gd_map$STATE_NAME, "<br>",
      "ZCTA: ", gd_map$GEOID, "<br>", 
      "ZIP Code: ", unname(zcta_to_zip[gd_map$GEOID]), "<br>", 
      "Population: ", as.data.frame(gd_map)[, paste0("total_",idx)], 
      # " (", trunc(as.data.frame(gd_map)[, paste0("perc_",idx)]), "%)",
      "<br>",
      full_name,": ", trunc(gd_map$Fill)) %>%
    lapply(htmltools::HTML)
  
  # get initial zoom area
  bounds <- 
    if (!is_com){
      unname(st_bbox(geodat))
    } else { # community focuses on ZCTA
      unname(st_bbox(geodat[geodat$GEOID == zcta_choose,]))
    }
  
  if (!add_poly){
    mp <- 
      if (!is_all){ # for specific states
        # create map
        leaflet(data = gd_map) %>%
          addProviderTiles("CartoDB") %>% #OpenStreetMap
          addPolygons(fillColor = ~pal(Fill),
                      weight = 1,
                      opacity = 1,
                      color = "#b2aeae",
                      dashArray = "",
                      fillOpacity = fill_opacity,
                      layerId = ~GEOID,
                      highlight = highlightOptions(weight = 2,
                                                   color = "#666",
                                                   dashArray = "",
                                                   fillOpacity = 0.7,
                                                   bringToFront = !is_com),
                      label = labels) %>%
          addLegend(pal = pal_wo_na,
                    values = ~c(0, Fill, 100), 
                    opacity = 0.7, 
                    position = "bottomright",
                    title = unname(full_name)#,
                    # labFormat = function(type, cuts, p){
                    #   paste0(c("0 (More Obstacles)", 
                    #            "20", "40", "60",
                    #            "80", "100 (More Assets)"))
                    # }
                    ) %>%
          fitBounds(
            lng1 = bounds[1],
            lng2 = bounds[3],
            lat1 = bounds[2],
            lat2 = bounds[4]
          )
      } else {
        leaflet(data = (gd_map)) %>%
          addProviderTiles("CartoDB") %>% #OpenStreetMap
          addCircleMarkers(fillColor = ~pal(Fill),
                           weight = 1,
                           opacity = 1,
                           color = ~pal(Fill),
                           dashArray = "",
                           fillOpacity = fill_opacity,
                           layerId = ~GEOID,
                           label = labels,
                           radius = 5) %>%
          addLegend(pal = pal_wo_na,
                    values = ~c(0, Fill, 100), 
                    opacity = 0.7, 
                    position = "bottomright",
                    title = unname(full_name)#,
                    # labFormat = function(type, cuts, p){
                    #   paste0(c("0 (More Challenges)", 
                    #            "<p align = 'left'>20<p>", "40", "60",
                    #            "80", "100 (More Assets)"))
                    # }
                    ) %>%
          fitBounds(
            lng1 = bounds[1],
            lng2 = bounds[3],
            lat1 = bounds[2],
            lat2 = bounds[4]
          )
      }
    
    # if it's a community, we also want to focus on it
    if (is_com){
      zcta_select <- gd_map[gd_map$GEOID == zcta_choose,]
      mp <- mp %>% 
        addPolygons(
          data = zcta_select,
          fillColor = ~pal(Fill),
          weight = 4,
          opacity = 1,
          color = "#000",
          dashArray = "",
          fillOpacity = fill_opacity,
          # group = "remove_me",
          highlight = highlightOptions(weight = 4,
                                       color = "#000",
                                       dashArray = "",
                                       fillOpacity = 0.7,
                                       bringToFront = T),
          label = labels[gd_map$GEOID == zcta_choose]
        )
    }
  } else {
    zcta_select <- gd_map[gd_map$GEOID == zcta_choose,]
    mp <- 
      if (!is_all){
        us_proxy %>% 
          addPolygons(
            data = zcta_select,
            fillColor = ~pal(Fill),
            weight = 4,
            opacity = 1,
            color = "#000",
            dashArray = "",
            fillOpacity = fill_opacity,
            layerId = "remove_me",
            # group = "remove_me",
            highlight = highlightOptions(weight = 4,
                                         color = "#000",
                                         dashArray = "",
                                         fillOpacity = 0.7,
                                         bringToFront = T),
            label = labels[gd_map$GEOID == zcta_choose]
          )
      } else {
        us_proxy %>% 
          addCircleMarkers(
            data = zcta_select,
            fillColor = ~pal(Fill),
            weight = 4,
            opacity = 1,
            color = "#000",
            dashArray = "",
            fillOpacity = 1,
            layerId = "remove_me",
            label = labels[gd_map$GEOID == zcta_choose],
            radius = 7)
      }
  }
  
  return(mp)
}

# plot a distribution of the fill value using a beeswarm plot (PLOTLY)
plot_bee_distr <- function(fill, st, mwi, idx, ol, is_all = F, hl = F, zcta_hl = ""){
  bee.df <- data.frame(
    val = mwi[,fill],
    zcta = mwi$ZCTA,
    lab = rep("val", nrow(mwi)),
    focus = rep("val", nrow(mwi)),
    focus_alpha = rep(1, nrow(mwi))
  )
  # remove all the empty rows
  bee.df <- bee.df[complete.cases(bee.df),]
  if (fill != "Mental_Wellness_Index"){
    # replace data with no directionality data
    bee.df$val <- ol$no_dir_perc_meas_df[bee.df$zcta, fill]
  }
  
  # if we're going to highlight a point
  if (hl){
    row_hl <- which(bee.df$zcta == zcta_hl)
    bee.df$focus[row_hl] <- "Focus"
    bee.df$focus_alpha[-row_hl] <- .3
    
    pal <- colorNumeric(
      palette = meas_colors[[ol$meas_col_to_type[ol$measure_to_names[[idx]][fill]]]],
      domain = c(0, bee.df$val, 100),
      na.color = "transparent"
    )
    
    hl_pal <- c(
      "val" = "#e3e3e3",
      "Focus" = pal(bee.df[row_hl, fill])
    )
    hl_size <- c(
      "val" = 1.5,
      "Focus" = 3
    )
  }
  
  p <- 
    if (hl){
      ggplot(bee.df, aes(lab, val, color = val, size = focus))+
        scale_color_gradientn(
          colors = 
            meas_colors_pal[[ol$meas_col_to_type[ol$measure_to_names[[idx]][fill]]]](100),
          limits = c(0, 100)
        )+
        scale_size_manual(values = hl_size)
    } else {
      ggplot(bee.df, aes(lab, val, color = val), size = 1.5)+
        scale_color_gradientn(
          colors = 
            meas_colors_pal[[ol$meas_col_to_type[ol$measure_to_names[[idx]][fill]]]](100),
          limits = c(0, 100)
        )
    }
  
  full_name <- ol$measure_to_names[[idx]][fill]
  if (full_name != "Mental Wellness Index"){
    full_name <- paste0(
      # ifelse(ol$info_dat[fill, "Directionality"] > 0, "Higher ", "Lower "), 
      full_name, 
      " Ranking"
    )
  }
  
  p <- suppressWarnings(
    p + 
      theme_bw()+
      ylab(full_name)+
      theme(
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank(),
        legend.position = "none",
        plot.title = element_text(hjust = .5)
      )+
      ylim(-3, 103)+
      if (!is_all){
        geom_quasirandom(
          aes(text = paste0(
            "ZCTA: ", zcta, "\n",
            "ZIP Code: ", unname(zcta_to_zip[zcta]), "<br>", 
            full_name, ": ", trunc(val)
          )),
          dodge.width = NULL, alpha = bee.df$focus_alpha)
      } else {
        geom_violin(
          fill = meas_colors_pal[[ol$meas_col_to_type[ol$measure_to_names[[idx]][fill]]]](3)[2],
          color = meas_colors_pal[[ol$meas_col_to_type[ol$measure_to_names[[idx]][fill]]]](3)[2]
        )
      }
  )
  
  ggplotly(
    p,
    tooltip = c("text")
  ) %>% 
    layout(margin = list(l = 0, r = 10,
                         b = 0, t = 0)) %>%
    config(displayModeBar = F)
}

# map percentiles to text
quant_map <- function(perc){
  return(
    if (perc < 34) {
      "bottom third"
    } else if (perc < 67){
      "middle third"
    } else {
      "top third"
    }
  )
}

# wrap text with color using HTML tags
html_color <- function(meas_color, text){
  return(paste0("<font color =", meas_color,">",text,"</font>"))
}

# UI ----

ui <- fluidPage(
  # Title panel sets text in the browser tab
  # This is necessary because the navbarPage title is html and not straight text
  div(
    titlePanel(
      title="", 
      windowTitle=HTML(paste0("Mental Wellness Index™ Tool"))
    ),
    style="display:none"
  ),
  
  navbarPage(
    collapsible = T,
    title=
      if (show_mitre){
        div(
          a(
            href="https://www.mitre.org/",
            img(src="media/MITRE_logo.png", height="30"),
            target="blank",
          ),
          HTML(paste0("Mental Wellness Index™ Tool"))
        )
      } else {
        div(
          HTML(paste0("Mental Wellness Index™ Tool")),
          "style" = "padding-top:5px"
        )
      },
    theme="stylesheets/app.css",
    
    # explore states ----
    tabPanel(
      title = div("Explore States", class="explore"),
      class = "explore-panel",
      
      sidebarLayout(
        sidebarPanel(
          width = 3,
          bsCollapse(
            multiple = T,
            open = c("Exploration Options", "About Selected Measure", "About the Mental Wellness Index"),
            bsCollapsePanel(
              "Exploration Options",
              HTML("<b>Welcome to the Mental Wellness Index (MWI) Tool!</b> Select any of the options below to get started. <b>If you would like to focus on a specific ZIP Code*, click on it in the map to the right or select it from the list below.</b><p>"),
              radioButtons(
                inputId = "idx_type",
                label = "Which population's MWI do you want to view?",
                choiceValues = unname(index_types),
                choiceNames = c("Overall", "Black"),
                inline = T
              ),
              # TODO: UPDATE THESE BASED ON POPULATION SELECTED
              selectInput(
                "st_focus",
                "Which state would you like to focus on?",
                choices = c(unname(f_st), "All"),
                selected = "Virginia"
              ),
              selectInput(
                "us_map_fill",
                "What would you like to explore?",
                choices = overall$avail_meas_list[["pop"]]
              ),
              textInput(
                "zip_choose",
                label = "Which ZIP Code would you like to focus on in the selected state?",
                placeholder = "e.g. 35004, 00501, 20041, etc."
              ),
              actionButton("reset_zcta_click", "Reset ZIP Code Focus")
            ),
            bsCollapsePanel(
              "Custom MWI Upload",
              tagList(
                HTML("<font size = '2'><p>"),
                "To create the necessary custom Mental Wellness Index file, please see the \"Create Your Own MWI\" tab. Note that data uploaded to this application is not kept -- it is deleted once you leave the page. However, if you would like to keep your data on your computer while viewing the MWI, please see the \"Add Local Data to MWI on Your Computer\" section.",
                HTML("<i>NOTE: file upload is currently experiencing issues on the website. In the meantime, you can explore your custom MWI on your local computer by following steps 1 - 7 on the \"Add Local Data to Mental Wellness Index (MWI) On Your Computer\" page under \"Create Your Own MWI\".</i>"),
                HTML("<p></font>"),
                fileInput(
                  "custom_data_st",
                  label = "Upload Custom Mental Wellness Index (.RData)",
                  accept = ".RData"
                ),
                actionButton(
                  "custom_data_load_st",
                  "Run Custom MWI"
                ),
                actionButton(
                  "custom_data_reset_st",
                  "Reset"
                )
              )
            ),
            bsCollapsePanel(
              "About the Mental Wellness Index",
              HTML("<center>"),
              img(src = file.path("media", "MWI Framework (Transparent Background).png"), align = "center", width = "90%"),
              HTML("</center>"),
              HTML("<font size = '2'>"),
              HTML(paste0("The Mental Wellness Index is the weighted sum of 28 measure values, which quantify facilitators and barriers to mental wellness. For more information about the Mental Wellness Index, please see the 'MWI Toolkit' page.<p></p>"
              )),
              HTML(paste0(
                "All states are included.",
                " Selecting \"All\" will show all included states. Note that this is slower to render and will show ZCTAs as points.<p></p>")),
              HTML(paste0("* ZCTAs are used in the Mental Wellness Index and are represented in maps and plots. ZIP codes are analgous to ZCTAs. When ZIP Codes are entered above, they are mapped to ZCTAs. For more information on ZCTAs, please see <a href='https://www.census.gov/programs-surveys/geography/guidance/geo-areas/zctas.html' target = '_blank'>census.gov</a>.<p></p>"
              )),
              HTML("</font>")
            )
          )
        ),
        mainPanel(
          width = 9,
          tags$head(tags$script(src = "msg_api.js")),
          tags$head(tags$script(src = "web_content.js")),
          column(
            width = 8,
            uiOutput("us_map_legend"),
            HTML("<br>"),
            withSpinner(leafletOutput("us_map", height = 850),
                        type = 8, color = "#005B94", hide.ui = F)
          ),
          column(
            width = 4,
            # hr(),
            uiOutput("us_distr_title"),
            withSpinner(plotlyOutput("us_distr", height = 400),
                        type = 8, color = "#005B94", hide.ui = F),
            # hr(),
            bsCollapse(
              multiple = T,
              open = c("Measure Interpretation", "About Selected Measure"),
              bsCollapsePanel(
                "Measure Interpretation",
                conditionalPanel(
                  condition = "!output.focus_on",
                  # tableOutput("us_quantile"),
                  uiOutput("us_map_expl")
                ),
                conditionalPanel(
                  condition = "output.focus_on",
                  uiOutput("us_info")
                )
              ),
              bsCollapsePanel(
                "About Selected Measure",
                uiOutput("data_info"),
                HTML(paste0(
                  "<font size = '2'>",

                  "For more information on data and overall methodology, please see the \"MWI Toolkit\" page.",

                  "</font>"
                ))
              )
            )
          )
        )
      )
    ),
    
    # explore ZIP codes ----
    tabPanel(
      title = div("Explore ZIP Codes", class="explore"),
      class = "explore-panel",
      
      sidebarLayout(
        sidebarPanel(
          width = 3,
          bsCollapse(
            multiple = T,
            open = c("Exploration Options", "About Selected Measure", "About the Mental Wellness Index"),
            bsCollapsePanel(
              "Exploration Options",
              HTML("<b>Welcome to the Mental Wellness Index (MWI) Tool!</b> To explore your community's outcomes, enter a specific ZIP Code* to get started.<p>"),
              radioButtons(
                inputId = "idx_type_com",
                label = "Which population's MWI do you want to view?",
                choiceValues = unname(index_types),
                choiceNames = c("Overall", "Black"),
                inline = T
              ),
              selectInput(
                "com_map_fill",
                "What would you like to explore?",
                choices = overall$avail_meas_list[["pop"]]
              ),
              textInput(
                "zip_choose_com",
                label = "Which ZIP Code would you like to focus on?",
                placeholder = "e.g. 35004, 00501, 20041, etc."
              )
            ),
            bsCollapsePanel(
              "Custom MWI Upload",
                tagList(
                  HTML("<font size = '2'><p>"),
                  "To create the necessary custom Mental Wellness Index file, please see the \"Create Your Own MWI\" tab. Note that data uploaded to this application is not kept -- it is deleted once you leave the page. However, if you would like to keep your data on your computer while viewing the MWI, please see the \"Add Local Data to MWI on Your Computer\" section.",
                  HTML("<i>NOTE: file upload is currently experiencing issues on the website. In the meantime, you can explore your custom MWI on your local computer by following steps 1 - 7 on the \"Add Local Data to Mental Wellness Index (MWI) On Your Computer\" page under \"Create Your Own MWI\".</i>"),
                  HTML("</p></font>"),
                  fileInput(
                    "custom_data_com",
                    label = "Upload Custom Mental Wellness Index (.RData)",
                    accept = ".RData"
                  ),
                  actionButton(
                    "custom_data_load_com",
                    "Run Custom MWI"
                  ),
                  actionButton(
                    "custom_data_reset_com",
                    "Reset"
                  )
                )
            ),
            bsCollapsePanel(
              "About the Mental Wellness Index",
              HTML("<center>"),
              img(src = file.path("media", "MWI Framework (Transparent Background).png"), align = "center", width = "90%"),
              HTML("</center>"),
              HTML("<font size = '2'>"),
              HTML(paste0("The Mental Wellness Index is the weighted sum of 28 measure values, which quantify facilitators and barriers to mental wellness. For more information about the Mental Wellness Index, please see the 'MWI Toolkit' page.<p></p>"
              )),
              HTML(paste0(
                "All states are included.",
                " Selecting \"All\" will show all included states. Note that this is slower to render and will show ZCTAs as points.<p></p>")),
              HTML(paste0("* ZCTAs are used in the Mental Wellness Index and are represented in maps and plots. ZIP codes are analgous to ZCTAs. When ZIP Codes are entered above, they are mapped to ZCTAs. For more information on ZCTAs, please see <a href='https://www.census.gov/programs-surveys/geography/guidance/geo-areas/zctas.html' target = '_blank'>census.gov</a>.<p></p>"
              )),
              HTML("</font>")
            )
          )
        ),
        mainPanel(
          width = 9,
          tabsetPanel(
            tabPanel(
              "Explore ZCTA Maps",
              p(),
              column(
                width = 8,
                uiOutput("com_map_legend"),
                HTML("<br>"),
                withSpinner(leafletOutput("com_map", height = 850),
                            type = 8, color = "#005B94", hide.ui = F)
              ),
              column(
                width = 4,
                bsCollapse(
                  multiple = T,
                  open = c("ZCTA Measure Results", "Selected Measure Interpretation", "About Selected Measure"),
                  bsCollapsePanel(
                    "Selected Measure Interpretation",
                    uiOutput("com_map_expl")
                  ),
                  bsCollapsePanel(
                    "About Selected Measure",
                    uiOutput("data_info_com"),
                    HTML(paste0(
                      "<font size = '2'>",
                      "For more information on data and overall methodology, please see the \"MWI Toolkit\" page.",
                      "</font>"
                    ))
                  ),
                  bsCollapsePanel(
                    "ZCTA Measure Results",
                    uiOutput("com_map_report_card")
                  )
                )
              )
            ),
            tabPanel(
              "Explore ZCTA Measures",
              p(),
              bsCollapse(
                open = c("ZCTA Measure Results"),
                bsCollapsePanel(
                  "ZCTA Measure Results",
                  HTML("<p><i>Measures have ranks from 0 to 100. Measures with a higher rank (closer to 100) indicate more community-level <b>assets</b> or <b>obstacles</b> to mental wellness, based on their respective directionality. Measure value corrsponds to the exact value in the data, corresponding to the measure description. For more information, please see `MWI Measures and Data` in the MWI Toolkit.</i></p>"),
                  uiOutput("com_report_card_table_mwi"),
                  HTML("<p></p>"),
                  DTOutput("com_report_card_table")
                )
              )
            )
          )
        )
      )
    ),
    
    # upload data ----
    
    navbarMenu(
      "Create Your Own MWI",
      tabPanel(
        title = div("Adjust MWI Weights and ZIP Codes", class = "explore"),
        fluidRow(
          column(width = 2),
          column(
            width = 8,
            HTML("<center><h2>Change weights and ZIP Codes to create your own Mental Wellness Index (MWI)!</h2></center>"),
            HTML(paste0(
              "<p align = 'justify'>",
              "Weights used in the Mental Wellness Index control the relative influence each measure has on the total MWI; higher numbers inidcate a higher influence. If you adjust the weights to 0, a measure has no influence on the MWI. You can also use this page to rank a subset of ZIP Codes against each other.",
              "<br><br>",
              "To adjust the weights or change the ZIP Codes used in the Mental Wellness Index, follow the instructions below. If you want to add your own data to the MWI, go to the \"Add Local Data to MWI\" section. Note that data uploaded to this application is not kept -- it is deleted once you leave the page, including any processing done to it.",
              "<ol>",
              "<li>To update weights for each measure, click the \"Adjust MWI Weights\" tab below. Then update the table as desired by doubleclicking the 'Updated Weights' column, then editing the measure to the desired amount (0 or a positive number). Click outside of that edited entry to lock it in. Note that weights do not need to add to 100 (they will be normalized, and have georgraphy/race stratification penalties applied, when the Custom MWI is calculated).</li>",
              "<br>",
              "<li>To rank a subset of ZIP Codes or ZCTAs against each other, click the \"Subset Zip Codes/ZCTAs\" tab below. There, you will enter the ZIP Codes you want to create the MWI for in the text box, with each ZIP Code or ZCTA on a separate line. Note that these all must be either ZIP Codes or ZCTAs -- you cannot submit a mix. Use the switch below the text box to indicate whether the entries are ZIP Codes or ZCTAs. Note: If no values are entered, all ZCTAs will be used. Entering ZCTAs is more accurate. Comparing at least 10 ZCTAs/ZIP Codes is recommended.</li>",
              "<br>",
              "<li>Click 'Create Custom MWI' below. This will take some time.</li>",
              "<br>",
              "<li>Once the custom MWI creation is complete, click 'Download Custom MWI' to download an .RData file with all of the needed information to view your MWI in this tool. <b>Note: if you navigate away from this page, all processing will be lost! Nothing is stored within this application.</b></li>",
              "<br>",
              "<li>To view your MWI, click the 'Custom MWI Upload' box under 'Explore States' or 'Explore ZIP Codes' and upload the downloaded '.RData' file. Once the file is fully loaded, click 'Upload' to see your Custom MWI results.</li>",
              "</ol>",
              "</p>"
            )),
            hr(),
            HTML("<center>"),
            tabsetPanel(
              tabPanel(
                "Adjust MWI Weights",
                DTOutput("custom_mwi_weights")
              ),
              tabPanel(
                "Subset Zip Codes/ZCTAs",
                br(),
                textAreaInput(
                  "custom_mwi_zips",
                  "Enter ZIP Codes or ZCTAs to subset the MWI to (line separated):",
                  placeholder = "12345\n10034\n19567\n...",
                  height = "200px",
                  width = "400px"
                ),
                switchInput(
                  "custom_mwi_zip_choice",
                  onLabel = "ZIP Code",
                  offLabel = "ZCTA",
                  value = T, # zips shown
                  onStatus = "secondary",
                  offStatus = "secondary"
                )
              )
            ),
            HTML("<br><br>"),
            actionButton("custom_mwi_go_weights", "Create Custom MWI"),
            downloadButton("download_custom_mwi_weights", "Download Custom MWI"),
            HTML("<br><br>"),
            verbatimTextOutput("custom_error_weights"),
            HTML("</center>")
          ),
          column(width = 2)
        )
      ),
      tabPanel(
        title = div("Add Local Data to MWI", class = "explore"),
        fluidRow(
          column(width = 2),
          column(
            width = 8,
            HTML("<center><h2>Add Local Data to Mental Wellness Index (MWI)</h2></center>"),
            HTML(paste0(
              "<p align = 'justify'>",
              "To create your own Mental Wellness Index with your own local data, follow the instructions below to create your own MWI for your community below by adjusting weights and adding your own data and metadata. If you only want to adjust the weights in the MWI, go to the \"Adjust MWI Weights\" section. Note that data uploaded to this application is not kept -- it is deleted once you leave the page, including any processing done to it. However, if you would like to keep your data on your computer while creating the MWI, please see the \"Add Local Data to MWI on Your Computer\" section. ",
              "<i>NOTE: file upload is currently experiencing issues on the website. In the meantime, you can explore your custom MWI on your local computer by following steps 1 - 7 on the \"Add Local Data to Mental Wellness Index (MWI) On Your Computer\" page under \"Create Your Own MWI\".</i>",
              "<ol>",
              "<li> Put each of your datasets in a CSV (comma separated value) format, with one column corresponding to the geographical ID of the data, a column corresponding to the numerator of the data, and another column corresponding to the denominator (if needed).</li>",
              "<ul>",
              "<li>Accepted geographical ID types are always numeric and include the following:</li>",
              "<ul>",
              "<li>ZCTA: 5 digit ZCTA (example: 35406)</li>",
              "<li>County: 5 digit County FIPS Code (2 digits state code and 3 digit county code, example: 01001)</li>",
              "<li>ZIP Code: US Postal Service ZIP Code (example: 35051)</li>",
              "<li>Census Tract: 11 digit Census Tract FIPS Code (2 digits state code, 3 digit county code, and 6 digit tract code, example: 01001020100)</li>",
              "</ul>",
              "<li>If a denominator column is provided, the final input to the MWI will be the numerator divided by the denominator, multiplied by the scaling number (specified in the metadata file, see next step).</li>",
              "<li>Numerators and denominators must be numeric columns.</li>",
              "<li>Missing data should have cells left blank.</li>",
              "<li>If race stratified, there should be two columns: one ending in '_pop' corresponding to the overall population measure, and one ending in '_black' corresponding to the black populations measure. In the Metadata.xlsx file edit, that row's 'Preprocessed' column should be set to TRUE.</li>",
              "</ul>",
              "<br>",
              "<li> Download Metadata.xlsx with the button below. If adding custom data, add a row and fill in information for each measure you want to add to the Mental Wellness Index. Descriptions for each column can be found in the 'Column Descriptions' sheet of the Metadata.xlsx. Note that <b>all</b> column names, with the exception of 'denominator', must be filled out.</li>",
              "<ul>",
              "<li>If you have multiple measures in one file, add a row for each measure and its qualities, but specify the same file name.</li>",
              "<li>If you would like to remove a measure in your MWI, either delete the measure row or set its weight to 0.</li>",
              "<li>If you would only like to adjust weights, change only the weight column to the desired values. Note that penalties for race stratifications and geographic granularity are still applied and total weights are scaled to sum to 100.</li>",
              "</ul>",
              "<br>",
              "<li>Upload your Metadata.xlsx and custom data files (if using) and click 'Create Custom MWI' below. Select multiple files by pressing ctrl/cmd and then clicking on each file in the file explorer box. Note: Do not have files open while uploading. This will take some time, depending on the amount of measures included.</li>",
              "<br>",
              "<li>Once the custom MWI creation is complete, click 'Download Custom MWI' to download an .RData file with all of the needed information to view your MWI in this tool. <b>Note: if you navigate away from this page, all processing and data will be lost! Nothing is stored within this application.</b></li>",
              "<br>",
              "<li>To view your MWI, click the 'Custom MWI Upload' box under 'Explore States' or 'Explore ZIP Codes' and upload the downloaded '.RData' file. Once the file is fully loaded, click 'Upload' to see your Custom MWI results.</li>",
              "</ol>",
              "</p>"
            )),
            tagList(
              hr(),
              HTML("<center>"),
              downloadButton("download_metadata", "Download Metadata.xlsx"),
              HTML("<br><br>"),
              fileInput(
                "custom_zip", 
                "Upload Custom Data Files (.xlsx, .csv) (do not have files open):",
                accept = c(".xlsx", ".csv"),
                multiple = T
              ),
              actionButton("custom_mwi_go", "Create Custom MWI"),
              downloadButton("download_custom_mwi", "Download Custom MWI"),
              HTML("<br><br>"),
              verbatimTextOutput("custom_error"),
              HTML("</center>")
            )
          ),
          column(width = 2)
        )
      ),
      tabPanel(
        title = div("Add Local Data to MWI on Your Computer", class = "explore"),
        fluidRow(
          column(width = 2),
          column(
            width = 8,
            HTML("<center><h2>Add Local Data to Mental Wellness Index (MWI) On Your Computer</h2></center>"),
            HTML(paste0(
              "<p align = 'justify'>",
              "If you want to keep your data on your computer, follow the instructions below to create your own MWI for your community by adjusting weights and adding your own data and metadata.",
              "<ol>",
              tagList(HTML(paste0(
                "<li> Download free versions of <a href = 'https://www.r-project.org/' target = '_blank'>R</a> and <a href = 'https://www.rstudio.com/products/rstudio/download/' target = '_blank'>RStudio</a>. Download a modern browser (Firefox, Chrome, Edge, etc.) and make that your default browser if you haven't already.</li>",
                "<br>",
                "<li> Go to the <a href = 'https://github.com/mitre/hse-mwi' target = '_blank'>Mental Wellness Index GitHub page</a> and download the repository by clicking \"Code\" in the top right corner, then clicking \"Download ZIP\" from the dropdown menu. This should download a ZIP file of the MWI repository into your downloads folder, called \"hse-mwi-main.zip\".</li>",
                "<br>",
                "<li> Unzip \"hse-mwi-main.zip\".</li>",
                "<br>",
                "<li> In the unzipped folder, open \"app.R\" in RStudio. This should open RStudio and the \"app.R\" script in the top left hand corner of the application.</li>",
                "<br>",
                "<li> In the console window, which is in the bottom left hand corner, enter the following line and answer \"yes\" to all prompts in the console as you install these packages:</li>",
                "<ul>",
                "<li>install.packages('readxl', 'writexl', 'htmltools', 'shiny', 'tigris', 'leaflet', 'RColorBrewer', 'sf', 'plotly', 'ggbeeswarm', 'shinyWidgets', 'sass', 'shinycssloaders', 'shinyBS', 'DT', 'dplyr')</li>",
                "</ul>",
                "<br>",
                "<li> In the top right hand corner of the \"app.R\" window, you should see \"Run App\". Click the small downward arrow to the right of that and click \"Run External\". Then click \"Run App\".</li>",
                "<br>",
                "<li> After a delay (this will be slow the first time, then quicker after that), the Mental Wellness Index Tool should open in your browser. Click on the \"Create Your Own MWI\" tab and follow the remaining steps to create your own MWI.</li>",
                "<br>"
              ))),
              "<li> If you are only adjusting weights for included data, skip the next step.</li>",
              "<br>",
              "<li> Put each of your datasets in a CSV (comma separated value) format, with one column corresponding to the geographical ID of the data, a column corresponding to the numerator of the data, and another column corresponding to the denominator (if needed).</li>",
              "<ul>",
              "<li>Accepted geographical ID types are always numeric and include the following:</li>",
              "<ul>",
              "<li>ZCTA: 5 digit ZCTA (example: 35406)</li>",
              "<li>County: 5 digit County FIPS Code (2 digits state code and 3 digit county code, example: 01001)</li>",
              "<li>ZIP Code: US Postal Service ZIP Code (example: 35051)</li>",
              "<li>Census Tract: 11 digit Census Tract FIPS Code (2 digits state code, 3 digit county code, and 6 digit tract code, example: 01001020100)</li>",
              "</ul>",
              "<li>If a denominator column is provided, the final input to the MWI will be the numerator divided by the denominator, multiplied by the scaling number (specified in the metadata file, see next step).</li>",
              "<li>Numerators and denominators must be numeric columns.</li>",
              "<li>Missing data should have cells left blank.</li>",
              "<li>If race stratified, there should be two columns: one ending in '_pop' corresponding to the overall population measure, and one ending in '_black' corresponding to the black populations measure. In the Metadata.xlsx file edit, that row's 'Preprocessed' column should be set to TRUE.</li>",
              "</ul>",
              "<br>",
              "<li> Download Metadata.xlsx with the button below. If adding custom data, add a row and fill in information for each measure you want to add to the Mental Wellness Index. Descriptions for each column can be found in the 'Column Descriptions' sheet of the Metadata.xlsx. Note that <b>all</b> column names, with the exception of 'denominator', must be filled out.</li>",
              "<ul>",
              "<li>If you have multiple measures in one file, add a row for each measure and its qualities, but specify the same file name.</li>",
              "<li>If you would like to remove a measure in your MWI, either delete the measure row or set its weight to 0.</li>",
              "<li>If you would only like to adjust weights, change only the weight column to the desired values. Note that penalties for race stratifications and geographic granularity are still applied and total weights are scaled to sum to 100.</li>",
              "</ul>",
              "<br>",
              "<li>Upload your Metadata.xlsx and custom data files (if using) and click 'Create Custom MWI' below. Select multiple files by pressing ctrl/cmd and then clicking on each file in the file explorer box. Note: Do not have files open while uploading. This will take some time, depending on the amount of measures included.",
              "<i>NOTE: file upload is currently experiencing issues on the website. In the meantime, you can explore your custom MWI on your local computer by following steps 1 - 7 on the \"Add Local Data to Mental Wellness Index (MWI) On Your Computer\" page under \"Create Your Own MWI\".</i>",
              "</li>",
              "<br>",
              "<li>Once the custom MWI creation is complete, click 'Download Custom MWI' to download an .RData file with all of the needed information to view your MWI in this tool. <b>Note: if you navigate away from this page, all processing and data will be lost! Nothing is stored within this application.</b></li>",
              "<br>",
              "<li>To view your MWI, click the 'Custom MWI Upload' box under 'Explore States' or 'Explore ZIP Codes' and upload the downloaded '.RData' file. Once the file is fully loaded, click 'Upload' to see your Custom MWI results.</li>",
              "</ol>",
              "</p>"
            )),
            tagList(
              hr(),
              HTML("<center>"),
              downloadButton("download_metadata_comp", "Download Metadata.xlsx"),
              HTML("<br><br>"),
              fileInput(
                "custom_zip_comp", 
                "Upload Custom Data Files (.xlsx, .csv) (do not have files open):",
                accept = c(".xlsx", ".csv"),
                multiple = T
              ),
              actionButton("custom_mwi_go_comp", "Create Custom MWI"),
              downloadButton("download_custom_mwi_comp", "Download Custom MWI"),
              HTML("<br><br>"),
              verbatimTextOutput("custom_error_comp"),
              HTML("</center>")
            )
          ),
          column(width = 2)
        )
      )
    ),
    

    # mwi toolkit ----
    
    # add toolkit pages dynamically since there are a lot of them
    do.call(
      navbarMenu,
      c(menuName = "toolkit",
        title = "MWI Toolkit",
        lapply(
          mwi_toolkit_order,
          function(x){
            tabPanel(
              id = tolower(x),
              title = div(gsub("_", " ", x), class = "about"),
              htmltools::tags$iframe(
                src = paste0("mwi-toolkit/", x, ".html"),
                class = "about-panel",
                frameborder = 0,
                scrolling = "auto")
              
            )
          }
        )
      )
    )
  ),
  
  # Copyright footer
  if (show_mitre){
  HTML(paste0(
    "<span class = 'copyright-footer'>&copy; ",
    format(Sys.Date(), "%Y"),
    
    ", The MITRE Corporation",
    "</span>"
  ))
  }
)

# SERVER ----

server <- function(input, output, session) {
  # preallocate custom data ----
  
  # overall list of mwi data
  ol <- do.call(reactiveValues, overall)
  
  # preallocate reactive values: state view ----
  
  focus_info <- reactiveValues(
    "hl" = F,
    "ZCTA" = ""
  )
  
  st_sub <- reactiveValues(
    "idx" = "pop",
    "st" = "Virginia",
    "geodat" = overall$geodat[["pop"]][overall$geodat[["pop"]]$STATE_NAME == "Virginia",],
    "mwi" = overall$mwi[["pop"]][overall$mwi[["pop"]]$STATE_NAME == "Virginia",],
    "us_map_fill" = "Mental_Wellness_Index",
    "is_all" = F
  )
  
  us_proxy <- leafletProxy("us_map")
  
  # preallocate reactive values: community view ----
  
  com_sub <- reactiveValues(
    "idx" = "pop",
    "ZCTA" = "23936", 
    "geodat" = overall$geodat[["pop"]][ # community -- within +/- .5
      st_coordinates(overall$geopts$pop)[,1] >=
        st_coordinates(overall$geopts$pop[overall$geopts$pop$GEOID == "23936",])[1] - 1 &
        st_coordinates(overall$geopts$pop)[,1] <=
        st_coordinates(overall$geopts$pop[overall$geopts$pop$GEOID == "23936",])[1] + 1 &
        st_coordinates(overall$geopts$pop)[,2] >=
        st_coordinates(overall$geopts$pop[overall$geopts$pop$GEOID == "23936",])[2] - 1 &
        st_coordinates(overall$geopts$pop)[,2] <=
        st_coordinates(overall$geopts$pop[overall$geopts$pop$GEOID == "23936",])[2] + 1 
      ,],
    "mwi" = overall$mwi[["pop"]][# community -- within +/- .5
      overall$mwi[["pop"]]$ZCTA %in% 
        overall$geodat[["pop"]]$GEOID[
          st_coordinates(overall$geopts$pop)[,1] >=
            st_coordinates(overall$geopts$pop[overall$geopts$pop$GEOID == "23936",])[1] - 1 &
            st_coordinates(overall$geopts$pop)[,1] <=
            st_coordinates(overall$geopts$pop[overall$geopts$pop$GEOID == "23936",])[1] + 1 &
            st_coordinates(overall$geopts$pop)[,2] >=
            st_coordinates(overall$geopts$pop[overall$geopts$pop$GEOID == "23936",])[2] - 1 &
            st_coordinates(overall$geopts$pop)[,2] <=
            st_coordinates(overall$geopts$pop[overall$geopts$pop$GEOID == "23936",])[2] + 1 
        ]
      ,],
    "com_map_fill" = "Mental_Wellness_Index"
  )
  
  # create and observe starting modal ----
  
  welcome_modal <- modalDialog(
    title = 
      HTML("<b><center>Welcome to the Mental Wellness Index™!</b></center>"),
    size = "l",
    fluidRow(
      column(width = 1),
      column(width = 10,
             HTML("<p align = 'center'><font size = '3'>"),
             HTML(
               "The <b>Mental Wellness Index (MWI)</b> combines 28 factors that influence <b>community-level mental wellness</b> into a single value for <b>each ZIP code</b> in the nation."
             ),
             HTML("</p></font>"),
             HTML("<center>"),
             img(src = file.path("media", "MWI Framework (Transparent Background).png"), align = "center", width = "60%"),
             HTML("</center>"),
             HTML("<br>"),
             HTML("<center>"),
             HTML("To learn more and view MWI videos, click <b>MWI Toolkit</b> in the blue bar at the top of the page."),
            
             HTML("</center>"),
             HTML("<center><font size = '2'><i>Notes: This application is best viewed on a tablet or computer in full screen mode. Data updated as of January 2023.</i></font></center>"),
      )),
     
    footer = tagList(
      HTML("<center>"),
      modalButton("Start Exploring!"),
      HTML("</center>")
    ),
    easyClose = T # it will just use defaults
  )
  
  showModal(welcome_modal)
  

  observeEvent(input$learn_button, {
    updateNavbarPage(session = session, 
                     inputId = "toolkit", 
                     selected = "mwi_overview")
    
    removeModal()
  })
  
  observeEvent(input$video_button, {
    updateNavbarPage(session = session, 
                     inputId = "toolkit", 
                     selected = "mwi_tool_videos_and_guides")
    
    removeModal()
  })
  
  observeEvent(input$enter_mwi, {
    # update all of the inputs -- this will cascade
    
    # update available states
    updateSelectInput(
      session = session,
      "st_focus",
      "Which state would you like to focus on?",
      choices = c(unname(f_st), "All"),
      selected = input$start_st
    )
    
    # update selected defaults for community view
    com_sub$ZCTA <- ol$mwi$pop$ZCTA[ol$mwi$pop$STATE_NAME == input$start_st][1]
    # community boundary
    # community -- within +/- .5
    com_log <- st_coordinates(ol$geopts$pop)[,1] >=
      st_coordinates(ol$geopts$pop[ol$geopts$pop$GEOID == com_sub$ZCTA,])[1] - 1 &
      st_coordinates(ol$geopts$pop)[,1] <=
      st_coordinates(ol$geopts$pop[ol$geopts$pop$GEOID == com_sub$ZCTA,])[1] + 1 &
      st_coordinates(ol$geopts$pop)[,2] >=
      st_coordinates(ol$geopts$pop[ol$geopts$pop$GEOID == com_sub$ZCTA,])[2] - 1 &
      st_coordinates(ol$geopts$pop)[,2] <=
      st_coordinates(ol$geopts$pop[ol$geopts$pop$GEOID == com_sub$ZCTA,])[2] + 1 
    
    com_sub$geodat <- ol$geodat[["pop"]][com_log,]
    com_sub$mwi <- ol$mwi[["pop"]][# community -- within +/- .5
      ol$mwi[["pop"]]$ZCTA %in% 
        ol$geodat[["pop"]]$GEOID[com_log],]
    
    removeModal()
  })
  
  # observe custom data ----
  
  # observe custom data upload: state
  observeEvent(input$custom_data_load_st, {
    withProgress(
      message = "Uploading custom Mental Wellness Index!", {
        if (!is.null(input$custom_data_st)){
          validate(need(endsWith(tolower(input$custom_data_st$datapath), ".rdata"),
                        "Must upload .RData File"))
          
          # load the RData file -- contains ov
          load(input$custom_data_st$datapath)
          
          for (ov in names(ol)){
            ol[[ov]] <- overall_output[[ov]]
          }
          
          # add geo data from previously loaded data --- this takes a bit, but 
          # is worth it
          geodat <- geopts <- list()
          for (idx in index_types){
            geo_sub <- st_drop_geometry(overall$geodat[[idx]])[, "GEOID"] %in%
              overall_output$mwi[[idx]][,"ZCTA"]
            
            geodat[[idx]] <-
              left_join(overall$geodat[[idx]][geo_sub, 1:7], 
                        overall_output$mwi[[idx]], by = c("GEOID" = "ZCTA"))
            
            # sort by state code and zcta
            geodat[[idx]] <- geodat[[idx]][order(geodat[[idx]]$STATE,
                                                 geodat[[idx]]$GEOID),]
            
            # convert to points for US visualization -- ignore warnings
            geopts[[idx]] <- st_centroid(geodat[[idx]])
          }
          
          ol[["geodat"]] <- geodat
          ol[["geopts"]] <- geopts
          
          
          # update available states
          updateSelectInput(
            session = session,
            "st_focus",
            "Which state would you like to focus on?",
            choices = c(unname(f_st[f_st %in% f_st[ol$mwi$pop$STATE]]), "All"),
            selected = unname(f_st[f_st %in% f_st[ol$mwi$pop$STATE]])[1]
          )
          
          # update available states
          updateSelectInput(
            session = session,
            "us_map_fill",
            "What would you like to explore?",
            choices = ol$avail_meas_list[["pop"]]
          )
          updateSelectInput(
            session = session,
            "com_map_fill",
            "What would you like to explore?",
            choices = ol$avail_meas_list[["pop"]]
          )
          
          # update selected defaults for community view
          com_sub$ZCTA <- ol$mwi$pop$ZCTA[1]
          com_sub$geodat <- ol$geodat[["pop"]][ # community -- within +/- .5
            st_coordinates(ol$geopts$pop)[,1] >=
              st_coordinates(ol$geopts$pop[ol$geopts$pop$GEOID == com_sub$ZCTA,])[1] - 1 &
              st_coordinates(ol$geopts$pop)[,1] <=
              st_coordinates(ol$geopts$pop[ol$geopts$pop$GEOID == com_sub$ZCTA,])[1] + 1 &
              st_coordinates(ol$geopts$pop)[,2] >=
              st_coordinates(ol$geopts$pop[ol$geopts$pop$GEOID == com_sub$ZCTA,])[2] - 1 &
              st_coordinates(ol$geopts$pop)[,2] <=
              st_coordinates(ol$geopts$pop[ol$geopts$pop$GEOID == com_sub$ZCTA,])[2] + 1 
            ,]
          com_sub$mwi <- ol$mwi[["pop"]][# community -- within +/- .5
            ol$mwi[["pop"]]$ZCTA %in% 
              ol$geodat[["pop"]]$GEOID[ # community -- within +/- .5
                st_coordinates(ol$geopts$pop)[,1] >=
                  st_coordinates(ol$geopts$pop[ol$geopts$pop$GEOID == com_sub$ZCTA,])[1] - 1 &
                  st_coordinates(ol$geopts$pop)[,1] <=
                  st_coordinates(ol$geopts$pop[ol$geopts$pop$GEOID == com_sub$ZCTA,])[1] + 1 &
                  st_coordinates(ol$geopts$pop)[,2] >=
                  st_coordinates(ol$geopts$pop[ol$geopts$pop$GEOID == com_sub$ZCTA,])[2] - 1 &
                  st_coordinates(ol$geopts$pop)[,2] <=
                  st_coordinates(ol$geopts$pop[ol$geopts$pop$GEOID == com_sub$ZCTA,])[2] + 1 
              ],]
        }
      })
  })
  
  # observe custom data upload: community
  observeEvent(input$custom_data_load_com, {
    withProgress(
      message = "Uploading custom Mental Wellness Index!", {
        if (!is.null(input$custom_data_com)){
          # load the RData file
          load(input$custom_data_com$datapath)
          
          for (ov in names(ol)){
            ol[[ov]] <- overall_output[[ov]]
          }
          
          # add geo data from previously loaded data --- this takes a bit, but 
          # is worth it
          geodat <- geopts <- list()
          for (idx in index_types){
            geo_sub <- st_drop_geometry(overall$geodat[[idx]])[, "GEOID"] %in%
              overall_output$mwi[[idx]][,"ZCTA"]
            
            geodat[[idx]] <-
              left_join(overall$geodat[[idx]][geo_sub, 1:7], 
                        overall_output$mwi[[idx]], by = c("GEOID" = "ZCTA"))
            
            # sort by state code and zcta
            geodat[[idx]] <- geodat[[idx]][order(geodat[[idx]]$STATE,
                                                 geodat[[idx]]$GEOID),]
            
            # convert to points for US visualization -- ignore warnings
            geopts[[idx]] <- st_centroid(geodat[[idx]])
          }
          
          ol[["geodat"]] <- geodat
          ol[["geopts"]] <- geopts
          
          # update available states
          updateSelectInput(
            session = session,
            "st_focus",
            "Which state would you like to focus on?",
            choices = c(unname(f_st[f_st %in% f_st[ol$mwi$pop$STATE]]), "All"),
            selected = unname(f_st[f_st %in% f_st[ol$mwi$pop$STATE]])[1]
          )
          
          # update available states
          updateSelectInput(
            session = session,
            "us_map_fill",
            "What would you like to explore?",
            choices = ol$avail_meas_list[["pop"]]
          )
          updateSelectInput(
            session = session,
            "com_map_fill",
            "What would you like to explore?",
            choices = ol$avail_meas_list[["pop"]]
          )
          
          # update selected defaults for community view
          com_sub$ZCTA <- ol$mwi$pop$ZCTA[1]
          com_sub$geodat <- ol$geodat[["pop"]][ # community -- within +/- .5
            st_coordinates(ol$geopts$pop)[,1] >=
              st_coordinates(ol$geopts$pop[ol$geopts$pop$GEOID == com_sub$ZCTA,])[1] - 1 &
              st_coordinates(ol$geopts$pop)[,1] <=
              st_coordinates(ol$geopts$pop[ol$geopts$pop$GEOID == com_sub$ZCTA,])[1] + 1 &
              st_coordinates(ol$geopts$pop)[,2] >=
              st_coordinates(ol$geopts$pop[ol$geopts$pop$GEOID == com_sub$ZCTA,])[2] - 1 &
              st_coordinates(ol$geopts$pop)[,2] <=
              st_coordinates(ol$geopts$pop[ol$geopts$pop$GEOID == com_sub$ZCTA,])[2] + 1 
            ,]
          com_sub$mwi <- ol$mwi[["pop"]][# community -- within +/- .5
            ol$mwi[["pop"]]$ZCTA %in% 
              ol$geodat[["pop"]]$GEOID[ # community -- within +/- .5
                st_coordinates(ol$geopts$pop)[,1] >=
                  st_coordinates(ol$geopts$pop[ol$geopts$pop$GEOID == com_sub$ZCTA,])[1] - 1 &
                  st_coordinates(ol$geopts$pop)[,1] <=
                  st_coordinates(ol$geopts$pop[ol$geopts$pop$GEOID == com_sub$ZCTA,])[1] + 1 &
                  st_coordinates(ol$geopts$pop)[,2] >=
                  st_coordinates(ol$geopts$pop[ol$geopts$pop$GEOID == com_sub$ZCTA,])[2] - 1 &
                  st_coordinates(ol$geopts$pop)[,2] <=
                  st_coordinates(ol$geopts$pop[ol$geopts$pop$GEOID == com_sub$ZCTA,])[2] + 1 
              ],]
        }
      })
  })
  
  # reset to overall MWI
  observeEvent(c(input$custom_data_reset_st, input$custom_data_reset_com), {
    if (!is.null(input$custom_data_com) | !is.null(input$custom_data_st)){
      for (ov in names(ol)){
        ol[[ov]] <- overall[[ov]]
      }
      
      # update available states
      updateSelectInput(
        session = session,
        "st_focus",
        "Which state would you like to focus on?",
        choices = c(unname(f_st[f_st %in% f_st[ol$mwi$pop$STATE]]), "All"),
        selected = unname(f_st[f_st %in% f_st[ol$mwi$pop$STATE]])[1]
      )
      
      # update available states
      updateSelectInput(
        session = session,
        "us_map_fill",
        "What would you like to explore?",
        choices = ol$avail_meas_list[["pop"]]
      )
      updateSelectInput(
        session = session,
        "com_map_fill",
        "What would you like to explore?",
        choices = ol$avail_meas_list[["pop"]]
      )
      
      # update selected defaults for community view
      com_sub$ZCTA <- "23936"
      com_sub$geodat <- ol$geodat[["pop"]][ # community -- within +/- .5
        st_coordinates(ol$geopts$pop)[,1] >=
          st_coordinates(ol$geopts$pop[ol$geopts$pop$GEOID == com_sub$ZCTA,])[1] - 1 &
          st_coordinates(ol$geopts$pop)[,1] <=
          st_coordinates(ol$geopts$pop[ol$geopts$pop$GEOID == com_sub$ZCTA,])[1] + 1 &
          st_coordinates(ol$geopts$pop)[,2] >=
          st_coordinates(ol$geopts$pop[ol$geopts$pop$GEOID == com_sub$ZCTA,])[2] - 1 &
          st_coordinates(ol$geopts$pop)[,2] <=
          st_coordinates(ol$geopts$pop[ol$geopts$pop$GEOID == com_sub$ZCTA,])[2] + 1 
        ,]
      com_sub$mwi <- ol$mwi[["pop"]][# community -- within +/- .5
        ol$mwi[["pop"]]$ZCTA %in% 
          ol$geodat[["pop"]]$GEOID[ # community -- within +/- .5
            st_coordinates(ol$geopts$pop)[,1] >=
              st_coordinates(ol$geopts$pop[ol$geopts$pop$GEOID == com_sub$ZCTA,])[1] - 1 &
              st_coordinates(ol$geopts$pop)[,1] <=
              st_coordinates(ol$geopts$pop[ol$geopts$pop$GEOID == com_sub$ZCTA,])[1] + 1 &
              st_coordinates(ol$geopts$pop)[,2] >=
              st_coordinates(ol$geopts$pop[ol$geopts$pop$GEOID == com_sub$ZCTA,])[2] - 1 &
              st_coordinates(ol$geopts$pop)[,2] <=
              st_coordinates(ol$geopts$pop[ol$geopts$pop$GEOID == com_sub$ZCTA,])[2] + 1 
          ],]
    }
  })
  
  # observe button inputs and clicks: state view ----
  
  # update measures when the population type changes
  observeEvent(input$idx_type, {
    idx <- input$idx_type
    
    fill <- 
      if (idx == "pop" & grepl("*_black$", input$us_map_fill)){
        gsub("*_black$", "_pop", input$us_map_fill)
      } else if (idx == "black" & grepl("*_pop$", input$us_map_fill) &
                 !input$us_map_fill %in% colnames(ol$mwi[["black"]])){
        gsub("*_pop$", "_black", input$us_map_fill)
      } else {
        input$us_map_fill
      }
    
    updateSelectInput(
      session = session,
      "us_map_fill",
      "What would you like to explore?",
      choices = ol$avail_meas_list[[idx]],
      selected = fill
    )
  })
  
  # update the data based on state or population change
  observeEvent(c(input$st_focus, input$idx_type), {
    idx <- input$idx_type
    
    # if the state is the thing that's changing, reset focus, otherwise no
    if (idx == st_sub$idx){
      focus_info$hl <- F
      focus_info$ZCTA <- ""
      
      # remove any previously highlighted polygon
      if (!st_sub$is_all){
        us_proxy %>% removeShape("remove_me")
      } else {
        us_proxy %>% removeMarker("remove_me")
      }
    }
    
    st_sub$idx <- idx
    
    if (input$st_focus == "All"){
      st_sub$st <- "All"
      st_sub$geodat <- ol$geopts[[idx]]
      st_sub$mwi <- ol$mwi[[idx]]
      st_sub$is_all <- T
      
    } else {
      # also include zips from bordering states
      
      st_sub$st <- input$st_focus
      st_sub$geodat <- ol$geodat[[idx]][
        ol$geodat[[idx]]$STATE_NAME == input$st_focus | 
          ol$geodat[[idx]]$STATE_2 == st_to_fips[input$st_focus] & 
          !is.na(ol$geodat[[idx]]$STATE_2),]
      st_sub$mwi <- ol$mwi[[idx]][
        ol$mwi[[idx]]$STATE_NAME == input$st_focus | 
          ol$mwi[[idx]]$STATE_2 == st_to_fips[input$st_focus] & 
          !is.na(ol$mwi[[idx]]$STATE_2),]
      st_sub$is_all <- F
    }
    
    st_sub$us_map_fill <- 
      if (st_sub$idx == "pop" & grepl("*_black$", input$us_map_fill)){
        gsub("*_black$", "_pop", input$us_map_fill)
      } else if (st_sub$idx == "black" & grepl("*_pop$", input$us_map_fill) &
                 !input$us_map_fill %in% colnames(ol$mwi[["black"]])){
        gsub("*_pop$", "_black", input$us_map_fill)
      } else {
        input$us_map_fill
      }
  })
  
  # update the ZCTA
  observeEvent(input$reset_zcta_click, {
    focus_info$hl <- F
    focus_info$ZCTA <- ""
    
    # remove any previously highlighted polygon
    if (!st_sub$is_all){
      us_proxy %>% removeShape("remove_me")
    } else {
      us_proxy %>% removeMarker("remove_me")
    }
  })
  
  # reset map click focus
  observeEvent(input$us_map_fill, {
    st_sub$us_map_fill <- 
      if (st_sub$idx == "pop" & grepl("*_black$", input$us_map_fill)){
        gsub("*_black$", "_pop", input$us_map_fill)
      } else if (st_sub$idx == "black" & grepl("*_pop$", input$us_map_fill) &
                 !input$us_map_fill %in% colnames(ol$mwi[["black"]])){
        gsub("*_pop$", "_black", input$us_map_fill)
      } else {
        input$us_map_fill
      }
  })
  
  # update the focus
  observeEvent(input$us_map_shape_click, {
    if (!is.null(input$us_map_shape_click$id)){
      focus_info$hl <- T
      focus_info$ZCTA <- input$us_map_shape_click$id
      
      # remove any previously highlighted polygon
      if (!st_sub$is_all){
        us_proxy %>% removeShape("remove_me")
      } else {
        us_proxy %>% removeMarker("remove_me")
      }
      
      # add a highlighted polygon
      us_proxy <- plot_map(st_sub$us_map_fill, st_sub$geodat, 
                           st_sub$idx,
                           ol = ol,
                           is_all = st_sub$is_all,
                           add_poly = T, us_proxy = us_proxy, 
                           zcta_choose = focus_info$ZCTA)
    } else {
      focus_info$hl <- F
      focus_info$ZCTA <- ""
      
      # remove any previously highlighted polygon
      if (!st_sub$is_all){
        us_proxy %>% removeShape("remove_me")
      } else {
        us_proxy %>% removeMarker("remove_me")
      }
    }
  })
  
  observeEvent(input$zip_choose, {
    if (input$zip_choose != "" & 
        nchar(input$zip_choose) == 5 &
        !grepl("\\D", input$zip_choose) & # only numbers
        input$zip_choose %in% names(zip_to_zcta) & # a valid zcta
        zip_to_zcta[input$zip_choose] %in% st_sub$geodat$GEOID # in the state
    ){
      focus_info$hl <- T
      focus_info$ZCTA <- unname(zip_to_zcta[input$zip_choose])
      
      # remove any previously highlighted polygon
      if (!st_sub$is_all){
        us_proxy %>% removeShape("remove_me")
      } else {
        us_proxy %>% removeMarker("remove_me")
      }
      
      # add a highlighted polygon
      us_proxy <- plot_map(st_sub$us_map_fill, st_sub$geodat, 
                           st_sub$idx,
                           ol = ol,
                           is_all = st_sub$is_all,
                           add_poly = T, us_proxy = us_proxy, 
                           zcta_choose = focus_info$ZCTA)
    } else {
      focus_info$hl <- F
      focus_info$ZCTA <- ""
      
      # remove any previously highlighted polygon
      if (!st_sub$is_all){
        us_proxy %>% removeShape("remove_me")
      } else {
        us_proxy %>% removeMarker("remove_me")
      }
    }
  })
  
  # observe button inputs and clicks: community view ----
  
  # update measures when the population type changes
  observeEvent(input$idx_type_com, {
    idx <- input$idx_type
    
    fill <- 
      if (idx == "pop" & grepl("*_black$", input$com_map_fill)){
        gsub("*_black$", "_pop", input$com_map_fill)
      } else if (idx == "black" & grepl("*_pop$", input$com_map_fill) &
                 !input$com_map_fill %in% colnames(ol$mwi[["black"]])){
        gsub("*_pop$", "_black", input$com_map_fill)
      } else {
        input$com_map_fill
      }
    
    updateSelectInput(
      session = session,
      "com_map_fill",
      "What would you like to explore?",
      choices = ol$avail_meas_list[[idx]],
      selected = fill
    )
  })
  
  # update the data based on state or population change
  observeEvent(c(input$idx_type_com, input$zip_choose_com), {
    # save both of these to evaluate if anything's changed
    idx <- input$idx_type_com
    orig_zcta <- com_sub$ZCTA
    
    # check that the entered zip is accurate
    if (input$zip_choose_com != "" & 
        nchar(input$zip_choose_com) == 5 &
        !grepl("\\D", input$zip_choose_com) & # only numbers
        input$zip_choose_com %in% names(zip_to_zcta) # a valid zcta
    ){
      # don't change it otherwsie
      com_sub$ZCTA <- unname(zip_to_zcta[input$zip_choose_com])
    } 
    
    if (com_sub$idx != idx | com_sub$ZCTA != orig_zcta){
      com_sub$idx <- idx
      
      # get coordinates to know the total
      all_coord <- st_coordinates(ol$geopts[[idx]])
      zcta_coord <- 
        st_coordinates(ol$geopts$pop[ol$geopts$pop$GEOID == com_sub$ZCTA,])
      
      # get surrounding community
      zcta_log <- all_coord[,1] >=
        zcta_coord[1] - 1 &
        all_coord[,1] <=
        zcta_coord[1] + 1 &
        all_coord[,2] >=
        zcta_coord[2] - 1 &
        all_coord[,2] <=
        zcta_coord[2] + 1 
      
      if (sum(zcta_log) <= 1){ # some zctas are very far apart (alaska)
        zcta_log <- all_coord[,1] >=
          zcta_coord[1] - 3 &
          all_coord[,1] <=
          zcta_coord[1] + 3 &
          all_coord[,2] >=
          zcta_coord[2] - 3 &
          all_coord[,2] <=
          zcta_coord[2] + 3 
      }
      
      # get within coordinates
      com_sub$geodat <- ol$geodat[[idx]][zcta_log,]
      com_sub$mwi <- ol$mwi[[idx]][ol$mwi[[idx]]$ZCTA %in% 
                                  ol$geodat[[idx]]$GEOID[zcta_log],]
      
      com_sub$com_map_fill <- 
        if (com_sub$idx == "pop" & grepl("*_black$", input$com_map_fill)){
          gsub("*_black$", "_pop", input$com_map_fill)
        } else if (com_sub$idx == "black" & grepl("*_pop$", input$com_map_fill) &
                   !input$com_map_fill %in% colnames(ol$mwi[["black"]])){
          gsub("*_pop$", "_black", input$com_map_fill)
        } else {
          input$com_map_fill
        }
    }
  })
  
  
  # reset map click focus
  observeEvent(input$com_map_fill, {
    com_sub$com_map_fill <- 
      if (com_sub$idx == "pop" & grepl("*_black$", input$com_map_fill)){
        gsub("_black", "_pop", input$com_map_fill)
      } else if (com_sub$idx == "black" & grepl("*_pop$", input$com_map_fill) &
                 !input$com_map_fill %in% colnames(ol$mwi[["black"]])){
        gsub("*_pop$", "_black", input$com_map_fill)
      } else {
        input$com_map_fill
      }
  })
  
  # output plots and information: state view ----
  
  # watch focus_info updating
  output$focus_on <- reactive({
    focus_info$ZCTA != ""
  })
  outputOptions(output, "focus_on", suspendWhenHidden = FALSE)
  
  output$data_info <- renderUI({
    withProgress(message = "Rendering data information", {
      full_name <- ol$measure_to_names[[st_sub$idx]][st_sub$us_map_fill]
      
      HTML(paste0(
        "<font size = '2'>",
        
        "A variety of data sources are used for measures comprising the Mental Wellness Index. The data currently pictured for ",
        full_name,
        " came from ",
        ifelse(full_name == "Mental Wellness Index",
               "",
               ol$info_dat[st_sub$us_map_fill, "Years"]
        ),
        " ",
        ifelse(full_name == "Mental Wellness Index",
               "various sources of",
               ol$info_dat[st_sub$us_map_fill, "Source"]
        ),
        " data.<p><p>",
        ifelse(
          full_name == "Mental Wellness Index",
          "",
          paste0(
            full_name, " Description: ",
            ol$info_dat[st_sub$us_map_fill, "Measure.Description"]
          )),
        "<p>",
        "</font>"
      ))
    })
  })
  
  # plot map based on fill
  output$us_map <- renderLeaflet({
    withProgress(message = "Rendering map", {
      us_proxy <- plot_map(st_sub$us_map_fill, st_sub$geodat,
                           st_sub$idx, ol, is_all = st_sub$is_all)
      
      if (focus_info$hl){
        # add a highlighted polygon
        us_proxy <- plot_map(st_sub$us_map_fill, st_sub$geodat, 
                             st_sub$idx, ol, 
                             is_all = st_sub$is_all,
                             add_poly = T, us_proxy = us_proxy, 
                             zcta_choose = focus_info$ZCTA)
      }
      
      us_proxy
    })
  })
  
  # put a legend
  output$us_map_legend <- output$com_map_legend <- renderUI({
    withProgress(message = "Rendering map legend", {
      HTML(paste0(
        "<center>",
        paste(
          sapply(1:length(meas_max_colors), function(x){
            if (names(meas_max_colors[x]) != "Mental Wellness Index"){
              paste0("<font color = ", meas_max_colors[x], " size = '3'><b>", 
                     names(meas_max_colors[x]), "</font></b>")
            } else {
              paste(
                sapply(1:nchar(names(meas_max_colors[x])), function(y){
                  bchar <- strsplit(names(meas_max_colors[x]), "")[[1]][y]
                  paste0(
                    "<font color = ", 
                    meas_colors_pal$`Mental Wellness Index`(nchar("Mental Wellness Index"))[y],
                    " size = '3'><b>", 
                    bchar, "</font></b>")
                }),
                collapse = ""
              )
            }
          }),
          collapse = "<font size = 3'><b> | </b></font>"
        ),
        "</center>"
      ))
    })
  })
  
  # put an explanation, no zcta selected
  output$us_map_expl <- renderUI({
    withProgress(message = "Rendering map explanation", {
      full_name <- ol$measure_to_names[[st_sub$idx]][st_sub$us_map_fill]
      mc <- meas_max_colors[ol$meas_col_to_type[full_name]]
      lc <- if (st_sub$us_map_fill == "Mental_Wellness_Index"){
        meas_min_colors[ol$meas_col_to_type[full_name]]
      } else {
        mc
      }
      
      # get measure directionality
      dir_val <- if (st_sub$us_map_fill != "Mental_Wellness_Index"){
        ol$info_dat[st_sub$us_map_fill, "Directionality"]
      } else {
        1
      }
      
      if (st_sub$us_map_fill == "Mental_Wellness_Index"){
        text <- paste0(
          "A ", 
          html_color(mc, "higher"),
          " value indicates more ",
          html_color(mc, "assets"),
          " supporting ", 
          html_color(mc, paste("mental wellness")),
          "."
        )
        
        text <- paste0(
          text,
          " A ",
          html_color(lc, "lower"),
          " value indicates more ",
          html_color(lc, "obstacles"),
          " to ",
          html_color(lc, "mental wellness"),
          "."
        )
      } else {
        text <- paste0(
          "A ", 
          html_color(mc, "higher"),
          " value indicates a ",
          html_color(mc, "higher"),
          " national ", ifelse(st_sub$us_map_fill != "Mental_Wellness_Index", "ranking", "value"), "  for ", 
          html_color(mc, full_name),
          ".",
          "</font></b><p></p>",
          "<font size = '2'>",
          "<i>",
          ifelse(
            dir_val == -1, 
            "Note: Measures with a higher rank (closer to 100) indicate more community-level <b>assets</b> or <b>obstacles</b> (indicated with *) to mental wellness, based on their respective directionality. This measure was designated as an obstacle when calculating the MWI. ",
            ""
          ),
          "</i>",
          "</font>"
        )
      }
      
      HTML(paste0(
        "<center>",
        "<font size = '3'><b>",
        text,
        "</b></font>",
        "</center>"
      ))
    })
  })
  
  output$us_distr_title <- renderUI({
    HTML(paste0(
      "<b><center><font size = '3'>",
      "Distribution of ", ol$measure_to_names[[st_sub$idx]][st_sub$us_map_fill],
      " for ",
      ifelse(st_sub$idx != "black", "the ", ""),
      ifelse(st_sub$idx == "black", "Black ", "Overall "),
      "Population",
      ifelse(st_sub$idx == "black", "s", ""),
      " in ",
      st_sub$st,
      "</b></center></font>"
    ))
  })
  
  output$us_distr <- renderPlotly({
    withProgress(message = "Rendering ZCTA data distribution", {
      plot_bee_distr(st_sub$us_map_fill, 
                     st = st_sub$st,
                     mwi = st_sub$mwi,
                     idx = st_sub$idx, 
                     ol = ol,
                     is_all = st_sub$is_all,
                     hl = focus_info$hl, 
                     zcta_hl = focus_info$ZCTA)
    })
  })
  
  output$us_quantile <- renderTable({
    withProgress(message = "Rendering ZCTA data quantiles", {
      dist_fill <- summary(st_sub$mwi[,st_sub$us_map_fill], digits = 4)
      
      data.frame("Quantiles" = names(dist_fill), "Values" = c(dist_fill))
    })
  },  
  striped = TRUE,  
  align = 'c',  
  digits = 3,
  width = "100%")
  
  output$us_info <- renderUI({
    withProgress(message = "Rendering ZCTA data distribution explanation", {
      if (focus_info$ZCTA != ""){
        full_name <- ol$measure_to_names[[st_sub$idx]][st_sub$us_map_fill]
        
        # get measure directionality
        dir_val <- if (st_sub$us_map_fill != "Mental_Wellness_Index"){
          ol$info_dat[st_sub$us_map_fill, "Directionality"]
        } else {
          1
        }
        
        # get scores
        dir_df <-
        if (st_sub$us_map_fill == "Mental_Wellness_Index"){
          st_sub$mwi
        } else {
          ol$no_dir_perc_meas_df[st_sub$mwi$ZCTA,]
        }
        overall_df <- 
          if (st_sub$us_map_fill == "Mental_Wellness_Index"){
            ol$mwi[[st_sub$idx]]
          } else {
            ol$no_dir_perc_meas_df
          }
        
        f_val <- 
          dir_df[
            dir_df$ZCTA == focus_info$ZCTA, st_sub$us_map_fill]
        all_st_val <- dir_df[, st_sub$us_map_fill]
        all_us_val <- overall_df[, st_sub$us_map_fill]
        
        # get colors
        if (st_sub$us_map_fill == "Mental_Wellness_Index"){
          mc <- 
            if (is.na(f_val) | f_val >= 50){
              meas_max_colors[ol$meas_col_to_type[full_name]]
            } else {
              meas_min_colors[ol$meas_col_to_type[full_name]]
            }
        } else {
          mc <- meas_max_colors[ol$meas_col_to_type[full_name]]
        }
        
        if (!is.na(f_val)){
          
          # get percentiles relative to state and us
          st_perc <- trunc(ecdf(all_st_val)(f_val)*100)
          us_perc <- trunc(ecdf(all_us_val)(f_val)*100)
          
          # get text value for percentile: very low/high, low/high
          st_comp <- quant_map(st_perc)
          us_comp <- quant_map(us_perc)
          
          HTML(paste0(
            "<center>",
            "<b><font size = '3'>",
            "ZCTA ", html_color(mc, focus_info$ZCTA),
            " (ZIP Code",
            ifelse(nchar(zcta_to_zip[focus_info$ZCTA]) > 5, "s "," "), 
            html_color(mc, zcta_to_zip[focus_info$ZCTA]), ")",
            " has a ", html_color(mc, full_name), " national ", ifelse(st_sub$us_map_fill != "Mental_Wellness_Index", "ranking", "value"), "  of ",
            html_color(mc, trunc(f_val)),
            " and is at the ",
            html_color(mc, st_perc), 
            " percentile for the state.",
            " This is in the ", 
            html_color(mc, st_comp), " relative to ",
            input$st_focus,", and in the ",
            html_color(mc, us_comp), " relative to the United States.",
            "</font></b><p></p>",
            "<font size = '2'>",
            "<i>A higher value indicates a higher national ", ifelse(st_sub$us_map_fill != "Mental_Wellness_Index", "ranking", "value"), "  for ",
            full_name,
            ". ",
            ifelse(
              dir_val == -1, 
              "Note: Measures with a higher rank (closer to 100) indicate more community-level <b>assets</b> or <b>obstacles</b> (indicated with *) to mental wellness, based on their respective directionality. This measure was designated as an obstacle when calculating the MWI. ",
              ""
            ),
            "</i>",
            "</font>",
            "</center>"
          ))
        } else {
          HTML(paste0(
            "<center>",
            "<b><font size = '3'>",
            "ZCTA ", html_color(mc, focus_info$ZCTA),
            " (ZIP Code",
            ifelse(nchar(zcta_to_zip[focus_info$ZCTA]) > 5, "s "," "), 
            html_color(mc, zcta_to_zip[focus_info$ZCTA]), ")",
            " does not have a value for ", 
            html_color(mc, full_name),
            ", indicating missing data or no population in this area.",
            "</font></b><p></p>",
            "<font size = '2'>",
            "<i>A higher value indicates a higher national ", ifelse(st_sub$us_map_fill != "Mental_Wellness_Index", "ranking", "value"), "  for ",
            full_name,
            ". ",
            ifelse(
              dir_val == -1, 
              "Note: Measures with a higher rank (closer to 100) indicate more community-level <b>assets</b> or <b>obstacles</b> (indicated with *) to mental wellness, based on their respective directionality. This measure was designated as an obstacle when calculating the MWI. ",
              ""
            ),
            "</i>",
            "</font>",
            "</center>"
          ))
        }
      }
    })
  })
  # output plots and information: ZIP code view ----
  
  output$data_info_com <- renderUI({
    withProgress(message = "Rendering data information", {
      full_name <- ol$measure_to_names[[com_sub$idx]][com_sub$com_map_fill]
      
      HTML(paste0(
        "<font size = '2'>",
        
        "A variety of data sources are used for measures comprising the Mental Wellness Index. The data currently pictured for ",
        full_name,
        " came from ",
        ifelse(full_name == "Mental Wellness Index",
               "",
               ol$info_dat[com_sub$com_map_fill, "Years"]
        ),
        " ",
        ifelse(full_name == "Mental Wellness Index",
               "various sources of",
               ol$info_dat[com_sub$com_map_fill, "Source"]
        ),
        " data.<p><p>",
        ifelse(
          full_name == "Mental Wellness Index",
          "",
          paste0(
            full_name, " Description: ",
            ol$info_dat[com_sub$com_map_fill, "Measure.Description"]
          )),
        "<p>",
        "</font>"
      ))
    })
  })
  
  # community map
  output$com_map <- renderLeaflet({
    withProgress(message = "Rendering map", {
      plot_map(com_sub$com_map_fill, com_sub$geodat,
               com_sub$idx,  ol, is_all = F,
               is_com = T, zcta_choose = com_sub$ZCTA)
    })
  })
  
  # put an community map explanation
  output$com_map_expl <- renderUI({
    withProgress(message = "Rendering map explanation", {
      full_name <- ol$measure_to_names[[com_sub$idx]][com_sub$com_map_fill]
      mc <- meas_max_colors[ol$meas_col_to_type[full_name]]
      lc <- meas_min_colors[ol$meas_col_to_type[full_name]]
      
      # add ZCTA interpretation
      # get measure directionality
      dir_val <- if (com_sub$com_map_fill != "Mental_Wellness_Index"){
        ol$info_dat[com_sub$com_map_fill, "Directionality"]
      } else {
        1
      }
      
      # get scores
      dir_df <-
        if (com_sub$com_map_fill == "Mental_Wellness_Index"){
          com_sub$mwi
        } else {
          ol$no_dir_perc_meas_df[com_sub$mwi$ZCTA,]
        }
      overall_df <- 
        if (com_sub$com_map_fill == "Mental_Wellness_Index"){
          ol$mwi[[com_sub$idx]]
        } else {
          ol$no_dir_perc_meas_df
        }
      
      f_val <- 
        dir_df[
          dir_df$ZCTA == com_sub$ZCTA, com_sub$com_map_fill]
      all_com_val <- dir_df[, com_sub$com_map_fill]
      all_us_val <- overall_df[, com_sub$com_map_fill]
      
      # get colors
      if (com_sub$com_map_fill == "Mental_Wellness_Index"){
        mc <- 
          if (is.na(f_val) | f_val >= 50){
            meas_max_colors[ol$meas_col_to_type[full_name]]
          } else {
            meas_min_colors[ol$meas_col_to_type[full_name]]
          }
      } else {
        mc <- meas_max_colors[ol$meas_col_to_type[full_name]]
      }
      
      if (!is.na(f_val)){
        
        # get percentiles relative to state and us
        com_perc <- trunc(ecdf(all_com_val)(f_val)*100)
        us_perc <- trunc(ecdf(all_us_val)(f_val)*100)
        
        # get text value for percentile: very low/high, low/high
        com_comp <- quant_map(com_perc)
        us_comp <- quant_map(us_perc)
        
        text_2 <- HTML(paste0(
          "<center><b><font size = '3'>",
          "ZCTA ", html_color(mc, com_sub$ZCTA),
          " (ZIP Code",
          ifelse(nchar(zcta_to_zip[com_sub$ZCTA]) > 5, "s "," "), 
          html_color(mc, zcta_to_zip[com_sub$ZCTA]), ")",
          " has a ", html_color(mc, full_name), " national ", ifelse(com_sub$com_map_fill != "Mental_Wellness_Index", "ranking", "value"), "  of ",
          html_color(mc, trunc(f_val)),
          " and is at the ",
          html_color(mc, com_perc), 
          " percentile for the state.",
          " This is in the ", 
          html_color(mc, com_comp), 
          " relative to the selected community, and in the ",
          html_color(mc, us_comp), " relative to the United States.",
          "</b>",
          ifelse(
            full_name != "Mental Wellness Index",
            paste0(
              "<p></p>",
              "<font size = '2'>",
              "<i>A higher value indicates a higher national ", ifelse(com_sub$com_map_fill != "Mental_Wellness_Index", "ranking", "value"), "  for ",
              full_name,
              ". ",
              ifelse(
                dir_val == -1, 
                "Note: Measures with a higher rank (closer to 100) indicate more community-level <b>assets</b> or <b>obstacles</b> (indicated with *) to mental wellness, based on their respective directionality. This measure was designated as an obstacle when calculating the MWI. ",
                ""
              ),
              "</i>",
              "</font>"
            ),
            ""
          ),
          "</font></center>"
        ))
      } else {
        text_2 <-
          HTML(paste0(
            "<center><b><font size = '3'>",
            "ZCTA ", html_color(mc, com_sub$ZCTA),
            " (ZIP Code",
            ifelse(nchar(zcta_to_zip[com_sub$ZCTA]) > 5, "s "," "), 
            html_color(mc, zcta_to_zip[com_sub$ZCTA]), ")",
            " does not have a value for ", 
            html_color(mc, full_name),
            ", indicating missing data or no population in this area.",
            "</b>",
            ifelse(
              full_name != "Mental Wellness Index",
              paste0(
                "<p></p>",
                "<font size = '2'>",
                "<i>A higher value indicates a higher national ", ifelse(com_sub$com_map_fill != "Mental_Wellness_Index", "ranking", "value"), "  for ",
                full_name,
                ". ",
                ifelse(
                  dir_val == -1, 
                  "Note: Measures with a higher rank (closer to 100) indicate more community-level <b>assets</b> or <b>obstacles</b> (indicated with *) to mental wellness, based on their respective directionality. This measure was designated as an obstacle when calculating the MWI. ",
                  ""
                ),
                "</i>",
                "</font>"
              ),
              ""
            ),
            "</font></center>"
          ))
      }
      
      HTML(paste0(
        "<center>",
        "<font size = '3'>",
        # text,
        # "<p><p>",
        text_2,
        "</font>",
        "</center>"
      ))
    })
  })
  
  # above the "table" version, add text for the MWI
  output$com_report_card_table_mwi <- renderUI({
    mwi_zcta <- com_sub$mwi[com_sub$mwi$ZCTA == com_sub$ZCTA, , drop = F]
    
    dn <- "Mental Wellness Index"
    mc <- meas_max_colors[dn]
    text_mwi <- paste0(
      "<b><font size = '3'>",
      html_color(
        ifelse(trunc(mwi_zcta[1, "Mental_Wellness_Index"]) >= 50,
               mc, meas_min_colors[dn]),
        dn),
      ifelse(dn == "Mental Wellness Index", 
             paste0(": ", trunc(mwi_zcta[1, "Mental_Wellness_Index"])),
             ""),
      "</b></font>",
      "<br>"
    )
    
    HTML(paste0(
      "<p><b><font size = '3'>",
      "ZCTA: ", com_sub$ZCTA,
      "</b></font></p>",
      text_mwi))
  })
  
  # put a "report card" for the community (in TABLE format)
  output$com_report_card_table <- renderDataTable({
    
    # Create Report Card Table
    reportcard <- ol$m_reg[,c("Measure", "Measure Description", "Category", "Directionality")]
    
    # Add column of measure values based on selected ZCTA and index type (Pop/Black)
    Rank <- t(ol$no_dir_perc_meas_df[com_sub$ZCTA,
                                     ol$avail_measures[[com_sub$idx]][-1]])
    colnames(Rank) <- "Rank"
    rownames(Rank) <- gsub("_pop$", "", rownames(Rank))
    
    # similarly, add the values
    val <- t(ol$meas_df[com_sub$ZCTA,
                        ol$avail_measures[[com_sub$idx]][-1]])
    colnames(val) <- "Value"
    rownames(val) <- gsub("_pop$", "", rownames(val))
    
    # add values to report card
    reportcard[rownames(val), "Value"] <- val
    reportcard <- reportcard[!is.na(reportcard$Measure),]
    
    reportcard <- merge(reportcard, Rank, by = "row.names",sort = FALSE) %>%
      as.data.frame() %>%
      mutate(Directionality = ifelse(Directionality == -1, "Obstacle", "Asset"),
             Rank = as.numeric(round(Rank)),
             Value = as.numeric(format(as.numeric(round(Value, 2)), scientific = F)),
             )  %>%
      select(Measure, Rank, Value, `Measure Description`, Category, Directionality)
    
    rownames(reportcard) <- NULL
    
    # measure_formatter <- formatter(
    #   "span",
    #   style = x ~ icontext(
    #     ifelse(x == com_sub$com_map_fill,
    #            "star",
    #            ""),
    #     x))
    # category_formatter <- formatter(
    #   "span",
    #   style = x ~ style(
    #     color = ifelse(x == "Healthcare Access", 
    #                    meas_max_colors[2],
    #                    ifelse(x == "Health Status", 
    #                           meas_max_colors[3],
    #                           meas_max_colors[1])
    #     )
    #   )
    # )
    
    datatable(
      # formattable(
        reportcard, 
                  # list(Measure = measure_formatter,
                  #      Category = category_formatter
                  # ),
                  # table.attr = 'style="font-size: 16px;";\"'), 
      rownames = F,
      options = list(
        "pageLength" = nrow(reportcard), # show all
        columnDefs = list(list(width = c('250px'), targets = c(4))) # wrap column descriptions
      )
    )
  })

  # put a "report card" for the community
  output$com_map_report_card <- renderUI({
    mwi_zcta <- com_sub$mwi[com_sub$mwi$ZCTA == com_sub$ZCTA, , drop = F]
    
    dn <- "Mental Wellness Index"
    mc <- meas_max_colors[dn]
    text_mwi <- paste0(
      "<b><font size = '3'>",
      html_color(
        ifelse(trunc(mwi_zcta[1, "Mental_Wellness_Index"]) >= 50,
               mc, meas_min_colors[dn]),
        dn),
      ifelse(dn == "Mental Wellness Index", 
             paste0(": ", trunc(mwi_zcta[1, "Mental_Wellness_Index"])),
             ""),
      "</b></font>",
      "<br>"
    )
    mwi_zcta <- ol$no_dir_perc_meas_df[com_sub$ZCTA, , drop = F]
    
    text <- ""
    for (dn in names(ol$avail_meas_list[[com_sub$idx]])){
      mc <- meas_max_colors[dn]
      
      if (dn != "Mental Wellness Index"){
        text <- paste0(
          text,
          "<b><font size = '3'>",
          html_color(mc, dn),
          ifelse(dn == "Mental Wellness Index", 
                 paste0(": ", trunc(mwi_zcta[1, "Mental_Wellness_Index"])),
                 ""),
          "</b></font>",
          "<br>"
        )
        
        for (cn in ol$avail_meas_list[[com_sub$idx]][[dn]]){
          # get measure directionality
          dir_val <- ol$info_dat[cn, "Directionality"]
          
          text <- paste0(
            text, 
            "<b>",
            html_color(
              mc, 
              paste0(
                # ifelse(ol$info_dat[cn, "Directionality"] > 0, "Higher ", "Lower "),
                ol$measure_to_names[[com_sub$idx]][cn])),
            ifelse(dir_val == -1, "*", ""),
            ": ",
            "</b>",
            trunc(mwi_zcta[1, cn]),
            "<br>"
          )
        }
        
        text <- paste0(
          text,
          "</p>",
          "<p>"
        )
      }
    }
    text_meas <- text
    
    HTML(paste0(
      "<font size = '2'>",
      "<i>A higher MWI value indicates more assets supporting mental wellness. A lower MWI value indicates more obstacles to mental wellness. Range from 0 to 100.</i>",
      "<p></p>",
      text_mwi,
      "<hr/>",
      "<font size = '3'><b>Measure Rankings:</b></font>",
      "<i><p>Range from 0 to 100. Measures with a higher rank (closer to 100) indicate more community-level <b>assets</b> or <b>obstacles</b> (indicated with *) to mental wellness, based on their respective directionality. See \"Explore ZCTA Measures\" for an interactive table with measure values.</p></i>",
      "<p></p>",
      text_meas,
      
      "</font>"
    ))
  })
  
  # observe and create custom MWI ----
  
  # preallocate download for custom processing
  overall_list <- reactiveVal()
  upd_weights <- reactiveVal(sub_m) # custom weight dataframe
  button_click <- reactiveValues( # tracking which custom button clicked
    "go" = 0,
    "weights" = 0,
    "comp" = 0
  )
  # STOP HERE: keeping track of which button clicked
  # also make weight processing WAY more efficient, you can do it
  
  # output for the customizing just the weights
  output$custom_mwi_weights <- renderDT(
    {
      datatable(upd_weights(), 
                rownames = F,
                options = list(pageLength = nrow(m_reg)),
                editable = list(target = "cell", 
                                disable = list(columns = c(0:2))))
    })
  
  # make table updates persistent
  observeEvent(input$custom_mwi_weights_cell_edit, {
    df <- upd_weights()
    row  <- input$custom_mwi_weights_cell_edit$row
    clmn <- input$custom_mwi_weights_cell_edit$col
    df[row, clmn+1] <- input$custom_mwi_weights_cell_edit$value
    upd_weights(df)
  })
  
  # download the Metadata file
  output$download_metadata <- output$download_metadata_comp <- downloadHandler(
    filename = function(){
      "Metadata.xlsx"
    },
    content = function(file){
      desc <- as.data.frame(
        read_excel(file.path(data_folder, "Metadata.xlsx"), sheet = 2)
      )
      
      write_xlsx(
        list(
          "Measure Registry" = m_reg,
          "Column Descriptions" = desc
        ), 
        file)
    }
  )
  
  observeEvent(c(input$custom_mwi_go, input$custom_mwi_go_comp, input$custom_mwi_go_weights), {
    # require one button to be pressed at least
    req(isTruthy(input$custom_mwi_go) || isTruthy(input$custom_mwi_go_comp) ||
          isTruthy(input$custom_mwi_go_weights))
    
    # check which button got pressed
    press <- if (input$custom_mwi_go[1] > button_click$go){
      "go"
    } else if (input$custom_mwi_go_comp[1] > button_click$comp){
      "comp"
    } else {
      "weights"
    }
    
    # update the saved values
    button_click$go <- input$custom_mwi_go[1]
    button_click$comp <- input$custom_mwi_go_comp[1]
    button_click$weights <- input$custom_mwi_go_weights[1]
    
    withProgress(
      message = "Creating custom Mental Wellness Index!", 
      detail = "Loading data...", {
        source(file.path("Processing_Pipeline", "pipeline_driver.R"))
        
        # start by checking files
        error <- F
        zip_true <- T
        # only look for a zip file for custom data
        if (press != "weights"){
          if (!is.null(input$custom_zip)){
            zip_df <- input$custom_zip # a list of files
          } else if (!is.null(input$custom_zip_comp)) {
            zip_df <- input$custom_zip_comp # a list of files
          } else {
            zip_true <- F
            error <- T
            output$custom_error <- output$custom_error_comp <- renderText({
              paste0("ERROR: Please upload files as described above.")
            })
          }
        }
        
        # for a subset list
        z_enter <- c()
        
        # for custom data
        if (!error){
          if (press != "weights"){
            # check that metadata is in the list, that there's something in it,
            # that the rest of the data is csv
            if (zip_true &
                "Metadata.xlsx" %in% zip_df$name & 
                all((endsWith(tolower(zip_df$datapath), ".xlsx")) |
                    (endsWith(tolower(zip_df$datapath), ".csv"))) & 
                (nrow(zip_df) == 1 |
                 (nrow(zip_df) > 1 &
                  all(endsWith(tolower(zip_df$name[zip_df$name != "Metadata.xlsx"]),
                               ".csv"))))){
              # read the metadata file
              m_reg_custom <- as.data.frame(
                read_xlsx(zip_df$datapath[zip_df$name == "Metadata.xlsx"], sheet = 1)
              )
              
              # read all custom data
              custom_data <-  lapply(
                zip_df$datapath[zip_df$name != "Metadata.xlsx"], function(x){
                  read.csv(x, check.names = F)
                }
              )
              names(custom_data) <- zip_df$name[zip_df$name != "Metadata.xlsx"]
              
            } else {
              error <- T
              output$custom_error <- output$custom_error_comp <-
                output$custom_error_weights <- renderText({
                  paste0(
                    "ERROR: One of the following is wrong with your uploaded files:\n",
                    "1: Does not contain Metadata.xlsx\n",
                    "2: Has additional custom data and custom data not in CSV format\n",
                    "3: There are no files\n",
                    "4: Additional files uploaded that are not CSV or XLSX format\n",
                    "Please fix the above, reupload, and re-do.")
                })
            }
          } else {
            # for custom weights
            m_reg_custom <- m_reg
            m_reg_custom[rownames(upd_weights()), "Weights"] <- 
              upd_weights()[,"Updated Weights"]
            
            # we also want to include specific ZCTAs
            z_enter <- unlist(strsplit(input$custom_mwi_zips, "\n"))
            z_enter <- unique(str_pad(z_enter, 5, pad = "0"))
            
            # if zip codes, change to ZCTAs
            if (input$custom_mwi_zip_choice){
              z_enter <- zip_to_zcta[z_enter]
            }
            
            z_enter <- z_enter[!is.na(z_enter)]
            
            custom_data <- list()
          }
        }
        
        if (!error){
          incProgress(detail = "Running pipeline...")
          
          # now pass it into the pipeline file
          overall_out <- tryCatch({
            mwi_pipeline(m_reg_custom, custom_data, run_custom = T, 
                         z_enter = z_enter)
          }, 
          error = function(cond){
            error <- T 
            output$custom_error <- output$custom_error_comp <-
              output$custom_error_weights <- renderText({
                paste0(
                  "ERROR: The pipeline returned the following error:\n",
                  cond, "\n",
                  "Please doublecheck your custom data and the metadata file according to the parameters given above, reupload, and re-do."
                )
              })
            
            return(list())
          })
        }
        
        if (!error){
          incProgress(detail = "Processing app data...")
          
          # need to also all preceding data, if the list came out correctly
          if (length(overall_out) > 0){
            overall_out$mwi <- list()
            overall_out$mwi[["pop"]] <- overall_out$pop_pm
            # remove any empty zcta rows (miswriting?) -- TODO: fix in pipeline
            overall_out$mwi[["pop"]] <-
              overall_out$mwi[["pop"]][overall_out$mwi[["pop"]]$ZCTA != "",]
            overall_out$mwi[["black"]] <- overall_out$black_pm
            # remove any empty zcta rows (miswriting?) -- TODO: fix in pipeline
            overall_out$mwi[["black"]] <-
              overall_out$mwi[["black"]][overall_out$mwi[["black"]]$ZCTA != "",]
            
            ap <- suppressWarnings(
              app_preprocess(overall_out$m_reg, 
                             overall_out$info_dat, overall_out$mwi,
                             app_start = F)
            )
            
            # add counties/states to mwi
            for (idx in index_types){
              overall_out$mwi[[idx]][, colnames(cty_cw)[-1]] <- 
                cty_cw[overall_out$mwi[[idx]]$ZCTA, -1]
            }
            
            for (a in names(ap)){
              overall_out[[a]] <- ap[[a]]
            }
            
            incProgress(detail = "Saving results...")
            
            # keep in reactive for download
            overall_list(overall_out)
            
            output$custom_error <- output$custom_error_comp <- 
              output$custom_error_weights <- renderText({
                paste0("Complete! Click 'Download Custom MWI' to download. Upload resulting .RData on the 'Custom MWI Upload' section of the 'Explore States' or 'Explore ZIP Codes' page to explore.")
              })
          }
        }
      })
  })
  
  
  # download the output file
  output$download_custom_mwi <- output$download_custom_mwi_comp <- 
    output$download_custom_mwi_weights <- 
    downloadHandler(
      filename = function(){
        paste0("Custom_MWI_", Sys.Date(), ".RData")
      },
      content = function(file){
        withProgress(
          message = "Creating Custom MWI file for download...",{ 
            overall_output <- overall_list()
            
            save(overall_output, file = file)
          })
      }
    )
}

# RUN ----

shinyApp(ui, server)
