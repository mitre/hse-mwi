# Mental Wellness Index Tool
# By HSE Team
# Originated on: 10/20/2021

# NOTE: Styling by Sarah Ober of Case Study for using health equity framework
# in population health team.

# load libraries ----

library(readxl)
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

# styling ----

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


# load data ----

# what indices are available?
index_types <- c("Population" = "pop",
                 "Black" = "black")

# folder where all the data and information for the pipeline is
data_folder <- file.path(
  gsub("\\\\","/", gsub("OneDrive - ","", Sys.getenv("OneDrive"))), 
  "Health and Social Equity - SJP - BHN Score Creation",
  "Data")

# load measure registry -- first sheet
m_reg <- as.data.frame(
  read_excel(file.path(data_folder, "Metadata.xlsx"), sheet = 1)
)
# remove everything that doesn't have a numerator
m_reg <- m_reg[!is.na(m_reg$Numerator),]
rownames(m_reg) <- m_reg$Numerator

# load index weighting/output
info_df <- read.csv(
  file.path(data_folder, "Cleaned", "HSE_MWI_Data_Information.csv"),
  row.names = "Numerator"
)

# load zip codes to zcta
zip_cw <- read.csv(
  file.path(data_folder, "Resources", "Zip_to_zcta_crosswalk_2020.csv"),
  colClasses = c(
    "ZIP_CODE" = "character",
    "ZCTA" = "character"
  ))
territories <- c("AS", "FM", "GU", "MH", "MP", "PW", "PR", "VI")
zip_cw <- zip_cw[!zip_cw$STATE %in% territories,]
zip_to_zcta <- setNames(zip_cw$ZCTA, zip_cw$ZIP_CODE)
zcta_to_zip <- aggregate(ZIP_CODE ~ ZCTA, data = zip_cw, 
                         FUN = function(x){paste(x, collapse = ", ")})
zcta_to_zip <- setNames(zcta_to_zip$ZIP_CODE, zcta_to_zip$ZCTA)

# load county cw for zcta state county mapping
county_cw <- read.csv(
  file.path(data_folder, "Resources", "zcta_county_rel_10.txt"),
  colClasses = c(
    "ZCTA5" = "character",
    "STATE" = "character",
    "COUNTY" = "character",
    "GEOID" = "character"
  ))
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

# plotting information ----

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
        gsub("_pop","",
             gsub("_black","",colnames(mwi[[idx]])[-c(1:2)])), "Measure"
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
  "#70ad47", # health status
  "#157ba7", # healthcare acess
  "#00441b" #"#70ad47" # MWI
)

#   sapply(1:length(meas_colors), function(x){
#   brewer.pal(3, meas_colors[x])[3]
# })
meas_min_colors <-  c(
  "#fcfbfd", # SDOH
  "#f7fcf5", # health status
  "#f7fbff", # healthcare acess
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

# add counties/states to mwi
for (idx in index_types){
  mwi[[idx]][, colnames(cty_cw)[-1]] <- 
    cty_cw[mwi[[idx]]$ZCTA, -1]
}

# get zip code data -- ONLY ORIGINAL
# NOTE: cb = T will download a generalized file
# zips <- zctas(cb = T)
# zips <- st_transform(zips, crs = "+proj=longlat +datum=WGS84")
# save(list = "zips", file = file.path(data_folder, "Resources", "ZCTAs_shapefile_US.RData"))
# 
# # load zip code data (should be much faster)
# load(file.path(data_folder, "Resources", "ZCTAs_shapefile_US.RData"))
# 
# # create the geo data for leaflet
# # NOTE: may want to do this ahead of time, if possible, when the base index is done
# geodat <- geopts <- list()
# for (idx in index_types){
#   geodat[[idx]] <-
#     geo_join(zips, mwi[[idx]], by_sp = "GEOID10", by_df = "ZCTA", how = "inner")
# 
#   # sort by state code and zcta
#   geodat[[idx]] <- geodat[[idx]][order(geodat[[idx]]$STATE,
#                                        geodat[[idx]]$GEOID10),]
# 
#   # convert to points for US visualization -- ignore warnings
#   geopts[[idx]] <- st_centroid(geodat[[idx]])
# }
# # saving for now, while things are stable
# save(list = c("geodat", "geopts"), file = file.path(data_folder, "Cleaned", "HSE_MWI_ZCTA_full_shapefile_US.RData"))

# load geodat data (should be much faster)
load(file.path(data_folder, "Cleaned", "HSE_MWI_ZCTA_full_shapefile_US.RData"))

# get available zctas -- both will have the same
avail_zctas <- geodat[["pop"]]$GEOID10
names(avail_zctas) <- paste0(geodat[["pop"]]$GEOID10, 
                             " (State: ", geodat[["pop"]]$STATE_NAME, ")")

# plot functions ----

# plot the overall map, filled by measure/score (LEAFLET)
plot_map <- function(fill, geodat, idx, is_all = F, is_com = F,
                     fill_opacity = .7,
                     add_poly = F, us_proxy = NA, zcta_choose = NA){
  # subset map for easy plotting
  gd_map <- geodat[,c(fill, "GEOID10", "STATE", "STATE_NAME", "geometry")]
  colnames(gd_map)[1] <- "Fill"
  
  # get rid of empty polygons
  gd_map <- gd_map[!is.na(gd_map$GEOID10),]
  
  # create palette
  pal <- colorNumeric(
    palette = meas_colors[[meas_col_to_type[measure_to_names[[idx]][fill]]]],
    domain = c(0, gd_map$Fill, 100),
    na.color = "transparent",
    reverse = ifelse(fill == "Score", T, F)
  )
  pal_wo_na <- colorNumeric(
    palette = meas_colors[[meas_col_to_type[measure_to_names[[idx]][fill]]]],
    domain = c(0, gd_map$Fill, 100),
    na.color=rgb(0,0,0,0),
    reverse = ifelse(fill == "Score", T, F)
  )
  
  # labels 
  full_name <- measure_to_names[[idx]][fill]
  if (full_name != "Mental Wellness Index"){
    full_name <- paste0(
      # ifelse(info_df[fill, "Directionality"] > 0, "Higher ", "Lower "), 
      full_name, 
      " Ranking"
    )
  }
  
  labels <- 
    paste0(
      "State: ", gd_map$STATE_NAME, "<br>",
      "ZCTA: ", gd_map$GEOID10, "<br>", 
      "ZIP Code: ", unname(zcta_to_zip[gd_map$GEOID10]), "<br>", 
      full_name,": ", trunc(gd_map$Fill)) %>%
    lapply(htmltools::HTML)
  
  # get initial zoom area
  bounds <- 
    if (!is_com){
      unname(st_bbox(geodat))
    } else { # community focuses on ZCTA
      unname(st_bbox(geodat[geodat$GEOID10 == zcta_choose,]))
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
                      layerId = ~GEOID10,
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
                    title = unname(full_name),
                    labFormat = function(type, cuts, p){
                      paste0(c("0 (More Obstacles)", 
                               "20", "40", "60",
                               "80", "100 (More Assets)"))
                    }) %>%
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
                           layerId = ~GEOID10,
                           label = labels,
                           radius = 5) %>%
          addLegend(pal = pal_wo_na,
                    values = ~c(0, Fill, 100), 
                    opacity = 0.7, 
                    position = "bottomright",
                    title = unname(full_name),
                    labFormat = function(type, cuts, p){
                      paste0(c("0 (More Challenges)", 
                               "<p align = 'left'>20<p>", "40", "60",
                               "80", "100 (More Assets)"))
                    }) %>%
          fitBounds(
            lng1 = bounds[1],
            lng2 = bounds[3],
            lat1 = bounds[2],
            lat2 = bounds[4]
          )
      }
    
    # if it's a community, we also want to focus on it
    if (is_com){
      zcta_select <- gd_map[gd_map$GEOID10 == zcta_choose,]
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
          label = labels[gd_map$GEOID10 == zcta_choose]
        )
    }
  } else {
    zcta_select <- gd_map[gd_map$GEOID10 == zcta_choose,]
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
            label = labels[gd_map$GEOID10 == zcta_choose]
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
            label = labels[gd_map$GEOID10 == zcta_choose],
            radius = 7)
      }
  }
  
  return(mp)
}

# plot a distribution of the fill value using a beeswarm plot (PLOTLY)
plot_bee_distr <- function(fill, st, mwi, idx, is_all = F, hl = F, zcta_hl = ""){
  bee.df <- data.frame(
    val = mwi[,fill],
    zcta = mwi$ZCTA,
    lab = rep("val", nrow(mwi)),
    focus = rep("val", nrow(mwi)),
    focus_alpha = rep(1, nrow(mwi))
  )
  # remove all the empty rows
  bee.df <- bee.df[complete.cases(bee.df),]
  
  # if we're going to highlight a point
  if (hl){
    row_hl <- which(bee.df$zcta == zcta_hl)
    bee.df$focus[row_hl] <- "Focus"
    bee.df$focus_alpha[-row_hl] <- .3
    
    pal <- colorNumeric(
      palette = meas_colors[[meas_col_to_type[measure_to_names[[idx]][fill]]]],
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
            meas_colors_pal[[meas_col_to_type[measure_to_names[[idx]][fill]]]](100),
          limits = c(0, 100)
        )+
        scale_size_manual(values = hl_size)
    } else {
      ggplot(bee.df, aes(lab, val, color = val), size = 1.5)+
        scale_color_gradientn(
          colors = 
            meas_colors_pal[[meas_col_to_type[measure_to_names[[idx]][fill]]]](100),
          limits = c(0, 100)
        )
    }
  
  full_name <- measure_to_names[[idx]][fill]
  if (full_name != "Mental Wellness Index"){
    full_name <- paste0(
      # ifelse(info_df[fill, "Directionality"] > 0, "Higher ", "Lower "), 
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
          groupOnX = T, alpha = bee.df$focus_alpha)
      } else {
        geom_violin(
          fill = meas_colors_pal[[meas_col_to_type[measure_to_names[[idx]][fill]]]](3)[2],
          color = meas_colors_pal[[meas_col_to_type[measure_to_names[[idx]][fill]]]](3)[2]
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
      windowTitle=HTML(paste0("Mental Wellness Index (TM) Tool"))
    ),
    style="display:none"
  ),
  
  navbarPage(
    title=div(
      a(
        href="https://www.mitre.org/",
        img(src="media/MITRE_logo.png", height="30"),
        target="blank",
      ),
      HTML(paste0("Mental Wellness Index",tags$sup("TM")," Tool")),
    ),
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
                selected = "Alabama"
              ),
              selectInput(
                "us_map_fill",
                "What would you like to explore?",
                choices = avail_meas_list[["pop"]]
              ),
              textInput(
                "zip_choose",
                label = "Which ZIP Code would you like to focus on in the selected state?",
                placeholder = "e.g. 35004, 00501, 20041, etc."
              ),
              actionButton("reset_zcta_click", "Reset ZIP Code Focus")
            ),
            bsCollapsePanel(
              "About Selected Measure",
              uiOutput("data_info"),
              HTML(paste0(
                "<font size = '2'>",
                "For more information on data and overall methodology, please see the \"About\" page.",
                "</font>"
              ))
            ),
            bsCollapsePanel(
              "About the Mental Wellness Index",
              HTML("<font size = '2'>"),
              HTML(paste0("The Mental Wellness Index is the weighted sum of 27 measure values, each weighted according to relative importance of the measure in estimating mental wellness (mental health and substance use).<p><p>"
              )),
              HTML(paste0(
                "All states are included.", 
                " Selecting \"All\" will show all included states. Note that this is slower to render and will show ZCTAs as points.<p><p>")),
              HTML(paste0("* ZCTAs are used in the Mental Wellness Index and are represented in maps and plots. ZIP codes are analgous to ZCTAs. When ZIP Codes are entered above, they are mapped to ZCTAs. For more information on ZCTAs, please see <a href='https://www.census.gov/programs-surveys/geography/guidance/geo-areas/zctas.html' target = '_blank'>census.gov</a>.<p><p>"
              )),
              HTML("</font>")
            )
          )
        ),
        mainPanel(
          width = 9,
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
            withSpinner(plotlyOutput("us_distr", height = 500),
                        type = 8, color = "#005B94", hide.ui = F),
            # hr(),
            bsCollapse(
              multiple = T,
              open = c("Measure Interpretation"),
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
              )
            )
          )
        )
      )
    ),
    
    # explore communities ----
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
                "What measure would you like to focus on?",
                choices = avail_meas_list[["pop"]]
              ),
              textInput(
                "zip_choose_com",
                label = "Which ZIP Code would you like to focus on?",
                placeholder = "e.g. 35004, 00501, 20041, etc."
              )
            ),
            bsCollapsePanel(
              "About Selected Measure",
              uiOutput("data_info_com"),
              HTML(paste0(
                "<font size = '2'>",
                "For more information on data and overall methodology, please see the \"About\" page.",
                "</font>"
              ))
            ),
            bsCollapsePanel(
              "About the Mental Wellness Index",
              HTML("<font size = '2'>"),
              HTML(paste0("The Mental Wellness Index is the weighted sum of 27 measure values, each weighted according to relative importance of the measure in estimating mental wellness (mental health and substance use).<p><p>"
              )),
              HTML(paste0(
                "All states are included.",
                " Selecting \"All\" will show all included states. Note that this is slower to render and will show ZCTAs as points.<p><p>")),
              HTML(paste0("* ZCTAs are used in the Mental Wellness Index and are represented in maps and plots. ZIP codes are analgous to ZCTAs. When ZIP Codes are entered above, they are mapped to ZCTAs. For more information on ZCTAs, please see <a href='https://www.census.gov/programs-surveys/geography/guidance/geo-areas/zctas.html' target = '_blank'>census.gov</a>.<p><p>"
              )),
              HTML("</font>")
            )
          )
        ),
        mainPanel(
          width = 9,
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
              open = c("ZCTA Measure Rankings", "Selected Measure Interpretation"),
              bsCollapsePanel(
                "Selected Measure Interpretation",
                uiOutput("com_map_expl")
              ),
              bsCollapsePanel(
                "ZCTA Measure Rankings",
                uiOutput("com_map_report_card")
              )
            )
          )
        )
      )
    ),
    
    # about ----
    tabPanel(
      title = div("About", class="about"),
      HTML(
        "Acknowledgements: MIP team, Larke Huang, etc.<br>"
      ),
      HTML("Contact: Emilie Gilde (egilde@mitre.org)")
    )
  ),
  
  # Copyright footer
  HTML(paste0(
    "<span class = 'copyright-footer'>&copy; ",
    format(Sys.Date(), "%Y"),
    ", The MITRE Corporation</span>"
  ))
  
)

# SERVER ----

server <- function(input, output, session) {
  # preallocate reactive values: state view ----
  
  focus_info <- reactiveValues(
    "hl" = F,
    "ZCTA" = ""
  )
  
  st_sub <- reactiveValues(
    "idx" = "pop",
    "st" = "Alabama",
    "geodat" = geodat[["pop"]][geodat[["pop"]]$STATE_NAME == "Alabama",],
    "mwi" = mwi[["pop"]][mwi[["pop"]]$STATE_NAME == "Alabama",],
    "us_map_fill" = "Mental_Wellness_Index",
    "is_all" = F
  )
  
  us_proxy <- leafletProxy("us_map")
  
  # preallocate reactive values: community view ----
  
  com_sub <- reactiveValues(
    "idx" = "pop",
    "ZCTA" = "30165", 
    "geodat" = geodat[["pop"]][ # community -- within +/- .5
      st_coordinates(geopts$pop)[,1] >=
        st_coordinates(geopts$pop[geopts$pop$GEOID10 == "30165",])[1] - 1 &
        st_coordinates(geopts$pop)[,1] <=
        st_coordinates(geopts$pop[geopts$pop$GEOID10 == "30165",])[1] + 1 &
        st_coordinates(geopts$pop)[,2] >=
        st_coordinates(geopts$pop[geopts$pop$GEOID10 == "30165",])[2] - 1 &
        st_coordinates(geopts$pop)[,2] <=
        st_coordinates(geopts$pop[geopts$pop$GEOID10 == "30165",])[2] + 1 
        ,],
    "mwi" = mwi[["pop"]][# community -- within +/- .5
      mwi[["pop"]]$ZCTA %in% 
        geodat[["pop"]]$GEOID10[
          st_coordinates(geopts$pop)[,1] >=
            st_coordinates(geopts$pop[geopts$pop$GEOID10 == "30165",])[1] - 1 &
            st_coordinates(geopts$pop)[,1] <=
            st_coordinates(geopts$pop[geopts$pop$GEOID10 == "30165",])[1] + 1 &
            st_coordinates(geopts$pop)[,2] >=
            st_coordinates(geopts$pop[geopts$pop$GEOID10 == "30165",])[2] - 1 &
            st_coordinates(geopts$pop)[,2] <=
            st_coordinates(geopts$pop[geopts$pop$GEOID10 == "30165",])[2] + 1 
        ]
      ,],
    "com_map_fill" = "Mental_Wellness_Index"
  )
  
  # observe button inputs and clicks: state view ----
  
  # update measures when the population type changes
  observeEvent(input$idx_type, {
    idx <- input$idx_type
    
    fill <- 
      if (idx == "pop" & grepl("_black", input$us_map_fill)){
        gsub("_black", "_pop", input$us_map_fill)
      } else if (idx == "black" & grepl("_pop", input$us_map_fill) &
                 !input$us_map_fill %in% colnames(mwi[["black"]])){
        gsub("_pop", "_black", input$us_map_fill)
      } else {
        input$us_map_fill
      }
    
    updateSelectInput(
      session = session,
      "us_map_fill",
      "Which score/measure would you like to explore?",
      choices = avail_meas_list[[idx]],
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
      st_sub$geodat <- geopts[[idx]]
      st_sub$mwi <- mwi[[idx]]
      st_sub$is_all <- T
      
    } else {
      # also include zips from bordering states
      
      st_sub$st <- input$st_focus
      st_sub$geodat <- geodat[[idx]][
        geodat[[idx]]$STATE_NAME == input$st_focus | 
          geodat[[idx]]$STATE_2 == st_to_fips[input$st_focus] & 
          !is.na(geodat[[idx]]$STATE_2),]
      st_sub$mwi <- mwi[[idx]][
        mwi[[idx]]$STATE_NAME == input$st_focus | 
          mwi[[idx]]$STATE_2 == st_to_fips[input$st_focus] & 
          !is.na(mwi[[idx]]$STATE_2),]
      st_sub$is_all <- F
    }
    
    st_sub$us_map_fill <- 
      if (st_sub$idx == "pop" & grepl("_black", input$us_map_fill)){
        gsub("_black", "_pop", input$us_map_fill)
      } else if (st_sub$idx == "black" & grepl("_pop", input$us_map_fill) &
                 !input$us_map_fill %in% colnames(mwi[["black"]])){
        gsub("_pop", "_black", input$us_map_fill)
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
      if (st_sub$idx == "pop" & grepl("_black", input$us_map_fill)){
        gsub("_black", "_pop", input$us_map_fill)
      } else if (st_sub$idx == "black" & grepl("_pop", input$us_map_fill) &
                 !input$us_map_fill %in% colnames(mwi[["black"]])){
        gsub("_pop", "_black", input$us_map_fill)
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
        zip_to_zcta[input$zip_choose] %in% st_sub$geodat$GEOID10 # in the state
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
      if (idx == "pop" & grepl("_black", input$com_map_fill)){
        gsub("_black", "_pop", input$com_map_fill)
      } else if (idx == "black" & grepl("_pop", input$com_map_fill) &
                 !input$com_map_fill %in% colnames(mwi[["black"]])){
        gsub("_pop", "_black", input$com_map_fill)
      } else {
        input$com_map_fill
      }
    
    updateSelectInput(
      session = session,
      "com_map_fill",
      "What measure would you like to focus on?",
      choices = avail_meas_list[[idx]],
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
      all_coord <- st_coordinates(geopts[[idx]])
      zcta_coord <- 
        st_coordinates(geopts$pop[geopts$pop$GEOID10 == com_sub$ZCTA,])
      
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
      com_sub$geodat <- geodat[[idx]][zcta_log,]
      com_sub$mwi <- mwi[[idx]][mwi[[idx]]$ZCTA %in% 
                                  geodat[[idx]]$GEOID10[zcta_log],]
      
      com_sub$com_map_fill <- 
        if (com_sub$idx == "pop" & grepl("_black", input$com_map_fill)){
          gsub("_black", "_pop", input$com_map_fill)
        } else if (com_sub$idx == "black" & grepl("_pop", input$com_map_fill) &
                   !input$com_map_fill %in% colnames(mwi[["black"]])){
          gsub("_pop", "_black", input$com_map_fill)
        } else {
          input$com_map_fill
        }
    }
  })
  
  
  # reset map click focus
  observeEvent(input$com_map_fill, {
    com_sub$com_map_fill <- 
      if (com_sub$idx == "pop" & grepl("_black", input$com_map_fill)){
        gsub("_black", "_pop", input$com_map_fill)
      } else if (com_sub$idx == "black" & grepl("_pop", input$com_map_fill) &
                 !input$com_map_fill %in% colnames(mwi[["black"]])){
        gsub("_pop", "_black", input$com_map_fill)
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
      full_name <- measure_to_names[[st_sub$idx]][st_sub$us_map_fill]

      HTML(paste0(
        "<font size = '2'>",

        "A variety of data sources are used for measures comprising the Mental Wellness Index. The data currently pictured for ",
        full_name,
        " came from ",
        ifelse(full_name == "Mental Wellness Index",
               "",
               info_df[st_sub$us_map_fill, "Years"]
        ),
        " ",
        ifelse(full_name == "Mental Wellness Index",
               "various sources of",
               info_df[st_sub$us_map_fill, "Source"]
        ),
        " data.<p><p>",
        ifelse(
          full_name == "Mental Wellness Index",
          "",
          paste0(
            full_name, " Description: ",
            info_df[st_sub$us_map_fill, "Measure.Description"]
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
                 st_sub$idx, is_all = st_sub$is_all)
      
      if (focus_info$hl){
        # add a highlighted polygon
        us_proxy <- plot_map(st_sub$us_map_fill, st_sub$geodat, 
                             st_sub$idx,
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
  
  # put an explanation
  output$us_map_expl <- renderUI({
    withProgress(message = "Rendering map explanation", {
      full_name <- measure_to_names[[st_sub$idx]][st_sub$us_map_fill]
      mc <- meas_max_colors[meas_col_to_type[full_name]]
      lc <- if (st_sub$us_map_fill == "Mental_Wellness_Index"){
        meas_min_colors[meas_col_to_type[full_name]]
      } else {
        mc
      }
      # # get the orientation for the measure
      # if (st_sub$us_map_fill != "Score"){
      #   ori <- info_df[st_sub$us_map_fill, "Direction"]
      #   ori <- ifelse(ori > 0, "higher", "lower")
      # } else {
      #   ori <- "higher"
      # }
      # 
      text <- paste0(
        "A ", 
        html_color(mc, "higher"),
        " value indicates more ",
        html_color(mc, "assets"),
        " supporting ", 
        html_color(mc, paste("mental wellness")),
        "."
      )
      
      # if (st_sub$us_map_fill != "Mental_Wellness_Index"){
      #   wt <- round(info_df[st_sub$us_map_fill, "Effective_Weights"],2)
      #   
      #   # TODO: COME BACK TO THIS NUMBER
      #   text <- paste0(
      #     text,
      #     " The ", 
      #     html_color(mc, full_name),
      #     " measure has a weight of ", 
      #     html_color(mc, wt),
      #     ", indicating a ",
      #     html_color(mc, ifelse(wt > 3, "high", "low")),
      #     " contribution to the overall Mental Wellness Index."
      #   )
      # } else {
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
      # }
      
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
      "Distribution of ", measure_to_names[[st_sub$idx]][st_sub$us_map_fill],
      " for the ",
      ifelse(st_sub$idx == "black", "Black ", "Overall "),
      "Population in ",
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
        full_name <- measure_to_names[[st_sub$idx]][st_sub$us_map_fill]
        
        # get scores
        f_val <- 
          st_sub$mwi[
            st_sub$mwi$ZCTA == focus_info$ZCTA, st_sub$us_map_fill]
        all_st_val <- st_sub$mwi[, st_sub$us_map_fill]
        all_us_val <- mwi[[st_sub$idx]][, st_sub$us_map_fill]
        
        # get colors
        if (st_sub$us_map_fill == "Mental_Wellness_Index"){
          mc <- 
            if (is.na(f_val) | f_val >= 50){
              meas_max_colors[meas_col_to_type[full_name]]
            } else {
              meas_min_colors[meas_col_to_type[full_name]]
            }
        } else {
          mc <- meas_max_colors[meas_col_to_type[full_name]]
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
            " has a ", html_color(mc, full_name), " national ranking of ",
            html_color(mc, trunc(f_val)),
            ", putting it at the ",
            html_color(mc, st_perc), 
            " percentile for the state.",
            " This is in the ", 
            html_color(mc, st_comp), " relative to ",
            input$st_focus,", and in the ",
            html_color(mc, us_comp), " relative to the United States.",
            "</font></b><p></p>",
            "<font size = '2'>",
            "<i>A higher value indicates more assets supporting mental wellness. A lower value indicates more obstacles to mental wellness.</i>",
            "</font>",
            "</center>"
          ))
        } else {
          HTML(paste0(
            "<center>",
            "<font size = '2'>",
            "<i>A higher value indicates more assets supporting mental wellness. A lower value indicates more obstacles to mental wellness.</i>",
            "<p></p>",
            "</font>",
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
            "<i>A higher value indicates more assets supporting mental wellness. A lower value indicates more obstacles to mental wellness.</i>",
            "</font>",
            "</center>"
          ))
        }
      }
    })
  })
  # output plots and information: community view ----
  
  output$data_info_com <- renderUI({
    withProgress(message = "Rendering data information", {
      full_name <- measure_to_names[[com_sub$idx]][com_sub$com_map_fill]
      
      HTML(paste0(
        "<font size = '2'>",
        
        "A variety of data sources are used for measures comprising the Mental Wellness Index. The data currently pictured for ",
        full_name,
        " came from ",
        ifelse(full_name == "Mental Wellness Index",
               "",
               info_df[com_sub$com_map_fill, "Years"]
        ),
        " ",
        ifelse(full_name == "Mental Wellness Index",
               "various sources",
               info_df[com_sub$com_map_fill, "Source"]
        ),
        " data.<p><p>",
        ifelse(
          full_name == "Mental Wellness Index",
          "",
          paste0(
            full_name, " Description: ",
            info_df[com_sub$com_map_fill, "Measure.Description"]
          )),
        "<p>",
        "</font>"
      ))
    })
  })
  
  # plot map based on fill -- STOP HERE
  output$com_map <- renderLeaflet({
    withProgress(message = "Rendering map", {
      plot_map(com_sub$com_map_fill, com_sub$geodat,
               com_sub$idx, is_all = F,
               is_com = T, zcta_choose = com_sub$ZCTA)
    })
  })
  
  # put an explanation
  output$com_map_expl <- renderUI({
    withProgress(message = "Rendering map explanation", {
      full_name <- measure_to_names[[com_sub$idx]][com_sub$com_map_fill]
      mc <- meas_max_colors[meas_col_to_type[full_name]]
      lc <- meas_min_colors[meas_col_to_type[full_name]]
      # # get the orientation for the measure
      # if (com_sub$com_map_fill != "Score"){
      #   ori <- info_df[com_sub$com_map_fill, "Direction"]
      #   ori <- ifelse(ori > 0, "higher", "lower")
      # } else {
      #   ori <- "higher"
      # }
      # 
      # text <- paste0(
      #   "A ",
      #   html_color(mc, "higher"),
      #   " value indicates more ",
      #   html_color(mc, "assets"),
      #   " supporting mental ",
      #   html_color(mc, paste(ori, "wellness")),
      #   "."
      # )
      # 
      # if (com_sub$com_map_fill != "Mental_Wellness_Index"){
      #   wt <- round(info_df[com_sub$com_map_fill, "Effective_Weights"],2)
      #   
      #   # TODO: COME BACK TO THIS NUMBER
      #   text <- paste0(
      #     text,
      #     " The ", 
      #     html_color(mc, full_name),
      #     " measure has a weight of ", 
      #     html_color(mc, wt),
      #     ", indicating a ",
      #     html_color(mc, ifelse(wt > 3, "high", "low")),
      #     " contribution to the overall Mental Wellness Index."
      #   )
      # } else {
      #   text <- paste0(
      #     text,
      #     " A ", 
      #     html_color(lc, "lower"),
      #     " value indicates more ",
      #     html_color(lc, "obstacles"),
      #     " to mental ", 
      #     html_color(lc, "wellness"),
      #     "."
      #   )
      # }
      # 
      # add ZCTA interpretation
      
      # get scores
      f_val <- 
        com_sub$mwi[
          com_sub$mwi$ZCTA == com_sub$ZCTA, com_sub$com_map_fill]
      all_com_val <- com_sub$mwi[, com_sub$com_map_fill]
      all_us_val <- mwi[[com_sub$idx]][, com_sub$com_map_fill]
      
      # get colors
      if (com_sub$com_map_fill == "Mental_Wellness_Index"){
        mc <- 
          if (is.na(f_val) | f_val >= 50){
            meas_max_colors[meas_col_to_type[full_name]]
          } else {
            meas_min_colors[meas_col_to_type[full_name]]
          }
      } else {
        mc <- meas_max_colors[meas_col_to_type[full_name]]
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
          " has a ", html_color(mc, full_name), " national ranking of ",
          html_color(mc, trunc(f_val)),
          ", putting it at the ",
          html_color(mc, com_perc), 
          " percentile for the state.",
          " This is in the ", 
          html_color(mc, com_comp), 
          " relative to the selected community, and in the ",
          html_color(mc, us_comp), " relative to the United States.",
          "</font></b></center>"
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
            "</font></b></center>"
          ))
      }
      
      HTML(paste0(
        "<center>",
        "<font size = '3'><b>",
        # text,
        # "<p><p>",
        text_2,
        "</b></font>",
        "</center>"
      ))
    })
  })
  
  # put a "report card" for the community
  output$com_map_report_card <- renderUI({
    mwi_zcta <- com_sub$mwi[com_sub$mwi$ZCTA == com_sub$ZCTA, , drop = F]
    
    text <- ""
    for (dn in names(avail_meas_list[[com_sub$idx]])){
      mc <- meas_max_colors[dn]
      
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
      
      if (dn != "Mental Wellness Index"){
        for (cn in avail_meas_list[[com_sub$idx]][[dn]]){
          text <- paste0(
            text, 
            "<b>",
            html_color(
              mc, 
              paste0(
                # ifelse(info_df[cn, "Directionality"] > 0, "Higher ", "Lower "),
                measure_to_names[[com_sub$idx]][cn])),
            ": ",
            "</b>",
            trunc(mwi_zcta[1, cn]),
            "<br>"
          )
        }
      }
      
      text <- paste0(
        text,
        "</p>",
        "<p>"
      )
    }
    
    HTML(paste0(
      "<font size = '2'>",
      "<i>A higher value indicates more assets supporting mental wellness. A lower value indicates more obstacles to mental wellness.</i>",
      "<p></p>",
      text,
      "</font>"
    ))
  })
  
}

# RUN ----

shinyApp(ui, server)
