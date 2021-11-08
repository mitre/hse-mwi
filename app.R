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
  "BuPu",
  "Greens",
  "Blues",
  "PRGn"
)
names(meas_colors) <- c(unique(m_reg$Category), "Mental Wellness Index")

# get max/min colors for each palette
meas_max_colors <- sapply(1:length(meas_colors), function(x){
  brewer.pal(3, meas_colors[x])[3]
})
meas_min_colors <- sapply(1:length(meas_colors), function(x){
  brewer.pal(3, meas_colors[x])[1]
})
names(meas_max_colors) <- names(meas_min_colors) <- names(meas_colors)

# add cities/states to mwi
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
# geodat <- list()
# for (idx in index_types){
#   geodat[[idx]] <-
#     geo_join(zips, mwi[[idx]], by_sp = "GEOID10", by_df = "ZCTA", how = "left")
# 
#   # sort by state code
#   geodat[[idx]] <- geodat[[idx]][order(geodat[[idx]]$STATE),]
# 
#   # add state and county (multiple counties for a zcta separated by pipes)
# }
# # saving for now, while things are stable
# save(list = "geodat", file = file.path(data_folder, "Cleaned", "HSE_MWI_ZCTA_full_shapefile_US.RData"))

# load geodat data (should be much faster)
load(file.path(data_folder, "Cleaned", "HSE_MWI_ZCTA_full_shapefile_US.RData"))

# get available zctas -- both will have the same
avail_zctas <- geodat[["pop"]]$GEOID10
names(avail_zctas) <- paste0(geodat[["pop"]]$GEOID10, 
                             " (State: ", geodat[["pop"]]$STATE_NAME, ")")

# plot functions ----

# plot the overall map, filled by measure/score (LEAFLET)
plot_map <- function(fill, geodat, idx, fill_opacity = .7,
                     add_poly = F, us_proxy = NA, zcta_choose = NA){
  # subset map for easy plotting
  gd_map <- geodat[,c(fill, "GEOID10", "STATE", "geometry")]
  colnames(gd_map)[1] <- "Fill"
  
  # get rid of empty polygons
  gd_map <- gd_map[!is.na(gd_map$GEOID10),]
  
  # create palette
  pal <- colorNumeric(
    palette = meas_colors[meas_col_to_type[measure_to_names[[idx]][fill]]],
    domain = c(0, gd_map$Fill, 100),
    na.color = "transparent",
    reverse = ifelse(fill == "Score", T, F)
  )
  pal_wo_na <- colorNumeric(
    palette = meas_colors[meas_col_to_type[measure_to_names[[idx]][fill]]],
    domain = c(0, gd_map$Fill, 100),
    na.color=rgb(0,0,0,0),
    reverse = ifelse(fill == "Score", T, F)
  )
  
  # labels 
  full_name <- measure_to_names[[idx]][fill]
  if (full_name != "Mental Wellness Index"){
    full_name <- paste0(
      ifelse(info_df[fill, "Directionality"] > 0, "Higher ", "Lower "), 
      full_name, 
      " Ranking"
    )
  }
  labels <- 
    paste0(
      "State: ", gd_map$STATE_NAME, "<br>",
      "ZCTA: ", gd_map$GEOID10, "<br>", 
      full_name,": ", signif(gd_map$Fill, 4)) %>%
    lapply(htmltools::HTML)
  
  # get initial zoom area
  bounds <- unname(st_bbox(geodat))
  
  if (!add_poly){
    # create map
    mp <- leaflet(data = gd_map) %>%
      addProviderTiles("OpenStreetMap") %>% #CartoDB
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
                                               bringToFront = T),
                  label = labels) %>%
      addLegend(pal = pal_wo_na,
                values = ~c(0, Fill, 100), 
                opacity = 0.7, 
                position = "bottomright",
                title = unname(full_name)) %>%
      fitBounds(
        lng1 = bounds[1],
        lng2 = bounds[3],
        lat1 = bounds[2],
        lat2 = bounds[4]
      )
  } else {
    zcta_select <- gd_map[gd_map$GEOID10 == zcta_choose,]
    
    mp <- us_proxy %>% 
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
        label = labels
      )
  }
  
  return(mp)
}

# plot a distribution of the fill value using a beeswarm plot (PLOTLY)
plot_bee_distr <- function(fill, st, mwi, idx, hl = F, zcta_hl = ""){
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
      palette = meas_colors[meas_col_to_type[measure_to_names[[idx]][fill]]],
      domain = c(0, bee.df$val, 100),
      na.color = "transparent",
      reverse = ifelse(fill == "Score", T, F)
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
      ggplot(bee.df, aes(val, lab, color = val, size = focus))+
        scale_color_distiller(
          palette = meas_colors[meas_col_to_type[measure_to_names[[idx]][fill]]],
          direction = ifelse(fill == "Score", -1, 1)
        )+
        # scale_alpha(range = c(0,1))+
        # scale_color_manual(values = hl_pal)+
        scale_size_manual(values = hl_size)
    } else {
      ggplot(bee.df, aes(val, lab, color = val), size = 1.5)+
        scale_color_distiller(
          palette = meas_colors[meas_col_to_type[measure_to_names[[idx]][fill]]],
          direction = ifelse(fill == "Score", -1, 1)
        )
    }
  
  full_name <- measure_to_names[[idx]][fill]
  if (full_name != "Mental Wellness Index"){
    full_name <- paste0(
      ifelse(info_df[fill, "Directionality"] > 0, "Higher ", "Lower "), 
      full_name, 
      " Ranking"
    )
  }
  
  p <- suppressWarnings(
    p + 
    geom_quasirandom(
      aes(text = paste0(
        "ZCTA: ", zcta, "\n",
        full_name, ": ", signif(val, 4)
      )),
      groupOnX = F, alpha = bee.df$focus_alpha)+
    theme_bw()+
    xlab(full_name)+
    theme(
      axis.text.y = element_blank(),
      axis.title.y = element_blank(),
      axis.ticks.y = element_blank(),
      legend.position = "none",
      plot.title = element_text(hjust = .5)
    )+
    xlim(-3, 103)+
    ggtitle(paste0("Distribution of ", measure_to_names[[idx]][fill], 
                   " for the ",
                   ifelse(idx == "black", "Black ", ""),
                   "Population in ", 
                   st))+
    NULL
  )
  
  ggplotly(
    p,
    tooltip = c("text")
  ) %>% config(displayModeBar = F)
}

# map percentiles to text
quant_map <- function(perc){
  return(
    if (perc < 25) {
      "very low"
    } else if (perc < 50){
      "low"
    } else if (perc < 75){
      "high"
    } else {
      "very high"
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
      title="", windowTitle="Mental Wellness Index Tool"
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
      "Mental Wellness Index Tool",
    ),
    theme="stylesheets/app.css",
    
    # explore ----
    tabPanel(
      title = div("Explore", class="explore"),
      class = "explore-panel",
      
      sidebarLayout(
        sidebarPanel(
          width = 3,
          HTML("<b>Welcome to the Mental Wellness Index (MWI) Tool!</b> Select any of the options below to get started. <b>If you would like to focus on a specific Zip Code Tabulation Area (ZCTA), click on it in the map to the right or select it from the list below.</b><p>"),
          radioButtons(
            inputId = "idx_type",
            label = "Which population's MWI do you want to view?",
            choices = index_types,
            inline = T
          ),
          # TODO: UPDATE THESE BASED ON POPULATION SELECTED
          selectInput(
            "st_focus",
            "Which state would you like to focus on?",
            choices = c(unname(f_st), "All"),
            selected = "North Carolina"
          ),
          selectInput(
            "us_map_fill",
            "What would you like to explore?",
            choices = avail_meas_list[["pop"]]
          ),
          # sliderInput(
          #   "us_map_fill_opacity",
          #   HTML("<b>What fill opacity (%)?</b> A higher number indicates a more opaque fill."),
          #   min = 0,
          #   max = 100,
          #   value = 70,
          #   step = 10
          # ),
          selectInput(
            "zcta_choose",
            "Which ZCTA would you like to focus on?",
            choices = c("None" = "")#, avail_zctas)
          ),
          actionButton("reset_zcta_click", "Reset ZCTA Focus"),
          hr(),
          HTML("<font size = '2'>"),
          HTML(paste0(
            "Data sourced from various publically available data sources.",
            "More information on methodology can be found in the about tab."
          )),
          # TODO: FIX INFO HERE
          # uiOutput("data_info"),
          HTML(paste0("The Mental Wellness Index is the weighted sum of 27 measure values, each weighted according to relative importance of the measure in estimating mental wellness (mental health and substance use).<p><p>"
          )),
          HTML(paste0(
            "All states are included.", 
            " Selecting \"All\" will show all included states. Note that this is slower to render.<p>")),
          HTML("</font>"),
        ),
        mainPanel(
          width = 9,
          leafletOutput("us_map", height = 400),
          HTML("<br>"),
          uiOutput("us_map_legend"),
          HTML("<br>"),
          uiOutput("us_map_expl"),
          hr(),
          fluidRow(
            column(
              width = 8,
              plotlyOutput("us_distr")
            ),
            column(
              width = 4,
              tableOutput("us_quantile"),
              br(),
              uiOutput("us_info")
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
  # preallocate reactive values ----
  
  focus_info <- reactiveValues(
    "hl" = F,
    "ZCTA" = ""
  )
  
  st_sub <- reactiveValues(
    "idx" = "pop",
    "st" = "North Carolina",
    "geodat" = geodat[["pop"]][geodat[["pop"]]$STATE_NAME == "North Carolina",],
    "mwi" = mwi[["pop"]][mwi[["pop"]]$STATE_NAME == "North Carolina",],
    "us_map_fill" = "Mental_Wellness_Index"
  )
  
  us_proxy <- leafletProxy("us_map")
  
  # observe button inputs and clicks ----
  
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
    
    st_sub$idx <- idx
    
    if (input$st_focus == "All"){
      st_sub$st <- "All"
      st_sub$geodat <- geodat[[idx]]
      st_sub$mwi <- mwi[[idx]]
      
      updateSelectInput(session, 
                        "zcta_choose", 
                        "Which ZCTA would you like to focus on?",
                        choices = c(
                          "None" = "", 
                          avail_zctas)
      )
    } else {
      st_sub$st <- input$st_focus
      st_sub$geodat <- geodat[[idx]][geodat[[idx]]$STATE_NAME == input$st_focus,]
      st_sub$mwi <- mwi[[idx]][mwi[[idx]]$STATE_NAME == input$st_focus,]
      
      updateSelectInput(session, 
                        "zcta_choose", 
                        "Which ZCTA would you like to focus on?",
                        choices = c(
                          "None" = "", 
                          avail_zctas[grepl(input$st_focus, names(avail_zctas))]
                        )
      )
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
    
    focus_info$hl <- F
    focus_info$ZCTA <- ""
    
    # remove any previously highlighted polygon
    us_proxy %>% removeShape("remove_me")
  })
  
  # update the ZCTA
  observeEvent(input$reset_zcta_click, {
    focus_info$hl <- F
    focus_info$ZCTA <- ""
    
    # remove any previously highlighted polygon
    us_proxy %>% removeShape("remove_me")
  })
  
  # reset map click focus
  observeEvent(input$us_map_fill, {
    st_sub$us_map_fill <- 
      if (st_sub$idx == "pop" & grepl("_black", input$us_map_fill)){
        gsub("_black", "_pop", input$us_map_fill)
      } else if (st_sub$idx == "black" & grepl("_pop", input$us_map_fill)){
        gsub("_pop", "_black", input$us_map_fill)
      } else {
        input$us_map_fill
      }
    
    focus_info$hl <- F
    focus_info$ZCTA <- ""
    
    # remove any previously highlighted polygon
    us_proxy %>% removeShape("remove_me")
  })
  
  # update the focus
  observeEvent(input$us_map_shape_click, {
    if (!is.null(input$us_map_shape_click$id)){
      focus_info$hl <- T
      focus_info$ZCTA <- input$us_map_shape_click$id
      
      # remove any previously highlighted polygon
      us_proxy %>% removeShape("remove_me")
      
      # add a highlighted polygon
      us_proxy <- plot_map(st_sub$us_map_fill, st_sub$geodat, 
                           st_sub$idx,
                           add_poly = T, us_proxy = us_proxy, 
                           zcta_choose = focus_info$ZCTA)
    } else {
      focus_info$hl <- F
      focus_info$ZCTA <- ""
      
      # remove any previously highlighted polygon
      us_proxy %>% removeShape("remove_me")
    }
  })
  
  observeEvent(input$zcta_choose, {
    if (input$zcta_choose != ""){
      focus_info$hl <- T
      focus_info$ZCTA <- input$zcta_choose
      
      # remove any previously highlighted polygon
      us_proxy %>% removeShape("remove_me")
      
      # add a highlighted polygon
      us_proxy <- plot_map(st_sub$us_map_fill, st_sub$geodat, 
                           st_sub$idx,
                           add_poly = T, us_proxy = us_proxy, 
                           zcta_choose = focus_info$ZCTA)
    } else {
      focus_info$hl <- F
      focus_info$ZCTA <- ""
      
      # remove any previously highlighted polygon
      us_proxy %>% removeShape("remove_me")
    }
  })
  
  # output plots and information ----
  
  # output$data_info <- renderUI({
  #   withProgress(message = "Rendering data information", {
  #     full_name <- measure_to_names[input$us_map_fill]
  #     
  #     HTML(paste0(
  #       "<font size = '2'>",
  #       
  #       "A variety of data sources are used for measures comprising the Unmet Need Score. The data currently pictured for ", 
  #       full_name, 
  #       " came from ",
  #       ifelse(full_name == "Unmet Need Score", 
  #              "various sources",
  #              measure_orientation[full_name, "Source"]),
  #       ".<p>",
  #       "</font>"
  #     ))
  #   })
  # })
  
  # plot map based on fill
  output$us_map <- renderLeaflet({
    withProgress(message = "Rendering map", {
      plot_map(st_sub$us_map_fill, st_sub$geodat,
               st_sub$idx)
    })
  })
  
  # put a legend
  output$us_map_legend <- renderUI({
    withProgress(message = "Rendering map legend", {
      HTML(paste0(
        "<center>",
        paste(
          sapply(1:length(meas_max_colors), function(x){
            paste0("<font color = ", meas_max_colors[x], " size = '3'><b>", 
                   names(meas_max_colors[x]), "</font></b>")
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
      lc <- meas_min_colors[meas_col_to_type[full_name]]
      # get the orientation for the measure
      if (st_sub$us_map_fill != "Score"){
        ori <- info_df[st_sub$us_map_fill, "Direction"]
        ori <- ifelse(ori > 0, "higher", "lower")
      } else {
        ori <- "higher"
      }
      
      text <- paste0(
        "A ", 
        html_color(mc, "higher"),
        " value indicates more ",
        html_color(mc, "assets"),
        " supporting mental ", 
        html_color(mc, paste(ori, "wellness")),
        "."
      )
      
      if (st_sub$us_map_fill != "Mental_Wellness_Index"){
        wt <- round(info_df[st_sub$us_map_fill, "Effective_Weights"],2)
        
        # TODO: COME BACK TO THIS NUMBER
        text <- paste0(
          text,
          " The ", 
          html_color(mc, full_name),
          " measure has a weight of ", 
          html_color(mc, wt),
          ", indicating a ",
          html_color(mc, ifelse(wt > 3, "high", "low")),
          " contribution to the overall Mental Wellness Index."
        )
      } else {
        text <- paste0(
          text,
          " A ", 
          html_color(lc, "lower"),
          " value indicates more ",
          html_color(lc, "obstacles"),
          " to mental ", 
          html_color(lc, "wellness"),
          "."
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
  
  output$us_distr <- renderPlotly({
    withProgress(message = "Rendering ZCTA data distribution", {
      plot_bee_distr(st_sub$us_map_fill, 
                     st = st_sub$st,
                     mwi = st_sub$mwi,
                     idx = st_sub$idx,
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
            if (f_val < 50){
              meas_max_colors[meas_col_to_type[full_name]]
            } else {
              meas_min_colors[meas_col_to_type[full_name]]
            }
        } else {
          mc <- meas_max_colors[meas_col_to_type[full_name]]
        }
        
        # get percentiles relative to state and us
        st_perc <- signif(ecdf(all_st_val)(f_val)*100, 4)
        us_perc <- signif(ecdf(all_us_val)(f_val)*100, 4)
        
        # get text value for percentile: very low/high, low/high
        st_comp <- quant_map(st_perc)
        us_comp <- quant_map(us_perc)
        
        HTML(paste0(
          "<center><b><font size = '3'>",
          "ZCTA ", html_color(mc, focus_info$ZCTA),
          " has a ", html_color(mc, full_name), " of ",
          html_color(mc, signif(f_val, 4)),
          ", putting it at the ",
          html_color(mc, st_perc), 
          " percentile for the state.",
          " This is ", 
          html_color(mc, st_comp), " relative to ",
          input$st_focus,", and ",
          html_color(mc, us_comp), " relative to the United States.",
          "</font></b></center>"
        ))
      }
    })
  })
}

# RUN ----

shinyApp(ui, server)
