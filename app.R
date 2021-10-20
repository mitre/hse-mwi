# Behavioral Health Needs App
# By Hannah De los Santos
# Originated on: 12/18/2020

# load data and libraries ----

library(htmltools)
library(shiny)
library(tigris)
library(leaflet)
library(RColorBrewer)
library(sf)
library(plotly)
library(ggbeeswarm)

#https://stackoverflow.com/questions/3452086/getting-path-of-an-r-script/35842176#35842176
# set working directory - only works in RStudio (with rstudioapi)
# setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

data_folder <- file.path("..", "Data")

# sanam 
sanam_orig <- read.csv(file.path(data_folder, "data_measures_and_score.csv"),
                       colClasses = c("ZCTA" = "character"))
sanam_colnames <- colnames(
  read.csv(file.path(data_folder, "data_measures_and_score.csv"), 
           nrows = 1, check.names = F)
)
# filter data only to NC and bordering states
states_filt <- c(
  "North Carolina" = "NC",
  "Virginia" = "VA",
  "Tennessee" = "TN",
  "Georgia" = "GA",
  "South Carolina" = "SC"
  )
# create a pretty set of names for later
st_abbrev_to_full <- c(names(states_filt), "All States")
names(st_abbrev_to_full) <- c(states_filt, "All")

sanam <- sanam_orig[sanam_orig$STATE %in% states_filt,]
sanam$ZCTA <- as.character(sanam$ZCTA)

# also remove data with no weights
s_wts <- read.csv(
  file.path(data_folder, 
            "data_weights_UNS 2.0 Weights Broadband ICE - PR Adjusted.csv")
)
sub_names <- sanam_colnames[
  sanam_colnames %in% c(
    s_wts$TYPE[s_wts$CATEGORY != "Not using" & !is.na(s_wts$States.DC)],
    "Limited Access to Healthy Foods",
    "Foreign Born" #adding a couple that do not fit the mold
    )
]
sanam <- cbind(sanam[,-c(2:122)],
               sanam[,2:122][, sanam_colnames[2:122] %in% sub_names])

# measure type information
measure_to_type <- read.csv(file.path(data_folder, "measures_to_type.csv"))

# measure orientation information
measure_orientation <- 
  read.csv(file.path(data_folder, "data_measure_registry.csv"))
# add measure orientation to full data frame
rownames(measure_orientation) <- measure_orientation$Measure
s_orientation <- 
  measure_orientation[sanam_colnames[2:122][sanam_colnames[2:122] %in% sub_names],
                      c("Orientation", "Measure")]
s_orientation[,"SANAM_Names"] <- colnames(sanam)[-c(1:11)]
rownames(s_orientation) <- s_orientation$SANAM_Names

# plotting information ----

# create measure names
avail_measures <- c("Score", colnames(sanam)[-c(1:11)])
names(avail_measures) <- 
  c("Unmet Need Score", sanam_colnames[2:122][sanam_colnames[2:122] %in% sub_names])

measure_to_names <- names(avail_measures)
names(measure_to_names) <- avail_measures

# get measure colors
meas_colors <- c(
  "Greens",
  "Blues",
  "BuPu",
  "RdBu"
)
names(meas_colors) <- unique(measure_to_type$Overall.Type)

# get max/min colors for each palette
meas_max_colors <- sapply(1:length(meas_colors), function(x){
  brewer.pal(3, meas_colors[x])[
    ifelse(names(meas_colors[x]) == "Unmet Need Score", 1, 3)
  ]
})
meas_min_colors <- sapply(1:length(meas_colors), function(x){
  brewer.pal(3, meas_colors[x])[
    ifelse(names(meas_colors[x]) == "Unmet Need Score", 3, 1)
  ]
})
names(meas_max_colors) <- names(meas_min_colors) <- names(meas_colors)

# has the pretty names
meas_col_to_type <- measure_to_type$Overall.Type
names(meas_col_to_type) <- measure_to_type$Name

# group into list for display
avail_meas_list <- list()
m_to_type <- meas_col_to_type[measure_to_names[avail_measures]]
# add the weights to the name
avail_meas_w_weights <- avail_measures
rownames(measure_to_type) <- measure_to_type$Name
names(avail_meas_w_weights) <- 
  paste0(names(avail_measures),
         " (Weight: ", measure_to_type[names(avail_measures), "Weight"],
         ")")
# unmet need score doesn't have a weight
names(avail_meas_w_weights)[1] <- names(avail_measures)[1]
# add them to the list
for (t in unique(m_to_type)){
  avail_meas_list[[t]] <- avail_meas_w_weights[m_to_type == t]
}

# # get zip code data
# # NOTE: cb = T will download a generalized file
# zips <- zctas(
#   cb = T, 
#   starts_with = as.character(sanam$ZCTA[sanam$STATE == "NC"])
# )
# for (st in states_filt[-1]){
#   zips <- rbind(
#     zips,
#     zctas(
#       cb = T, 
#       starts_with = as.character(sanam$ZCTA[sanam$STATE == st])
#     ))
# }
# zips <- st_transform(zips, crs = "+proj=longlat +datum=WGS84")

# load zip code data (should be much faster)
load(file.path(data_folder, "ZCTAS_NC_and_border_states.RData"))

# create the geo data for leaflet
geodat <- geo_join(zips, sanam, by_sp = "GEOID10", by_df = "ZCTA", how = "left")

# get available zctas
avail_zctas <- geodat$GEOID10
names(avail_zctas) <- paste0(geodat$GEOID10, " (State: ", geodat$STATE, ")")

# plot functions ----

# plot the overall map, filled by measure/score (LEAFLET)
plot_map <- function(fill, geodat, fill_opacity = .7,
                     add_poly = F, us_proxy = NA, zcta_choose = NA){
  # subset map for easy plotting
  gd_map <- geodat[,c(fill, "GEOID10", "STATE", "geometry")]
  colnames(gd_map)[1] <- "Fill"
  
  # create palette
  pal <- colorNumeric(
    palette = meas_colors[meas_col_to_type[measure_to_names[fill]]],
    domain = c(0, gd_map$Fill, 100),
    na.color = "transparent",
    reverse = ifelse(fill == "Score", T, F)
  )
  pal_wo_na <- colorNumeric(
    palette = meas_colors[meas_col_to_type[measure_to_names[fill]]],
    domain = c(0, gd_map$Fill, 100),
    na.color=rgb(0,0,0,0),
    reverse = ifelse(fill == "Score", T, F)
  )
  
  # labels 
  labels <- 
    paste0(
      "State: ", gd_map$STATE, "<br>",
      "ZCTA: ", gd_map$GEOID10, "<br>", 
      measure_to_names[fill],": ", signif(gd_map$Fill, 4)) %>%
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
                title = unname(measure_to_names[fill])) %>%
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
plot_bee_distr <- function(fill, st, sanam, hl = F, zcta_hl = ""){
  bee.df <- data.frame(
    val = sanam[,fill],
    zcta = sanam$ZCTA,
    lab = rep("val", nrow(sanam)),
    focus = rep("val", nrow(sanam)),
    focus_alpha = rep(1, nrow(sanam))
  )
  # remove all the empty rows
  bee.df <- bee.df[complete.cases(bee.df),]
  
  # if we're going to highlight a point
  if (hl){
    row_hl <- which(bee.df$zcta == zcta_hl)
    bee.df$focus[row_hl] <- "Focus"
    bee.df$focus_alpha[-row_hl] <- .3
    
    pal <- colorNumeric(
      palette = meas_colors[meas_col_to_type[measure_to_names[fill]]],
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
          palette = meas_colors[meas_col_to_type[measure_to_names[fill]]],
          direction = ifelse(fill == "Score", -1, 1)
        )+
        # scale_alpha(range = c(0,1))+
        # scale_color_manual(values = hl_pal)+
        scale_size_manual(values = hl_size)
    } else {
      ggplot(bee.df, aes(val, lab, color = val), size = 1.5)+
        scale_color_distiller(
          palette = meas_colors[meas_col_to_type[measure_to_names[fill]]],
          direction = ifelse(fill == "Score", -1, 1)
        )
    }
  
  full_name <- measure_to_names[fill]
  p <- suppressWarnings(
    p + 
    geom_quasirandom(
      aes(text = paste0(
        "ZCTA: ", zcta, "\n",
        full_name, ": ", signif(val, 4)
      )),
      groupOnX = F, alpha = bee.df$focus_alpha)+
    theme_bw()+
    xlab(measure_to_names[fill])+
    theme(
      axis.text.y = element_blank(),
      axis.title.y = element_blank(),
      axis.ticks.y = element_blank(),
      legend.position = "none",
      plot.title = element_text(hjust = .5)
    )+
    xlim(-3, 103)+
    ggtitle(paste0("Distribution of ", measure_to_names[fill], " in ", 
                   st_abbrev_to_full[st]))+
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

ui <- navbarPage(
  "Unmet Health Needs App",
  tabPanel(
    "Explore US",
    sidebarLayout(
      sidebarPanel(
        width = 3,
        HTML("<b>Welcome to the Unmet Health Needs App!</b> Select any of the options below to get started. <b>If you would like to focus on a specific Zip Code Tabulation Area (ZCTA), click on it in the map to the right or select it from the list below.</b><p>"),
        selectInput(
          "st_focus",
          "Which state would you like to focus on?",
          choices = c(states_filt, "All" = "All")
        ),
        selectInput(
          "us_map_fill",
          "Which score/measure would you like to explore?",
          choices = avail_meas_list
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
          choices = c("None" = "", avail_zctas)
        ),
        actionButton("reset_zcta_click", "Reset ZCTA Focus"),
        hr(),
        HTML("<font size = '2'>"),
        HTML(paste0(
          "Data and methodology sourced from the Health Resources and Services Administration (HRSA) Service Area Needs Assessment Methodology (SANAM), a methodology that generates a quantitative assessment of unmet need (Unmet Need Score) for primary and preventive health care. ",
          "More information on SANAM can be found <a href = 'https://bphc.hrsa.gov/programopportunities/strategic-initiatives' target = '_blank'>here</a>.<p><p>"
        )),
        uiOutput("data_info"),
        HTML(paste0("The Unmet Need Score is the weighted sum of 28 measure values, each weighted according to relative importance of the measure in estimating unmet need.<p><p>"
        )),
        HTML(paste0(
          "States currently included are: ", 
          paste(names(states_filt), collapse = ", "),
          ". Selecting \"All\" will show all included states. Note that this is slower to render.<p>")),
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
  )
)

# SERVER ----

server <- function(input, output, session) {
  # preallocate reactive values ----
  
  focus_info <- reactiveValues(
    "hl" = F,
    "ZCTA" = ""
  )
  
  st_sub <- reactiveValues(
    "st" = "NC",
    "geodat" = geodat[geodat$STATE == "NC",],
    "sanam" = sanam[sanam$STATE == "NC",]
  )
  
  us_proxy <- leafletProxy("us_map")
  
  # observe button inputs and clicks ----
  
  # update the states
  observeEvent(input$st_focus, {
    if (input$st_focus == "All"){
      st_sub$st <- "All"
      st_sub$geodat <- geodat
      st_sub$sanam <- sanam
      
      updateSelectInput(session, 
                        "zcta_choose", 
                        "Which ZCTA would you like to focus on?",
                        choices = c(
                          "None" = "", 
                          avail_zctas)
      )
    } else {
      st_sub$st <- input$st_focus
      st_sub$geodat <- geodat[geodat$STATE == input$st_focus,]
      st_sub$sanam <- sanam[sanam$STATE == input$st_focus,]
      
      updateSelectInput(session, 
                        "zcta_choose", 
                        "Which ZCTA would you like to focus on?",
                        choices = c(
                          "None" = "", 
                          avail_zctas[grepl(input$st_focus, names(avail_zctas))]
                        )
      )
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
      us_proxy <- plot_map(input$us_map_fill, st_sub$geodat, 
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
      us_proxy <- plot_map(input$us_map_fill, st_sub$geodat, 
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
  
  output$data_info <- renderUI({
    withProgress(message = "Rendering data information", {
      full_name <- measure_to_names[input$us_map_fill]
      
      HTML(paste0(
        "<font size = '2'>",
        
        "A variety of data sources are used for measures comprising the Unmet Need Score. The data currently pictured for ", 
        full_name, 
        " came from ",
        ifelse(full_name == "Unmet Need Score", 
               "various sources",
               measure_orientation[full_name, "Source"]),
        ".<p>",
        "</font>"
      ))
    })
  })
  
  # plot map based on fill
  output$us_map <- renderLeaflet({
    withProgress(message = "Rendering map", {
      plot_map(input$us_map_fill, st_sub$geodat)
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
      full_name <- measure_to_names[input$us_map_fill]
      mc <- meas_max_colors[meas_col_to_type[full_name]]
      lc <- meas_min_colors[meas_col_to_type[full_name]]
      # get the orientation for the measure
      if (input$us_map_fill != "Score"){
        ori <- s_orientation[input$us_map_fill, "Orientation"]
        ori <- ifelse(ori > 0, "higher", "lower")
      } else {
        ori <- "higher"
      }
      
      text <- paste0(
        "A ", 
        html_color(mc, "higher"),
        " value indicates ", 
        html_color(mc, paste(ori, "need")),
        "."
      )
      
      if (input$us_map_fill != "Score"){
        wt <- measure_to_type$Weight[measure_to_type$Name == full_name]
        
        text <- paste0(
          text,
          " The ", 
          html_color(mc, full_name),
          " measure has a weight of ", 
          html_color(mc, wt),
          ", indicating a ",
          html_color(mc, ifelse(wt > 5, "high", "low")),
          " contribution to the overall unmet need score."
        )
      } else {
        text <- paste0(
          text,
          " A ", 
          html_color(lc, "lower"),
          " value indicates ", 
          html_color(lc, "lower need"),
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
      plot_bee_distr(input$us_map_fill, 
                     st = st_sub$st,
                     sanam = st_sub$sanam,
                     hl = focus_info$hl, 
                     zcta_hl = focus_info$ZCTA)
    })
  })
  
  output$us_quantile <- renderTable({
    withProgress(message = "Rendering ZCTA data quantiles", {
      dist_fill <- summary(st_sub$sanam[,input$us_map_fill], digits = 4)
      
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
        full_name <- measure_to_names[input$us_map_fill]
        
        # get scores
        f_val <- 
          st_sub$sanam[st_sub$sanam$ZCTA == focus_info$ZCTA, input$us_map_fill]
        all_st_val <- st_sub$sanam[, input$us_map_fill]
        all_us_val <- sanam[, input$us_map_fill]
        
        # get colors
        if (input$us_map_fill == "Score"){
          mc <- 
            if (f_val > 50){
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
          st_abbrev_to_full[input$st_focus],", and ",
          html_color(mc, us_comp), " relative to all included states.",
          "</font></b></center>"
        ))
      }
    })
  })
}

# RUN ----

shinyApp(ui, server)
