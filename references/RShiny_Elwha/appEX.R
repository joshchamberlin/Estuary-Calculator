# Created by Seattle University student team (Masters in Data Science) winter/spring of 2024
# adapted and updated by AH Fullerton fall 2024 for display on NOAA Posit Connect server

# To do
# Add mapped metrics or temperatures on a given day/time?

# Load packages ----------------------------------------------------------------
library(shiny)
library(tidyverse)
library(maps)
library(sf)
library(readxl)
library(leaflet)
library(RColorBrewer)
library(bslib) #not sure what this is
library(dygraphs) #ditto
library(xts) #ditto
library(DT) #ditto

# Load data --------------------------------------------------------------------
load("data/stemp_data.RData")  #I think these are saved environments so that all the objects don't have to be reloaded
load("data/elwha_geo.RData")
metrics <- read.csv(paste0("data/elwha_thermal_metrics_all.csv"))
# standardize years so we can compare mon/day
metrics$first.week <- as.Date(paste0("2000-", lubridate::month(metrics$first.week), "-", lubridate::day(metrics$first.week)), format = "%Y-%m-%d")
metrics$Start <- as.Date(metrics$Start)
metrics$End <- as.Date(metrics$End)
lifestages.dat <- read.csv("data/lifestages.csv")
life.stages <- unique(lifestages.dat$Lifestage)
species <- unique(lifestages.dat$Species)

foo <- rowSums(st_wide[2:ncol(st_wide)], na.rm = T)
idx <- which(foo == 0)
if(length(idx) > 0) st_wide <- st_wide[-idx,]
foo <- rowSums(st_wide_fw[2:ncol(st_wide_fw)], na.rm = T)
idx <- which(foo == 0)
if(length(idx) > 0) st_wide_fw <- st_wide_fw[-idx,]
site_names <- colnames(st_wide)[2:ncol(st_wide)]
fwsite_names <- colnames(st_wide_fw)[2:ncol(st_wide_fw)]

# Functions ----
# Plotting metrics
fncPlotData <- function(dat, xvar, y10 = "Q10", y50 = "Q50", y90 = "Q90",
                        xlb = xvar, ylb = "", mn.lab = met, colbypop = F){
  
  if(xvar == "RKM"){
    dat <- dat[order(dat[,xvar], decreasing = T),]
  } else{
    dat <- dat[order(dat[,xvar]),]
  }
  xxvar <- dat[,xvar]
  yy10 <- dat[,y10]; yy10 <- yy10[!is.na(xxvar)]
  yy50 <- dat[,y50]; yy50 <- yy50[!is.na(xxvar)]
  yy90 <- dat[,y90]; yy90 <- yy90[!is.na(xxvar)]
  xxvar <- xxvar[!is.na(xxvar)]
  var <- colnames(dat)[ncol(dat)]
  
  ymin <- as.numeric(min(yy10, na.rm = T)) * 0.9
  ymax <- as.numeric(max(yy90, na.rm = T)) * 1.1
  ylm <- c(ymin, ymax)
  
  par(mar = c(5,5.5,3,1))
  plot(c(xxvar, rev(xxvar)), c(yy10, rev(yy90)), type = "n", las = 1, ylab = "", xlab = xlb,
       ylim = ylm, main = mn.lab)
  title(ylab = ylb, line = 4.5)
  arrows(xxvar, as.numeric(yy90), xxvar, as.numeric(yy10), code = 3, length = 0.02, angle = 90, col = "gray50")

  points(xxvar, yy50, cex = dat[,var] * 0.1, pch = 21, bg = "#CDE5E5")
  #points(xxvar, yy50, pch = 19, cex = dat[,var] * 0.05)
  # Get y-axis tick marks and add horizontal guidelines
  y_ticks <- axTicks(2)  # 2 specifies the y-axis
  abline(h = y_ticks, col = "gray", lty = 3) 
  
  # legend
  mx <- round(max(dat[,var]))
  mi <- round(min(dat[,var]))
  txt1 <- ifelse(mi == 1, " site", " sites")
  txt2 <- " sites"
  if(xvar == "RKM") txt1 <- ifelse(mi == 1, " year", " years")
  if(xvar == "RKM") txt2 <- " years"
  xx1 <- round(min(dat[,xvar]))
  ypos <- seq(ylm[1], ylm[2], length.out = 21)
  yy1 <- ypos[21]
  yy2 <- ypos[19]
  
  points(xx1, yy1, cex = (mi * 0.1), pch = 21, bg = "#CDE5E5")
  points(xx1, yy2, cex = (mx * 0.1), pch = 21, bg = "#CDE5E5")
  #points(xx1, yy1, pch = 19, cex = (mi * 0.05))
  #points(xx1, yy2, pch = 19, cex = (mx * 0.05))
  text((xx1 + 1), yy1, paste0(mi, txt1), adj = 0)
  text((xx1 + 1), yy2, paste0(mx, txt2), adj = 0)
    
}
# Printed name of metrics
fncMetricName <- function(metric = met){
  if(metric == "pExc") return ("Proportion of days exceeding threshold")
  if(metric == "durExc") return ("Days consecutaviley exceeding threshold")
  if(metric == "first.week") return ("First week exceeding threshold")
  if(metric == "daysSuitable") return ("Days within suitable range (4C to threshold)")
  if(metric == "cum.exp") return("Cumulative exposure in degree-days")
  if(metric == "IWI") return ("Minimum weekly minimum")
  if(metric == "AWI") return ("Mean weekly minimum")
  if(metric == "AWA") return ("Mean weekly mean")
  if(metric == "MWA") return ("Maximum weekly mean")
  if(metric == "AWM") return ("Mean weekly maximum")
  if(metric == "MWM") return ("Maximum weekly maximum")
  if(metric == "IWV") return ("Minimum weekly variance")
  if(metric == "AWV") return ("Mean weekly variance")
  if(metric == "MWV") return ("Maximum weekly variance")
  if(metric == "VAR") return ("Raw variance")
  if(metric == "RNG") return ("Range")
}

# UI Elements -------------------------------------------------------------------
# Cards ----
ts_cards <- list(
  card(
    full_screen = TRUE,
    card_header("Time series for one or more sites"),
    dygraphOutput("comboplot")
  ),
  card(
    full_screen = TRUE,
    card_header("Aggregated statistics across sites"),
    dygraphOutput("statplot")
  )
)

map_cards <- list(
  card(
    full_screen = TRUE,
    card_header("Locations of monitored sites"),
    leafletOutput("map")
  ),
  card(
    full_screen = TRUE,
    card_header("Time series of stream temperatures"),
    dygraphOutput("leaflet_dygraph")
  )
)

met_cards1 <- list(
  card(
    full_screen = TRUE,
    card_header("Selected thermal metric vs. year"),
    plotOutput("metric_plot1"),
    card_footer(
      "Symbols and whiskers display medians and 10th/90th percentiles across sites where data were available 
      for each year. Symbol size reflects the number of sites for which data were available each year."
    ),
    fluidRow(
      column(width = 4, offset = 2, downloadButton('download.metric1', "Data")),
      column(width = 4, offset = 2, downloadButton('download.plotmetric1', "Plot"))
    )
  ),
  card(
    full_screen = TRUE,
    card_header("Selected thermal metric vs. site"),
    plotOutput("metric_plot2"),
    card_footer(
      "Symbols and whiskers display medians and 10th/90th percentiles over years where data were available
      for a given site, plotted against river kilometer (i.e., distance upstream from the river's mouth). 
      Symbol size reflects the number of years for which data were available at each site."
    ),
    fluidRow(
      column(width = 4, offset = 2, downloadButton('download.metric2', "Data")),
      column(width = 4, offset = 2, downloadButton('download.plotmetric2', "Plot"))
    )
  ),
  card(
    card_header("Thermal metric definitions used in calculations"),
    DTOutput("metric_definitions")
  )
)



# Sidebars ----

# Sidebar panel for map-selection tab
map_filters <- sidebar(
  #Select Section
  selectInput(
    'MapSection',
    label = 'River Section',
    choices = unique(sites_df$SECTION),
    selected = unique(sites_df$SECTION),
    multiple = TRUE
  ),
  "To limit the sites displayed on the map, click on items in the box, delete them, and then add the desired section.
  Pan and zoom in the map to examine site locations. Then click on a site from the map to display its stream temperature below.",
  br(), br(), br(), br(), br(), hr(), 
  checkboxInput(
    'Show_preds',
    label = 'Show modeled data',
    value = TRUE
  ),
  "The box at the plot origin (shown with modeled data) controls the number of days for displaying a moving average.
  The range of dates displayed is controlled by the slide bar below the graph.",
  downloadButton('download.map_ts', "Download data")
)

# Sidebar panel for plot comparisons tab
plot_filters <- sidebar(
  #Select Section
  selectInput(
    'Section',
    label = 'River Section',
    choices = unique(sites_df$SECTION),
    selected = unique(sites_df$SECTION),
    multiple = TRUE
  ),
  "Click in the box above to choose (or remove) one or more river sections. Then choose the site(s) to display below.",
  checkboxInput(
    'fwsites',
    label = 'Use Foodweb Site IDs',
    value = TRUE
  ),
  
  #Select a site
  selectInput(
    'Site',
    label = 'Choose a Site',
    choices = fwsite_names,
    selected = fwsite_names,
    multiple = TRUE
  )
)

# Sidebar panel for themal metrics 
metrics_filters <- sidebar(
    selectInput(inputId = "metric.species", label = "Species:",
                choices = c("generic", "Chinook", "steelhead", "coho"),
                selected = "Chinook"),
    selectInput(inputId = "metric.lifestage", label = "Life stage:",
                choices = c("Prespawn" = "prespawn", "Incubation" = "incubat", 
                            "Rearing" = "rearing", "Fry outmigration" = "outmigr0",
                            "Parr outmigration" = "outmigr1",
                            as.character(lubridate::month(1:12, label = T))),
                selected = "prespawn"),
    selectInput(inputId = "metric.met", label = "Thermal metric:",
                choices = c("Proportion of days exceeding threshold" = "pExc", 
                            "Days consecutively exceeding threshold" = "durExc", 
                            #"First week exceeding threshold" = "first.week", 
                            "Days within suitable range (4C to threshold)" = "daysSuitable",
                            "Cumulative exposure in degree-days" = "cum.exp", 
                            "Minimum weekly minimum" = "IWI", "Mean weekly minimum" = "AWI",
                            "Mean weekly mean" = "AWA", "Maximum weekly mean" = "MWA", 
                            "Mean weekly maximum" = "AWM", "Maximum weekly maximum" = "MWM", 
                            "Minimum weekly variance" = "IWV", "Mean weekly variance" = "AWV", 
                            "Maximum weekly variance" = "MWV", "Raw variance" = "VAR", "Range" = "RNG"),
                selected = "AWA"),
    radioButtons(inputId = 'metric.habitats', label = 'Habitat:',
                choices = list("All" = "", "Mainstem" = "MS", "Floodplain" = "SC", "Tributaries" = "TR"),
                selected = "")
  ) #end sidebar
  


# Define UI --------------------------------------------------------------------
ui <- fluidPage(
  #shinythemes::themeSelector(),
  theme = bs_theme(preset = "yeti"),
  navbarPage(
  "Elwha Stream Temperatures",
  
# Tabs ----  
  tabsetPanel(
            # Tab for map-based selection and time series plot
            tabPanel("Map-based selection", 
                       fluid = TRUE,
                       page_sidebar(
                         sidebar = map_filters,
                         # display the map
                         map_cards[[1]],
                         # time series
                         map_cards[[2]]
                         ) # end page_sidebar
                       ), # end tab panel
            
              # Tab for time series Plots and comparisons
              tabPanel("Compare time series",
                       fluid = TRUE,
                       page_sidebar(
                         sidebar = plot_filters,
                         # plots of raw data
                         ts_cards[[1]],
                         # plots of aggregated metrics
                         ts_cards[[2]]
                         ) # end page_sidebar
                       ), # end tab panel
            
              tabPanel("Thermal metrics",
                       fluid = TRUE,
                       page_sidebar(
                         sidebar = metrics_filters,
                         layout_columns( 
                           #plots
                           met_cards1[[1]],
                           met_cards1[[2]]
                         ), # end layout_columns
                         layout_columns(
                           met_cards1[[3]]
                         )
                         ) # end page_sidebar
                      ), # end tabPanel
              
              #Tab for Information/Acknowledgements
              tabPanel("Metadata",
                       fluid = TRUE,
                       mainPanel(
                         tags$h1(" "),
                         tags$h2("Observed data"),
                         "Empirical observations of stream temperature were collected through a collaborative effort. 
                         Data loggers were installed, maintained, and downloaded by individuals from the following 
                         organizations over the years. Thank you for your contribution in making these data accessible  
                         to all interested parties. ",
                         br(), 
                         br(), tags$a(href="https://www.elwha.org/departments/natural-resources/fisheries", "Lower Elwha Klallam Tribe"), 
                         br(), tags$a(href="https://www.fisheries.noaa.gov/region/west-coast/northwest-science", "NOAA Northwest Fisheries Science Center"),
                         br(), tags$a(href="https://www.nps.gov/olym/index.htm", "Olympic National Park"),
                         br(), tags$a(href="https://www.seattleu.edu/science-engineering", "Seattle University"),
                         br(), tags$a(href="https://www.clallamcountywa.gov/901/Streamkeepers", "Clallam County Streamkeepers"),
                         br(), tags$a(href="https://www.fws.gov", "US Fish & Wildlife Service"),
                         br(), tags$a(href="https://www.wildedgefarm.com", "Wild Edge Farm"),
                         br(), tags$a(href="https://www.dva.wa.gov/vcc", "Veterans Conservation Corps"),
                         br(), "Private citizens including K. Denton, and the Liermann family",
                         br(),
                         hr(),
                         tags$h2("Modeled data"),
                         "Predicted stream temperatures for the Elwha River watershed were extracted from the following publication:",
                         br(), br(),
                         "Siegel, JE, AH Fullerton, AM FitzGerald, D Holzer, CE Jordan. 2023. 
                         Daily stream temperature predictions for free-flowing streams in the Pacific Northwest, USA. 
                         PLoS Water2(8): e0000119. https://doi.org/10.1371/ journal.pwat.0000119.", 
                         br(), tags$a(href="https://doi.org/10.1371/journal.pwat.0000119", "Full text"),
                         br(), br(),
                         "Modeled data (shown on the first panel) were aligned with empirical data based on the closest stream reach from the National Hydrography Dataset version 2.
                          Because these reaches can be long, there can be multiple empirical observations associated with the same predicted temperatures. 
                          As well, temperature monitored in floodplain reaches was aligned spatially with the nearest predicted temperature in mainstem reaches.
                          Some monitored locations were not able to be associated with a predicted temperature."

                       ) # End main panel
              ) # End tab panel
        ) # End tabsetPanel
  ) # End navbar page
)# End fluid page
      
# Define server ----------------------------------------------------------------
server <- function(input, output, session) {
  # Reactive function to update the input source for plots
  updateSource <- reactive({
    return(input)
  })
  
# Tab with map-based selection ----
  # Reactive function to update the input source for map plots
  updateMapSource <- reactive({
    return(input)
  })
  
  # Make leaflet markers reactive to input selecting the Section (Habitat)
  leaflet_marks <- reactive({
    sites_df[sites_df$SECTION %in% updateMapSource()$MapSection,]
  })
  
  # leaflet map
  output$map <- renderLeaflet({
    leaflet() %>%
      #Basemap
      addProviderTiles(providers$OpenStreetMap) %>%
      setView(lng = -123.5596,
              lat = 48.03,
              zoom = 10) %>%
      addPolygons(data = elwha_boundaries, weight = 3,col = 'lightblue') %>%
      addPolylines(data = elwha_streams, weight = 3,col = 'lightblue')%>%
       # add site markers, show site names when clicked
      clearMarkers() %>% 
      addCircleMarkers(
        # plot filtered leaflet markers
        data = leaflet_marks(),
        ~ LONG,
        ~ LAT,
        weight = 1,
        opacity = 10,
        popup =  ~ paste0(Site, "; ", FW_Site),
        options = markerOptions(riseOnHover = TRUE)
      ) %>% 
      # add minimap for context
      addMiniMap(toggleDisplay = TRUE)
  })
  
  # Create reactive object for leaflet data
  leaflet_data <- reactive({
    # Ensure the data is not empty and avoid error message
    validate(
      need(input$map_marker_click$lat != "", "Please select a site from the map to get started.")
    )

    # Get lat and long from marker click
    lat <- input$map_marker_click$lat
    lng <- input$map_marker_click$lng

    # Filter previously merged dataset
    filtered <- merged_data[merged_data$LAT %in% lat & merged_data$LONG %in% lng,] 
  })
  
  output$leaflet_dygraph <- renderDygraph({
    plotting_data <- leaflet_data()
    plotting_data<- plotting_data[,c("Date", "Temp", "prd.stream_temp")]
    plotting_data <- xts::as.xts(plotting_data)

    if (input$Show_preds) {
    dy <- dygraph(plotting_data, x = "Date") %>% 
      dySeries("prd.stream_temp", label = "Predicted", color = "#3182bd") %>% 
      dySeries("Temp", label = "Observed", color = "#318") %>% 
      dyRoller(showRoller = T, rollPeriod = 1) %>%
      dyRangeSelector() %>%
      dyLegend(width = 160, labelsSeparateLines = T) %>%
      dyAxis("y", label = "Stream temperature (C)")
      #dyAxis("y", valueRange = c(0,30))
    } else {
      plotting_data <- plotting_data[, "Temp"]
      #plotting_data <- plotting_data[!is.na(plotting_data[, "Temp"]),]
      dy <- dygraph(plotting_data, x = "Date") %>% 
        dySeries("Temp", label = "Observed", color = "#318") %>% 
        dyRangeSelector() %>%
        dyLegend(width = 160, labelsSeparateLines = T) %>%
        dyAxis("y", label = "Stream temperature (C)")
    }
    return(dy)
  })
  
  # Download data for the site selected on the map
  
  data2save <- reactive({
    dat <- leaflet_data()
  }
  )
    
  output$download.map_ts <- downloadHandler(
    filename = function(){paste0(data2save()$Site[1], "_", data2save()$FW_Site[1], ".csv")}, 
    content = function(fname){
      write.csv(data2save(), fname, row.names = FALSE)
    }
  )

# Tab with time series comparisons ----
  
  #Make sites reactive to section
    newSites <- reactive({
      if(updateSource()$fwsites == T){
        ss <- sites_df[sites_df$FW_Site %in% fwsite_names,]
        ss %>% filter(SECTION %in% updateSource()$Section & !is.na(FW_Site)) %>% select(FW_Site)
      } else {
        ss <- sites_df[sites_df$Site %in% site_names,]
        ss %>% filter(SECTION %in% updateSource()$Section & !is.na(Site)) %>% select(Site)
      }
    })

  observeEvent(input$Section, {
    updateSelectInput(session, "Site", choices = newSites())
  })
  observeEvent(input$fwsites, {
    updateSelectInput(session, "Site", choices = newSites())
  })
  
  #Filter for selected site
  selected_data <- reactive({
    
    validate(
      need(input$Site != "", "Please select a site from the sidebar")
    )
    
    if(input$fwsites == T){
      st_wide_fw %>% select('Date', all_of(updateSource()$Site))
    } else {
      st_wide %>% select('Date', all_of(updateSource()$Site))
    }
  })
  
  stats <- reactive({
    selected_data() %>% rowwise(Date) %>%
      summarize(
        mean = rowMeans(pick(where(is.numeric)), na.rm = TRUE),
        min = min(pick(where(is.numeric)), na.rm = TRUE),
        max = max(pick(where(is.numeric)), na.rm = TRUE)
      )
  })
  
  #Generate plot
  output$comboplot <- renderDygraph({
    dygraph(selected_data()) %>% 
    dyRangeSelector() %>%
    dyLegend(width = 160, labelsSeparateLines = T) %>%
    dyAxis("y", label = "Stream temperature (C)")
    
  })
  
  output$statplot <- renderDygraph({
    dygraph(stats()) %>% 
    dyRangeSelector() %>%
    dyLegend(width = 160, labelsSeparateLines = T) %>%
    dyAxis("y", label = "Stream temperature (C)")
  })

# Tab with metrics ----
  
  # Make drop-down choice of year life stages upon user input of species
  metric.lifestage = reactive({
    if(updateSource()$metric.species == "generic") {
       as.character(lubridate::month(1:12, label = T))
    } else {
      c("Prespawn" = "prespawn", "Incubation" = "incubat", 
        "Rearing" = "rearing", "Fry outmigration" = "outmigr0",
        "Parr outmigration" = "outmigr1")
    }
  })
  
  observeEvent(input$metric.species, {
    updateSelectInput(session, "metric.lifestage", choices = metric.lifestage())
  })
  
  updateData.metrics1 <- reactive({
    sp <- updateSource()$metric.species
    ls <- updateSource()$metric.lifestage
    met <- updateSource()$metric.met
    hab <- updateSource()$metric.habitats
    if(is.na(hab)){
      mdat <- metrics[metrics$Life.stage %in% ls & metrics$Species %in% sp, c("Site", "Year", met)]
    } else {
      mdat <- metrics[metrics$Life.stage %in% ls & metrics$Species %in% sp & metrics$Habitat %in% hab, c("Site", "Year", met)]
    }
    if(class(mdat[,met]) == "Date"){mdat[,met][mdat[,met] == as.Date("1900-01-01")] <- NA}
    dat <- mdat %>% group_by(Year) %>% summarise(q = list(quantile(.data[[met]], 
                                                                   probs = c(0,0.1, 0.25, 0.5, 0.75, 0.9, 1), na.rm = T))) %>% unnest_wider(q)
    colnames(dat) <- c("Year", "Min", "Q10", "Q25", "Q50", "Q75", "Q90", "Max")
    d1 <- unique(mdat[,c("Site", "Year")])
    d2 <- table(d1)
    d3 <- colSums(d2)
    sites.per.year <- cbind.data.frame("Year" = as.numeric(names(d3)), as.numeric(d3))
    colnames(sites.per.year)[ncol(sites.per.year)] <- "NoSites"
    sites.per.year[,"Year"] <- as.integer(sites.per.year[,"Year"])
    dat <- dplyr::left_join(dat, sites.per.year, by = "Year")
    dat <- as.data.frame(dat)
    mdat <- dplyr::left_join(mdat, sites_df[, c("Site", "FW_Site", "RKM")], by = "Site")
    mdat <- as.data.frame(mdat)
    
    return(list(dat, mdat))
  })
  
  updatePlot.metrics1 <- reactive({
    sp <- updateSource()$metric.species
    ls <- updateSource()$metric.lifestage
    met <- updateSource()$metric.met
    
    df <- updateData.metrics1()[[1]]
    
    par(oma = rep(0.5, 4), mar = c(4,4,2,1), las = 1)
    if(nrow(df) > 0){
      fncPlotData(dat = df, xvar = "Year", mn.lab = paste0(sp, ", ", ls), ylb = fncMetricName(met), xlb = "Year")
    } else{
      plot(1:nrow(df) ~ df[,"Year"], type = 'n', yaxt = 'n', xlab = "", main = "There are no data to display")
    }
    recordPlot()
  })
  
  output$metric_plot1 <- renderPlot({
    updatePlot.metrics1()
  })
  
  output$download.metric1 <- downloadHandler(
    filename = function(){paste0(updateSource()$metric.species, "_", updateSource()$metric.lifestage, "_", updateSource()$metric.met, "_year.csv")}, 
    content = function(fname){
      write.csv(updateData.metrics1()[[2]], fname, row.names = FALSE)
    }
  )
  
  output$download.plotmetric1 <- downloadHandler(
    filename = function(){paste0(updateSource()$metric.species, "_", updateSource()$metric.lifestage, "_", updateSource()$metric.met, "_year.png")},
    content = function(file){
      png(file, width = 6, height = 4, res = 150, units = "in")
      replayPlot(updatePlot.metrics1())
      dev.off()
    }
  )
  
  updateData.metrics2 <- reactive({
    sp <- updateSource()$metric.species
    ls <- updateSource()$metric.lifestage
    met <- updateSource()$metric.met
    hab <- updateSource()$metric.habitats
    if(is.na(hab)){
      mdat <- metrics[metrics$Life.stage %in% ls & metrics$Species %in% sp, c("Site", "Year", met)]
    } else {
      mdat <- metrics[metrics$Life.stage %in% ls & metrics$Species %in% sp & metrics$Habitat %in% hab, c("Site", "Year", met)]
    }
    if(class(mdat[,met]) == "Date"){mdat[,met][mdat[,met] == as.Date("1900-01-01")] <- NA}
    mdat <- dplyr::left_join(mdat, sites_df[, c("Site", "RKM")], by = "Site")
    dat <- mdat %>% group_by(Site) %>% summarise(q = list(quantile(.data[[met]], 
                    probs = c(0,0.1, 0.25, 0.5, 0.75, 0.9, 1), na.rm = T))) %>% unnest_wider(q)
    colnames(dat) <- c("Site", "Min", "Q10", "Q25", "Q50", "Q75", "Q90", "Max")
    d1 <- unique(mdat[,c("Site", "Year")])
    d2 <- t(table(d1))
    d3 <- colSums(d2)
    years.per.site <- cbind.data.frame("Site" = names(d3), as.numeric(d3))
    colnames(years.per.site)[ncol(years.per.site)] <- "NoYears"
    dat <- dplyr::left_join(dat, years.per.site, by = "Site")
    dat <- as.data.frame(dat)
    mdat <- dplyr::left_join(mdat, sites_df[, c("Site", "FW_Site")], by = "Site")
    mdat <- as.data.frame(mdat)
    
    return(list(dat, mdat))
    
  })
  
  updatePlot.metrics2 <- reactive({
    sp <- updateSource()$metric.species
    ls <- updateSource()$metric.lifestage
    met <- updateSource()$metric.met
    
    df <- updateData.metrics2()[[1]]
    df <- dplyr::left_join(df, sites_df[, c("Site", "RKM")], by = "Site")
    df <- df[,c(1:8, 10, 9)] #reorder to what the plot function expects
    
    par(oma = rep(0.5, 4), mar = c(4,4,2,1), las = 1)
    if(nrow(df) > 0){
      fncPlotData(dat = df, xvar = "RKM", mn.lab = paste0(sp, ", ", ls), ylb = fncMetricName(met), xlb = "River kilometer")
    } else{
      plot(1:nrow(df) ~ df[,"RKM"], type = 'n', yaxt = 'n', xlab = "", main = "There are no data to display")
    }
    recordPlot()
  })
  
  output$metric_plot2 <- renderPlot({
    updatePlot.metrics2()
  })
  
  output$download.metric2 <- downloadHandler(
    filename = function(){paste0(updateSource()$metric.species, "_", updateSource()$metric.lifestage, "_", updateSource()$metric.met, "_rkm.csv")}, 
    content = function(fname){
      write.csv(updateData.metrics2()[[2]], fname, row.names = FALSE)
    }
  )
  
  output$download.plotmetric2 <- downloadHandler(
    filename = function(){paste0(updateSource()$metric.species, "_", updateSource()$metric.lifestage, "_", updateSource()$metric.met, "_rkm.png")},
    content = function(file){
      png(file, width = 6, height = 4, res = 150, units = "in")
      replayPlot(updatePlot.metrics2())
      dev.off()
    }
  )
  
  table_data <- lifestages.dat[,1:8]
  output$metric_definitions <- renderDT({
    datatable(table_data)
  })
  
}
# Create a Shiny app object ----------------------------------------------------
shinyApp(ui = ui, server = server)