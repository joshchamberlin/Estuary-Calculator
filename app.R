#
# This is Catalina practicing building a Shiny web application. 
#Last edited: 3/19

# Load packages -------------------------
library(shiny)
  shinyOptions(cache = cachem::cache_disk("./app_cache/cache/")) #Posit recommends caching to disk
library(shinydashboard)
library(units)
library(tidyverse)
library(maps)
library(sf)
library(lwgeom)
library(readxl)
library(leaflet)
library(RColorBrewer)
library(bslib) #UI themes
library(rsconnect) #for deplying app

options(shiny.maxRequestSize = 30*1024^2) #increase the file upload size to 30MB

# Deploying app to Posit -----------------
#re-run these lines to update app for the connect site
#Connect Git-backed publishing does not support Git Large File Storage (LFS)
# list.files()
# writeManifest()
# list.files()

# Load Data -------------------------------
site <- read_sf("data/CedarGroveMitigation")
SnoDelta <- read_sf("data/SnoDelta")
Hveg <- read_sf("data/Historic_Veg")
l_connect <- read_sf("data/Landscape_Connectivity")

# Source dependent scripts
source(paste(getwd(), "/code/landscape_connectivity.R", sep = ""))
source(paste(getwd(), "/code/vegetation.R", sep = ""))

# Unify CRS
st_crs(SnoDelta) #check CRS
SnoDelta <- st_transform(SnoDelta, crs = st_crs(site))

Hveg <- st_transform(Hveg, crs = st_crs(site)) %>% 
  st_make_valid()

l_connect <- st_transform(l_connect, crs = st_crs(site)) 

#################################################
# User Interface ----------------------------  

ui <- dashboardPage(
  #Page Title
  dashboardHeader(title = "Estuary Habitat Values Model"),
  
  #Menu Outline
  dashboardSidebar(
    sidebarMenu(
      menuItem("Home", tabName = "home", icon = icon("home"), startExpanded = F, 
               menuSubItem("Welcome", tabName = "welcome"),
               menuSubItem("How it works", tabName = "how2"),
               menuSubItem("Background and Purpose", tabName = "background"),
               menuSubItem("Glossary", tabName = "gloss"),
               menuSubItem("Uses and Limitations", tabName = "disclaimer")), 
      menuItem("Project Design", tabName = "design", icon = icon("globe", lib = "glyphicon")),
      menuItem("Metrics", tabName = "metrics", icon = icon("cog", lib = "glyphicon"), startExpanded = F,
               menuSubItem("Site Metrics", tabName = "site",),
               menuSubItem("Landscape Metrics", tabName = "landscape")),
      menuItem("Habitat Equivalency Analysis", tabName = "hea", icon = icon("hourglass", lib = "glyphicon")),
      menuItem("About Us", tabName = "about", icon = icon("info-sign", lib = "glyphicon"))
    )
  ), 
  
  # This organizes each of the "body" pages -> the real interactive pieces of the app
  dashboardBody(
    
  # Tabs ---------------------------------
  # Home Tab
  tabItems(
    tabItem(tabName = "welcome",
            box(title = h3("Welcome to the Estuary Habitat Values App for ESA Section 7 Consultations"),
                width = 12),
            br(),
            "This is the landing page. We can provide a brief summary and link to reference documents.",
            br(), 
            tags$a(href="https://www.fisheries.noaa.gov/west-coast/habitat-conservation/puget-sound-nearshore-habitat-conservation-calculator",
                   "User Guide"),
            br(), tags$a(href="https://www.fisheries.noaa.gov/west-coast/habitat-conservation/puget-sound-nearshore-habitat-conservation-calculator",
                         "Rationale"),
            br(), br(),
            tags$img(src = "conceptualdiagram.png", height = "600px") #html tag
                       ),
    tabItem(tabName = "how2"
            ),
    tabItem(tabName = "background"
            ),
    tabItem(tabName = "gloss"
            ),
    tabItem(tabName = "disclaimer"
            ),
    
    # -----------------------------
    # Project Design Tab
    tabItem(tabName = "design",
            #Dropdown to select project type
             selectInput(inputId = " type",
                         label = "Select project type:",
                         choices = c("Select","Restoration", "Dike", "Tide Gate")),
            #User total area polygon file upload
             fileInput(inputId = "footprintdata",
                       label = "Upload a shapefile of the project footprint",
                       multiple = T, 
                       accept = c(".shp", ".dbf", ".sbn", ".sbx", ".shx", ".prj")),
            #User channel polygon file upload
            fileInput(inputId = "channeldata",
                      label = "Upload a shapefile of the channel polygons",
                      multiple = T,
                      accept = c(".shp", ".dbf", ".sbn", ".sbx", ".shx", ".prj")),
            leafletOutput(outputId = "map", height = 900)
            ),
    #--------------------------------
    # Metrics Tab
    tabItem(tabName = "site",
            leafletOutput(outputId = "veginteract"),
            plotOutput(outputId = "vegplot")
            ),
    tabItem(tabName = "landscape"
            ),
    
    #------------------------------
    # HEA Tab
    tabItem(tabName = "hea",
            box(title = h3("Project Site Metrics"),
                width = 12),
            "Annual sediment load:",
            textOutput(outputId = "sediment"),
            "Mean annual freshwater discharge:",
            textOutput(outputId = "discharge"),
            "Area (acres)",
            textOutput(outputId = "area"),
            "Vegetation Habitat Type:",
            textOutput(outputId = "veg"),
            "Landscape Connectivity:",
            textOutput(outputId = "lcon")
            ),
    
    #-----------------------------
    #About Us Tab
    tabItem(tabName = "about")
    )
))


###############################################
# Server ----------------------------------
server <- function(input, output, session) {

# Project Design Tab -------------------------------    
  #REACTIVE
  #Save user's map upload to a directory and read it into the working environment
  footprint <- reactive({
    req(input$footprintdata)
    shpdf <- input$footprintdata
    tempdirname <- dirname(shpdf$datapath[1])
    for (i in 1:nrow(shpdf)) {
      file.rename(
        shpdf$datapath[i],
        paste0(tempdirname, "/", shpdf$name[i])
      )
    }
    footprint <- read_sf(paste(tempdirname, shpdf$name[grep(pattern = ".shp", shpdf$name)],
                         sep="/"))
    
    #change projection to match landscape connectivity layer
    st_transform(footprint, crs = st_crs(site))

  })
  
  channel <- reactive({
    req(input$channeldata)
    shpdf <- input$channeldata
    tempdirname <- dirname(shpdf$datapath[1])
    for (i in 1:nrow(shpdf)) {
      file.rename(
        shpdf$datapath[i],
        paste0(tempdirname, "/", shpdf$name[i])
      )
    }
    channel <- read_sf(paste(tempdirname, shpdf$name[grep(pattern = ".shp", shpdf$name)],
                               sep="/"))
    
    #change projection to be compatible with leaflet
    st_transform(channel, crs = st_crs(site))
    
  })
  

  #create map with users uploaded file
  output$map <- renderLeaflet({
    
    #get the bounding box so that the leaflet will rescale to the size of the uploaded polygon
    bounds <- footprint() %>% 
      st_transform(crs = '+proj=longlat +datum=WGS84') %>% 
      st_bbox() %>% 
      as.character()
    
    leaflet() %>%
      addTiles(group = "Street") %>% #basemap
      addProviderTiles(providers$Esri.WorldImagery,
                       group = "Satellite") %>% #satelite base map
      
      #Setting the zoom
      fitBounds(bounds[1], bounds[2], bounds[3], bounds[4]) %>% 
      
      #User uploaded footprint
      addPolygons(data = footprint() %>% st_transform(crs = '+proj=longlat +datum=WGS84'), #transforming CRS to be compatible with leaflet
                  color = "red", fill = F,
                  popup = "Project Site", group = "Project Site") %>%
      
      #User uploaded channel features
      addPolygons(data = channel() %>% filter(FeatType == "Tidal Channel") %>% st_transform(crs = '+proj=longlat +datum=WGS84'),
                  fill = "blue", stroke = F, fillOpacity = 1,
                  popup = "Channels", group = "Channels") %>% 
    
      #Interactive map features
      addLayersControl(baseGroups = c("Satelite", "Street"), #switch map view
                       overlayGroups = c("Project Site", "Channels"), #toggle on and off the project polygon
                       options = layersControlOptions(collapsed = F)) %>%
      addScaleBar()
  })

# METRICS TAB --------------------------
  #Site Metrics
  output$veginteract <- renderLeaflet({
    habitat_change_leaflet
  })
  
  output$vegplot <- renderPlot({
    habitat_change_plot
  })

  
  #Landscape Metrics
  
  

# HEA TAB -----------------------------    
  # REACTIVE SEGMENTS
  #Intersect the uploaded polygon with the Snohomish Delta to extract metrics
  intersection <- reactive(st_intersection(x=footprint(), y=SnoDelta))
  
  #Vegetation
  HabitatVeg <- reactive({st_intersection(x = footprint(), y = Hveg)})
  
  #Prep the footprint for landscape connectivity calculation  -------------
  landconnect <- reactive({
    line <- st_nearest_points(footprint(), st_union(l_connect))
    
    point <- st_intersection(l_connect, line) %>% 
      st_as_sf()
    
    startingline <- st_collection_extract(st_split(l_connect, point), "LINESTRING") %>% 
                               filter(fid == point$fid) %>% #This filters for the line segement that we sliced above
                               slice(2) #this drops one of the segments... but I'm not sure if this will work 100% of the time?
    c1 <- st_length(startingline) * startingline$bi_order
    i1 <- startingline$i
    #this function is from the dependency script landscape_connectivity.R
    connectivity(l_connect, i1, c1)
    
  })
  
  
  # OUTPUTS
  #Extract sediment load
  output$sediment <- renderText({
    intersection()$AnnualSedi
  })
  
  #Extract freshwater discharge
  output$discharge <- renderText({
    intersection()$MeanAnnual
    
  })
  
  #Extract user project site area
  output$area <- renderText({
    area <- st_area(footprint())
    set_units(area, "acres")
  })
  
  output$veg <- renderText({
    HabitatVeg()$Veg
  })
  
  output$lcon <-renderText({
    landconnect()
  })

#end of server
}

###############################################
# Run the application ------------------------ 
shinyApp(ui = ui, server = server)
