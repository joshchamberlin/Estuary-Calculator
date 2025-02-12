#
# This is Catalina practicing building a Shiny web application. 
#Last edited: 2/5

# Load packages -------------------------
library(shiny)
library(shinydashboard)
library(units)
library(tidyverse)
library(maps)
library(sf)
library(readxl)
library(leaflet)
library(RColorBrewer)
library(bslib) #UI themes

options(shiny.maxRequestSize = 30*1024^2) #increase the file upload size to 30MB

# Load Data -------------------------------
SnoDelta <- read_sf("data/SnoDelta")
st_crs(SnoDelta) #check CRS
SnoDelta <- st_transform(SnoDelta, crs = '+proj=longlat +datum=WGS84')

Hveg <- read_sf("data/Historic_Veg")
Hveg <- st_transform(Hveg, crs = '+proj=longlat +datum=WGS84')


# User Interface ----------------------------  

ui <- dashboardPage(
  #Page Title
  dashboardHeader(title = "Estuary Habitat Values Model"),
  
  #Menu Outline
  dashboardSidebar(
    sidebarMenu(
      menuItem("Home", tabName = "home", icon = icon("home"), startExpanded = T, 
               menuSubItem("Welcome", tabName = "welcome"),
               menuSubItem("How it works", tabName = "how2"),
               menuSubItem("Background and Purpose", tabName = "background"),
               menuSubItem("Glossary", tabName = "gloss"),
               menuSubItem("Uses and Limitations", tabName = "disclaimer")), 
      menuItem("Project Design", tabName = "design", icon = icon("globe", lib = "glyphicon")), 
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
            textOutput(outputId = "area")
            ),
    
    #-----------------------------
    #About Us Tab
    tabItem(tabName = "about")
    )
))


?ls
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
    
    #change projection to be compatible with leaflet
    st_transform(footprint, crs = '+proj=longlat +datum=WGS84')

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
    st_transform(channel, crs = '+proj=longlat +datum=WGS84')
    
  })
  

  #create map with users uploaded file
  output$map <- renderLeaflet({
    
    #get the bounding box so that the leaflet will rescale to the size of the uploaded polygon
    bounds <- footprint() %>% 
      st_bbox() %>% 
      as.character()
    
    leaflet() %>%
      addTiles(group = "Street") %>% #basemap
      addProviderTiles(providers$Esri.WorldImagery,
                       group = "Satellite") %>% #satelite base map
      
      #Setting the zoom
      fitBounds(bounds[1], bounds[2], bounds[3], bounds[4]) %>% 
      
      #User uploaded footprint
      addPolygons(data = footprint(), 
                  color = "red", fill = F,
                  popup = "Project Site", group = "Project Site") %>%
      
      #User uploaded channel features
      addPolygons(data = channel() %>% filter(FeatType == "Tidal Channel"),
                  fill = "blue", stroke = F, fillOpacity = 1,
                  popup = "Channels", group = "Channels") %>% 
    
      #Interactive map features
      addLayersControl(baseGroups = c("Satelite", "Street"), #switch map view
                       overlayGroups = c("Project Site", "Channels"), #toggle on and off the project polygon
                       options = layersControlOptions(collapsed = F)) %>%
      addScaleBar()
  })

#HEA TAB -----------------------------    
  #REACTIVE
  #Intersect the uploaded polygon with the Snohomish Delta to extract metrics
  intersection <- reactive(st_intersection(x=footprint(), y=SnoDelta))
  
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

#end of server
}

# Run the application ------------------------ 
shinyApp(ui = ui, server = server)
