#
# This is Catalina practicing building a Shiny web application. 
#Last edited: 2/5

# Load packages -------------------------
library(shiny)
library(shinydashboard)
library(tidyverse)
library(maps)
library(sf)
library(readxl)
library(leaflet)
library(RColorBrewer)
library(bslib) #UI themes

options(shiny.maxRequestSize = 30*1024^2) #increase the file upload size to 30MB

# Load Data -------------------------------
#delta <- read_sf("data/SnoDelta.shp")
#st_crs(SnoDelta)


# Create Map -------------------------------

delta <- st_transform(delta, crs = '+proj=longlat +datum=WGS84')

map <-leaflet() %>%
  addTiles(group = "Street") %>% #basemap
  addProviderTiles(providers$Esri.WorldImagery,
                   group = "Satellite") %>% #satelite base map
  setView(lng =-122.214, lat = 48.024, zoom = 10) %>%
  addPolygons(data = delta,
              popup = "Snohomish Delta", group = "Snohomish Delta") %>%
  addLayersControl(baseGroups = c("Satelite", "Street"),
                   overlayGroups = c("Snohomish Delta"),
                   options = layersControlOptions(collapsed = F)) %>%
  addScaleBar()


map

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
    # Map Tab
    tabItem(tabName = "design",
            #Dropdown to select project type
             selectInput(inputId = " type",
                         label = "Select project type:",
                         choices = c("Select","Restoration", "Dike", "Tide Gate")),
            #User file upload
             fileInput(inputId = "mapdata",
                       label = "Upload Project Design (.shp)",
                       multiple = T,
                       accept = c(".shp", ".dbf", ".sbn", ".sbx", ".shx", ".prj")),
             textInput("Design", "Design features"),
             leafletOutput(outputId = "map", height = 900)
            ),
    
    #------------------------------
    # HEA Tab
    tabItem(tabName = "hea"
            ),
    
    #-----------------------------
    #About Us Tab
    tabItem(tabName = "about")
    )
))


?ls
# Server ----------------------------------
server <- function(input, output, session) {
  
  #Save user's map upload to a directory and read it into the working environment
  map <- reactive({
    req(input$mapdata)
    shpdf <- input$mapdata
    tempdirname <- dirname(shpdf$datapath[1])
    for (i in 1:nrow(shpdf)) {
      file.rename(
        shpdf$datapath[i],
        paste0(tempdirname, "/", shpdf$name[i])
      )
    }
    map <- read_sf(paste(tempdirname, shpdf$name[grep(pattern = ".shp", shpdf$name)],
                         sep="/"))
    
    #change projection to be compatable with leaflet
    map <- st_transform(map, crs = '+proj=longlat +datum=WGS84')
    
    map
  })


  #create map with users uploaded file
  output$map <- renderLeaflet({
    if (is.null(data()) | is.null(map())) {
      return(NULL)
    }
    map <- map()
    
    leaflet() %>%
      addTiles(group = "Street") %>% #basemap
      addProviderTiles(providers$Esri.WorldImagery,
                       group = "Satellite") %>% #satelite base map
      setView(lng =-122.214, lat = 48.024, zoom = 10) %>%
      addPolygons(data = map,
                  popup = "Project Site", group = "Project Site") %>%
      addLayersControl(baseGroups = c("Satelite", "Street"),
                       overlayGroups = c("Project Site"),
                       options = layersControlOptions(collapsed = F)) %>%
      addScaleBar()
    
  })
}

# Run the application ------------------------ 
shinyApp(ui = ui, server = server)
