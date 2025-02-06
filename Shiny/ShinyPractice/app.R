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
delta <- read_sf("data/SnoDelta.shp")
st_crs(SnoDelta)


# Create Map -------------------------------

delta <- st_transform(delta, crs = '+proj=longlat +datum=WGS84')

map <-leaflet() %>% 
  addTiles(group = "Street") %>% #basemap
  addProviderTiles(providers$Esri.WorldImagery,
                   group = "Satellite") %>% #satelite base map
  setView(lng =-122.214, lat = 48.024, zoom = 10) %>% 
  addPolygons(data = delta,
              popup = "Snohomish Delta", group = "Snohomish Delta") %>% 
  addPolygons(data = restoration) %>% 
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
             selectInput(inputId = " type",
                         label = "Select project type:",
                         choices = c("Restoration", "Dike", "Tide Gate")),
             fileInput("file1",
                       "Upload Project Design (.shp)",
                       multiple = T,
                       accept = ".shp"),
             textInput("Design", "Design features"),
             leafletOutput("map")
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
  output$summary <- renderPrint({
    dataset <- get(input$dataset, "package:datasets")
    summary(dataset)
  })
  
  output$table <- renderTable({
    dataset <- get(input$dataset, "package:datasets")
    dataset
  })
  
  output$map <- renderLeaflet({
    map
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
