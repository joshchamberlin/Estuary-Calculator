#
# This is Catalina practicing building a Shiny web application. 
#Last edited: 1/30

# Load packages -------------------------
library(shiny)
library(tidyverse)
library(maps)
library(sf)
library(readxl)
library(leaflet)
library(RColorBrewer)
library(bslib) #UI themes

options(shiny.maxRequestSize = 30*1024^2) #increase the file upload size to 30MB

# Load Data -------------------------------
SnoDelta <- read_sf("data/SnoDelta.shp")

st_crs(SnoDelta)

# Create Map -------------------------------

data_map <- st_transform(SnoDelta, crs = '+proj=longlat +datum=WGS84')

map <-leaflet() %>% 
  addTiles() %>% 
  setView(lng =-122.214, lat = 48.024, zoom = 10) %>% 
  addPolygons(data = data_map)


# User Interface ----------------------------  

ui <- fluidPage(
  theme = bs_theme(preset = "yeti"), #UI theme
  navbarPage("Estuary Habitat Values Model"), #Page Title
  
  # Tabs ---------------------------------
  tabsetPanel(
    tabPanel("Information",
             mainPanel("This is where we can give information about the Estuary 
                       Calculator Tool and link to important reference documents",
                       br(), tags$a(href="https://www.fisheries.noaa.gov/west-coast/habitat-conservation/puget-sound-nearshore-habitat-conservation-calculator", "User Guide"))),
    tabPanel("Project Design",
             fileInput("file1",
                       "Upload Project Design (.shp)",
                       multiple = T,
                       accept = ".shp"),
             textInput("Design", "Design features"),
             leafletOutput("map")),
    tabPanel("HEA Valuation"))
)


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
