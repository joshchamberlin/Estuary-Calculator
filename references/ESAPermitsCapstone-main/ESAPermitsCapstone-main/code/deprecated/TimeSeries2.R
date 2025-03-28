# Making Time Series Infographics - Percentage
# Alana Santana and Rory Spurr
#=============================================================
#Reading in packages
library(shiny)
library(ggplot2)
library(sf)
library(dplyr)
library(tidyverse)
library(leaflet)
library(NMFSResPermits)
library(plotly)
library(viridis)
sf_use_s2(FALSE)
#==============================================================
#Sourcing Script
#setwd("~/GitHub/ESA_Permits_Capstone")
source(paste(getwd(), "/code/dependencies/Reading and Filtering.R", sep = ""))
source(paste(getwd(), "/code/dependencies/TSPreAppCode.R", sep = ""))
#==============================================================
#Shiny Integration
ui <-  fluidPage(
  titlePanel("Percent Reported/Authorized Take (Lethal/Non-Lethal) per Year"),
  sidebarLayout(
    
    sidebarPanel(
      radioButtons(inputId = "LifeStage", label = "Choose a lifestage",
                   choices = c("Adult", "Juvenile")),
      radioButtons(inputId = "Production", label = "Choose an Origin",
                   choices = c("Natural", "Listed Hatchery")),
      selectInput(inputId = "ESU", label = "Choose an ESU to View",
                  choices = levels(df$ESU), 
                  multiple = F)),
    mainPanel(
    h5("These charts display the percentage of total authorized take (lethal/non-lethal) used (reported) per year"),
    h6("*Data is only showing what was reported, not complete"), 
      plotlyOutput("plot1"), fluid = T,
      plotlyOutput("plot2"), fluid = T
    )))

server <- function(input, output, session){
  dat <- reactive({
    req(c(input$LifeStage, input$Production, input$ESU))
    df1 <- dfplot %>% 
      filter(LifeStage %in% input$LifeStage) %>% 
      filter(Production %in% input$Production) %>%  
      filter(ESU %in% input$ESU)
  })
  dat2 <- reactive({
    req(c(input$LifeStage, input$Production, input$ESU))
    df1 <- dfplot2 %>% 
      filter(LifeStage %in% input$LifeStage) %>%
      filter(Production %in% input$Production) %>%
      filter(ESU %in% input$ESU)
  })
  output$plot1 <-renderPlotly({
    ggplot(data = dat(), aes (y = Percentage, x = Year, fill = Take_Type))+ 
      geom_bar(stat = "identity", position = "stack")+
      scale_fill_viridis(discrete = T, name = "Take Type", option = "C") +
      labs(x = "Year", y = "Percent Take (%)")+
      theme(axis.text.x = element_text(angle = 30, hjust = 0.5, vjust = 0.5))
    ggplotly(tooltip = c("y", "x"))
  })
  output$plot2 <-renderPlotly({
    ggplot(data = dat2(), aes (y = Percentage, x = Year, fill = Take_Type))+ 
      geom_bar(stat = "identity", position = "stack")+
      scale_fill_viridis(discrete = T, name = "Take Type", option = "C") +
      labs(x = "Year", y = "Percent Take (%)")+
      theme(axis.text.x = element_text(angle = 30, hjust = 0.5, vjust = 0.5))
    ggplotly(tooltip = c("y", "x"))
  })
} 

shinyApp (ui = ui, server = server) 
