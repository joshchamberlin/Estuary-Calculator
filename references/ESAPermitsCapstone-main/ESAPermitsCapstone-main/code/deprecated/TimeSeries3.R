# Making Time Series Infographics - Line plots
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
  titlePanel("Authorized and Reported Take (Lethal/Non-Lethal) per Year"),
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
      h5("These charts display the authorized and reported take 
         (lethal/non-lethal) per year"),
      h6("*Data is only showing what was reported, not complete"), 
      plotlyOutput("plot1"), fluid = T,
      plotlyOutput("plot2"), fluid = T
    )))

server <- function(input, output, session){
  dat <- reactive({
    req(c(input$LifeStage, input$Production, input$ESU))
    df1 <- plot %>% 
      filter(LifeStage %in% input$LifeStage) %>% 
      filter(Production %in% input$Production) %>%  
      filter(ESU %in% input$ESU)
  })
  dat2 <- reactive({
    req(c(input$LifeStage, input$Production, input$ESU))
    df1 <- plot2 %>% 
      filter(LifeStage %in% input$LifeStage) %>%
      filter(Production %in% input$Production) %>%
      filter(ESU %in% input$ESU)
  })
  output$plot1 <-renderPlotly({
    ggplot(data = dat(), aes (y = N, x = Year, color = Take, group = Take))+ 
      geom_line()+
      geom_point()+
      scale_color_viridis(discrete = T, name = "Take Type", option = "C") +
      labs(x = "Year", y = "Number of Fish")+ 
      theme(axis.text.x = element_text(angle = 30, hjust = 0.5, vjust = 0.5),
               panel.background = element_rect(fill = "#A5A3A3" ))
    ggplotly(tooltip = c("y", "x", "fill"))
  })
  output$plot2 <-renderPlotly({
    ggplot(data = dat2(), aes (y = N, x = Year, color = Take, group = Take))+ 
      geom_line()+
      geom_point()+
      scale_color_viridis(discrete = T, name = "Take Type", option = "C") +
      labs(x = "Year", y = "Number of Fish")+ 
      theme(axis.text.x = element_text(angle = 30, hjust = 0.5, vjust = 0.5),
            panel.background = element_rect(fill = "#A5A3A3") )
    ggplotly(tooltip = c("y", "x", "fill"))
  })
} 

shinyApp (ui = ui, server = server) 
