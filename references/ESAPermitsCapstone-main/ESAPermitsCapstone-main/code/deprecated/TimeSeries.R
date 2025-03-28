# Making Time Series Infographics - Calculated Numbers
# Alana Santana and Rory Spurr
#=============================================================
#Reading in packages
library(shiny)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(NMFSResPermits)
library(plotly)
library(viridis)
library(DT)
library(shinyjqui)
library(RColorBrewer)
#==============================================================
#Sourcing Script
#setwd("~/GitHub/ESA_Permits_Capstone")
source(paste(getwd(), "/code/dependencies/Reading and Filtering.R", sep = ""))
source(paste(getwd(), "/code/dependencies/TSPreAppCode.R", sep = ""))
#==============================================================
#Shiny Integration
ui <-  fluidPage(
  titlePanel("Authorized and Reported Take (Lethal and Non-Lethal) per Year"),
  sidebarLayout(
    
    sidebarPanel(
      radioButtons(inputId = "LifeStage", label = "Choose a lifestage",
                   choices = c("Adult", "Juvenile")),
      radioButtons(inputId = "Production", label = "Choose an Origin",
                   choices = c("Natural", "Listed Hatchery")),
      selectInput(inputId = "ESU", label = "Choose an ESU to View",
                  choices = levels(df$ESU), 
                  multiple = F), width = 4), 
    mainPanel(
      h5("These charts display the authorized take (lethal and non-lethal) in number of fish per year. Total authorized is broken down into reported take (yellow) 
         and unused authorized take (blue)."),
      h6("*Data is only showing what was reported, not complete"),
      plotlyOutput("plot1"), fluid = T,
      plotlyOutput("plot2"), fluid = T,
    width = 8, height = 7),
    position = c("left", "right"),
    fluid = T
  ),
    
    DT::dataTableOutput("table", width = "100%", height = "auto")
)


server <- function(input, output, session){
  dat <- reactive({
    req(c(input$LifeStage, input$Production, input$ESU))
    df1 <- df_plot %>% 
    filter(LifeStage %in% input$LifeStage) %>% 
    filter(Production %in% input$Production) %>%  
    filter(ESU %in% input$ESU)
  })
    dat2 <- reactive({
      req(c(input$LifeStage, input$Production, input$ESU))
      df1 <- df_plot2 %>% 
        filter(LifeStage %in% input$LifeStage) %>%
        filter(Production %in% input$Production) %>%
        filter(ESU %in% input$ESU)
  })
    dat3 <- reactive({
      dt %>%
        filter(ESU == input$ESU) %>%
        filter(LifeStage == input$LifeStage) %>% 
        filter(Production == input$Production) %>%
        filter(ResultCode != "Tribal 4d") %>%
        select(Year:Authorized_Mortality_Unused)
    })
output$plot1 <-renderPlotly({
  ggplot(data = dat(), aes (y = N, x = Year, fill = Take_Type))+ 
    geom_bar(stat = "identity", position = "stack", color = "black")+
    scale_fill_manual(values = mycols, name = "Take Type") +
     labs(x = "Year", y = "Total Take (Number of fish)", title = "Non-Lethal Take Plot")+ 
    theme(axis.text.x = element_text(angle = 30, hjust = 0.5, vjust = 0.5), 
          panel.background = element_rect(fill = "#D0D3D4" ))
  ggplotly(tooltip = c("y", "x", "fill"))
})
output$plot2 <-renderPlotly({
  ggplot(data = dat2(), aes (y = N, x = Year, fill = Take_Type))+ 
    geom_bar(stat = "identity", position = "stack", color = "black")+
    scale_fill_manual(values = mycols, name = "Take Type") +
    labs(x = "Year", y = "Total Take (Number of fish)", title = "Lethal Take Plot")+ 
    theme(axis.text.x = element_text(angle = 30, hjust = 0.5, vjust = 0.5), 
          panel.background = element_rect(fill = "#D0D3D4" ))
  ggplotly(tooltip = c("y", "x", "fill"))
          
})
output$table <- DT::renderDataTable(dat3(),
                caption = "Note : table excludes 'Tribal 4d' permits for privacy concerns, 
                but are included in the take totals", 
                colnames = c("Year", "ESU", "Production", "Life Stage",
                             "Permit Code", "Gear Type", "Report ID", "Permit Type", 
                             "Reported Take", "Authorized Take", "Reported Mortality",
                             "Authorized Mortality", "Unused Take", "Unused Mortality"),
                options = list(pageLength = 10, autoWidth = T)
)
jqui_sortable("#table thead tr")}

shinyApp (ui = ui, server = server) 
