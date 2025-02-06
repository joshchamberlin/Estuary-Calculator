# Rory Spurr
# R Shiny Practice

library(shiny)
library(tidyverse)


source("code/Reading and Filtering.R")
source("code/Functions.R")


# UI Code chunk
ui <- fluidPage(
  dateRangeInput(inputId = "date",
    label = "Choose a start and end year",
    start = "2018-02-09", end = "2022-01-05",
    min = min(wcr$DateIssued), max = max(wcr$DateIssued),
    separator = "-"),

  plotOutput(outputId = "Plot")
  # 
  # selectInput(inputId = "Species",
  #             label = "Choose a Species",
  #             choices = species),
  # 
  # plotOutput(outputId = "map")
)

# Server Code Chunk
server <- function(input, output){
  output$Plot <- renderPlot({
    wcr <- wcr %>% filter(DateIssued>input$date[1] & DateIssued<input$date[2])
    SpeciesDist <- species.num.apps(wcr) # create species/permits data frame
    ggplot(data = SpeciesDist, aes(x=species, y=nApps))+
      geom_col() +
      theme_bw() +
      scale_x_discrete(expand = c(0,0))+
      scale_y_continuous(expand = c(0,0)) +
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank()) +
      labs(x = "Species", y = "Number of Applications")
  })
  # output$map <- renderPlot({
  #   species.sum <- subset(species.dats, names(species.dats) == input$Species)
  #   species.sum <- species.sum %>% as.data.frame() %>% st_as_sf()
  #   names(species.sum)[1:2] <- c("huc8", "sum")
  #   ggplot() +
  #     geom_sf(data = wcr.bound, fill = "ivory") +
  #     geom_sf(data = species.sum, aes(fill = sum)) +
  #     scale_fill_continuous(type = "viridis", name = "Incidental Mortality") +
  #     theme_bw()
  # })
}

# Piece together the App
shinyApp(ui = ui, server = server)
