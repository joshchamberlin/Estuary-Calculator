#----
#template app code
library(shiny)
ui <- fluidPage("Hello World") #sets ui object

server <- function(input, output) {} #sets up server object

shinyApp (ui = ui, server = server) #knits it into shiny app
#----
#input functions
ui <- fluidPage(sliderInput(inputId = "num", #assign name to input
                            label = "choose a number", #label for figure
                            value = 25, min = 1, max = 100)) #values for min and max

server <- function(input, output) {} #sets up server object

shinyApp (ui = ui, server = server) #knits it into shiny app

#Type of functions
#'actionButton()
#'submitButton()
#'checkboxInput()
#'checkboxGroupInput()
#'dateInput()
#'dateRangeInput()
#'fileInput()
#'numericInput()
#'passwordInput()
#'radioButtons()
#'selectInput()
#'sliderInput()
#'textInput()
#----
#output functions
library(shiny)
ui <- fluidPage(sliderInput(inputId = "num", #assign name to input
                            label = "choose a number", #label for figure
                            value = 25, min = 1, max = 100), #values for min and max
                plotOutput(outputId = "hist")) #gives name to output

server <- function(input, output) {} #sets up server object

shinyApp (ui = ui, server = server) #knits it into shiny app

#'Type of functions
#'dataTableOutput()
#'htmlOutput()
#'imageOutput()
#'plotOutput()
#'tableOutput()
#'textOutput()
#'uiOutput()
#'verbatimTextOutput()
#----
#talking to your server
ui <- fluidPage(sliderInput(inputId = "num", #assign name to input
                            label = "choose a number", #label for figure
                            value = 25, min = 1, max = 100), #values for min and max
                plotOutput(outputId = "hist")) #gives name to output
#' save objects to display output$
server <- function(input, output) {
  output$hist <-renderPlot({ 
  title <- "100 random normal values"
  hist (rnorm(input$num), main = title)
  })
} 
#' use the render() function that creates the type of output you wish to make

shinyApp (ui = ui, server = server) #knits it into shiny app
#Reactivity automatically occurs whenever you use an input value to render an output object

#'Type of functions
#'renderDataTable()
#'renderImage()
#'renderPlot()
#'renderPrint()
#'renderTable()
#'renderText()
#'renderUI()
#----
#sharing your apps
ui <- fluidPage(sliderInput(inputId = "num", 
                            label = "choose a number", 
                            value = 25, min = 1, max = 100), 
                plotOutput(outputId = "hist")) 
server <- function(input, output) {
  output$hist <-renderPlot({ 
    title <- "100 random normal values"
    hist (rnorm(input$num), main = title)
  })
} 
shinyApp (ui = ui, server = server)

#'One directory with every file app needs:
#'app.R (your script which ends with a call to shinyApp())
#'datasets, images, css, helper scripts, etc.
#----
#build your own server
ui <- fluidPage(sliderInput(inputId = "num", 
                            label = "choose a number", 
                            value = 25, min = 1, max = 100), 
                plotOutput(outputId = "hist")) 
server <- function(input, output) {
  output$hist <-renderPlot({ 
    title <- "100 random normal values"
    hist (rnorm(input$num), main = title)
  })
} 
shinyApp (ui = ui, server = "Shiny Server")#use shiny server 
#See P1S9 for more details 
#----
#'reactive values 
#'reactive values notify the function that use them when 
#'becoming invalid
#'the objects created by reactive functions respond
library(shiny)
ui <- fluidPage(sliderInput(inputId = "num", 
                            label = "choose a number", 
                            value = 25, min = 1, max = 100), 
                plotOutput(outputId = "hist")) 
server <- function(input, output) {
  output$hist <-renderPlot({ 
    title <- "100 random normal values"
    hist (rnorm(input$num), main = title)
  })
} 
shinyApp (ui = ui, server = server)
#' 1. reactive values act as the dta stream that flows through 
#' your app.
#' 2. the input list is a list of reactive values. the values show 
#' the current state of inputs.
#' 3. you can only call a reactive value from a reactive function
#' that is designed to work with one.
#' 4. reactive values notify, the objects created by reactive 
#' functions respond. 
#----
#'rendering 
#'

library(shiny)
ui <- fluidPage(sliderInput(inputId = "num", 
                            label = "choose a number", 
                            value = 25, min = 1, max = 100), 
                plotOutput(outputId = "hist")) 
server <- function(input, output) {
  output$hist <-renderPlot({ 
    title <- "100 random normal values"
    hist (rnorm(input$num), main = title)
  })
} 
shinyApp (ui = ui, server = server)
#'Types of Functions
#'renderDataTable()
#'renderImage()
#'renderPlot()
#'renderPrint()
#'renderTable()
#'renderText()
#'renderUI()
#'
#'
#'
#-----
#' Reactivity and trigger code based on changes in the input objects
#' Action Buttons
#' library(shiny)
ui <- fluidPage(actionButton(inputId = "clicls", label = "click me"), sliderInput(inputId = "num", 
                            label = "choose a number", 
                            value = 25, min = 1, max = 100), 
                plotOutput(outputId = "hist")) 
server <- function(input, output) {
  output$hist <-renderPlot({ 
    title <- "100 random normal values"
    hist (rnorm(input$num), main = title)
  })
} 
#' 
#' 
#' 
#-----
#' R Shiny Dashboard Functions
 install.packages("shinydashboard")
 library(shinydashboard)
 library (shiny)
## ui.R ##
dashboardPage(
  dashboardHeader(),
  dashboardSidebar(),
  dashboardBody()
)
#
#
## app.R ##
ui <- dashboardPage(
  dashboardHeader(),
  dashboardSidebar(),
  dashboardBody()
)

server <- function(input, output) { }

shinyApp(ui, server)
#
#
## app.R blank dashboard ##
ui <- dashboardPage(
  dashboardHeader(title = "Basic dashboard"),
  dashboardSidebar(),
  dashboardBody(
    # Boxes need to be put in a row (or column)
    fluidRow(
      box(plotOutput("plot1", height = 250)),
      
      box(
        title = "Controls",
        sliderInput("slider", "Number of observations:", 1, 100, 50)
      )
    )
  )
)

server <- function(input, output) {
  set.seed(122)
  histdata <- rnorm(500)
  
  output$plot1 <- renderPlot({
    data <- histdata[seq_len(input$slider)]
    hist(data)
  })
}

shinyApp(ui, server)
#
#
## creating sidebar ##
#'There are two parts that need to be done. 
#'First, you need to add menuItems to the sidebar, with appropriate tabNames.
#'In the body, add tabItems with corrsponding values for tabName

## Sidebar content
dashboardSidebar(
  sidebarMenu(
    menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
    menuItem("Widgets", tabName = "widgets", icon = icon("th"))
  )
)

## Body content
dashboardBody(
  tabItems(
    # First tab content
    tabItem(tabName = "dashboard",
            fluidRow(
              box(plotOutput("plot1", height = 250)),
              
              box(
                title = "Controls",
                sliderInput("slider", "Number of observations:", 1, 100, 50)
              )
            )
    ),
    
    # Second tab content
    tabItem(tabName = "widgets",
            h2("Widgets tab content")
    )
  )
)
#
#
