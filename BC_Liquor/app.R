#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(shinythemes)
library(DT)
bcl <- read.csv("bcl-data.csv", stringsAsFactors = FALSE)

# Define the UI for BC Liquid Store
ui <- fluidPage(
  
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "https://fonts.cdnfonts.com/css/cute")
  ),
  
  # Feature 4 - Added a special font using an external css, which helps make our webpage a bit nicer
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
  ),
  
  # Feature 1 - Added a new theme for the shinny app - One change to UI, this helps enhance the UI because people might be bored with the default UI
  theme = shinytheme("darkly"),
  
  # Feature 3 - Added a UBC logo in the title panel - Another change to UI, this helps users identify where the developers are and can better protect the property right
  titlePanel(title = div(img(width = "20%", src="UBC_logo.png"), "BC Liquor Store prices by UBC students")),
  tabsetPanel(
    tabPanel("Overview", sidebarLayout(
      sidebarPanel(
        sliderInput("priceInput", "Price", min = 0, max = 100,
                    value = c(25, 40), pre = "$"),
        radioButtons("typeInput", "Product type",
                     choices = c("BEER", "REFRESHMENT", "SPIRITS", "WINE"),
                     selected = "WINE"),
        uiOutput("countryOutput"),
        
        # Feature 2 - Allow user to sort the data by Alcohol_Content - One new functional widget that modifies at least one output. This lets user find out which alcohol is more suitable for them.
        checkboxInput("sortByAlcoholContent", "Sort data by alcoholic content", TRUE)
      ),
      mainPanel(
        plotOutput("coolplot"),
        br(), br(),
        # Feature 6 - Allow user to export/download the dataset in CSV format so that they can process it on Excel.
        downloadButton("exportCSV"),
        tableOutput("results")
      )
    )),
    
    # Feature 5 - We made a static plot to allow users to view how many alcohol we have according to different alcohol types, which gives them a general overview about our current stock.
    tabPanel("Overview By Types of Alcohol", plotOutput("plotbytype")),
  )
)

# Define server logic required for the BC liquid store app
server <- function(input, output) {
  output$countryOutput <- renderUI({
    selectInput("countryInput", "Country",
                sort(unique(bcl$Country)),
                selected = "CANADA")
  })
  
  filtered <- reactive({
    if (is.null(input$countryInput)) {
      return(NULL)
    }   
    
    # this is data before sort
    raw <-
      bcl %>%
      filter(Price >= input$priceInput[1],
             Price <= input$priceInput[2],
             Type == input$typeInput,
             Country == input$countryInput
      )
    
    if (input$sortByAlcoholContent) {
      # Feature 2: sort data by alcohol content - One new functional widget that modifies at least one output.
      raw %>% arrange(Alcohol_Content)
    } else {
      raw
    }
  })
  
  output$coolplot <- renderPlot({
    if (is.null(filtered())) {
      return()
    }
    ggplot(filtered(), aes(Alcohol_Content)) +
      geom_histogram()
  })
  
  output$results <- renderTable({
    filtered()
  })
  
  output$plotbytype <- renderPlot({
    ggplot(bcl, aes(x = factor(1), fill = Type)) +
      geom_bar() +
      labs(y= "Number", x ="") + 
      coord_polar("y")
  })
  
  # Feature 6 - Allow user to export/download the dataset in CSV format so that they can process it on Excel.
  output$exportCSV <- downloadHandler(
    filename = function() {
      "bcl-results.csv"
    },
    content = function(file) {
      write.csv(filtered(), file)
    }
  )
}

# Run the application 
shinyApp(ui = ui, server = server)
