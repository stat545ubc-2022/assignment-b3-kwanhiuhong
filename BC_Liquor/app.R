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

bcl <- read.csv("bcl-data.csv", stringsAsFactors = FALSE)

# Define the UI for BC Liquid Store
ui <- fluidPage(
  
  # Feature 1 - Added a new theme for the shinny app - One change to UI, this helps enhance the UI because people might be bored with the default UI
  theme = shinytheme("darkly"),
  
  # Feature 3 - Added a UBC logo in the title panel - Another change to UI, this helps users identify where the developers are and can better protect the property right
  titlePanel(title = div(img(width = "20%", src="UBC_logo.png"), "BC Liquor Store prices by UBC students")),
  
  sidebarLayout(
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
      tableOutput("results")
    )
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
}

# Run the application 
shinyApp(ui = ui, server = server)
