library(shiny)
library(tidyverse)

options(scipen = 999)

house_data <- read_csv("Melbourne_housing_FULL.csv", col_names = TRUE)

## Define UI
ui <- fluidPage(
  
  titlePanel("Melbourne Housing Data Exploration!"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      h2("Subset the Data"),
      
      selectInput(
        inputId = "cat_var",
        label = "Categorical Variable",
        choices = c("Suburb",
                    "Number of Rooms" = "Rooms",  # treating as categorical & numerical
                    "Type of House" = "Type",
                    "Method of Sale" = "Method",
                    "Postal Code" = "Postcode",
                    "Number of Bathrooms" = "Bathroom",
                    "Number of Carspots" = "Cars",
                    "Governing Council for the Area" = "CouncilArea",
                    "General Region" = "Regionname")
      ),
      
      selectInput(
        inputId = "num_var_1",
        label = "Numerical Variable 1",
        choices = c("Sale Price" = "Price",
                    "Number of Rooms" = "Rooms",
                    "Land Size in Meters" = "Landsize",
                    "Year the House was Built" = "YearBuilt")
      ),
      
      uiOutput("slider1"),
      
      selectInput(
        inputId = "num_var_2",
        label = "Numerical Variable 2",
        choices = c("Sale Price" = "Price",
                    "Number of Rooms" = "Rooms",
                    "Land Size in Meters" = "Landsize",
                    "Year the House was Built" = "YearBuilt")
      ),
      
      uiOutput("slider2"),
      
      actionButton("sample_data", "Get a Data Sample")
      
    ),
    mainPanel(
      tableOutput("test_data")

    )
  )
)


## Define server logic
server <- function(input, output, session) {
  
  ## prevent user from choosing the same numeric variables
  observeEvent(c(input$num_var_1, input$num_var_2), {
    
    num_var_1 <- input$num_var_1
    num_var_2 <- input$num_var_2
    choices <- c("Sale Price" = "Price",
                 "Number of Rooms" = "Rooms",
                 "Land Size in Meters" = "Landsize",
                 "Year the House was Built" = "YearBuilt")
    
    if (num_var_1 == num_var_2){
      choices <- choices[-which(choices == num_var_1)]
      updateSelectizeInput(session,
                           "num_var_2",
                           choices = choices)
    }
  })
  
  ## create slider for first numeric variable
  output$slider1 <- renderUI({
    
    num_range_1 <- range(house_data[[input$num_var_1]], na.rm = TRUE)
    
    sliderInput(inputId = "num_range_1",
                label = paste("Values of", input$num_var_1),
                min = num_range_1[1],
                max = num_range_1[2],
                value = num_range_1)
  })
  
  ## create slider for second numeric variable
  output$slider2 <- renderUI({
    
    num_range_2 <- range(house_data[[input$num_var_2]], na.rm = TRUE)
    
    sliderInput(inputId = "num_range_2",
                label = paste("Values of", input$num_var_2),
                min = num_range_2[1],
                max = num_range_2[2],
                value = num_range_2)
  })
  
  ## create sample from full data set based on user selections
  house_sample <- reactiveValues()
  
  observeEvent(input$sample_data, {
    
    house_sample$data <- house_data |>
      select(input$cat_var, input$num_var_1, input$num_var_2) |>
      filter(between(!!sym(input$num_var_1), input$num_range_1[1], input$num_range_1[2]),
             between(!!sym(input$num_var_2), input$num_range_2[1], input$num_range_2[2]))
  })
  
  output$test_data <- renderTable({
    house_sample$data
  })
  
}

## Run the application 
shinyApp(ui = ui, server = server)