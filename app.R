library(shiny)
library(tidyverse)

options(scipen = 999)

house_data <- read_csv("Melbourne_housing_FULL.csv", col_names = TRUE)

# Define UI
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
      
      uiOutput("slider1")
      
    ),
    mainPanel(

    )
  )
)


# Define server logic
server <- function(input, output, session) {
  
  output$slider1 <- renderUI({
    
    var_range <- range(house_data[[input$num_var_1]], na.rm = TRUE)
    
    sliderInput(inputId = "var_range",
                label = paste("Values of", input$num_var_1),
                min = var_range[1],
                max = var_range[2],
                value = var_range)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)