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
        inputId = "house_type",
        label = "Choose the House Type:",
        choices = c("All",
                    "House, Cottage, Villa" = "h",
                    "Unit, Duplex" = "u",
                    "Townhouse" = "t")
      ),
      
      selectInput(
        inputId = "house_region",
        label = "Choose the House Region:",
        choices = c("All",
                    unique(house_data$Regionname[house_data$Regionname != "#N/A"]))
      ),
      
      selectInput(
        inputId = "num_var_1",
        label = "Choose a Numerical Variable:",
        choices = c("Sale Price" = "Price",
                    "Number of Rooms" = "Rooms",
                    "Land Size in Meters" = "Landsize",
                    "Year the House was Built" = "YearBuilt")
      ),
      
      uiOutput("slider1"),
      
      selectInput(
        inputId = "num_var_2",
        label = "Choose another Numerical Variable:",
        choices = c("Sale Price" = "Price",
                    "Number of Rooms" = "Rooms",
                    "Land Size in Meters" = "Landsize",
                    "Year the House was Built" = "YearBuilt")
      ),
      
      uiOutput("slider2"),
      
      actionButton("sample_data", "Get a Data Sample")
      
    ),
    mainPanel(
      
      tabsetPanel(
        
        tabPanel(title = "About",
                 p("The purpose of this app is to dynamically analyze and explore housing market data from Melbourne, Australia."),
                 p("The data provides information about houses sold in Melbourne, Australia from 2016-2018.
                   The variables include the price the house was sold at along with information about the house and where it is located.
                   More information about this data can be found at:", 
                   a(href = "https://www.kaggle.com/datasets/anthonypino/melbourne-housing-market", "This Link")),
                 p("The sidebar allows the user to create a subset of the data based on specific variables and data values."),
                 p("The About Tab (this tab) provides information about the app and the data."),
                 p("The Data Download Tab allows the user to download a csv file of the data, subsetted if sidebar selections were made."),
                 p("The Data Exploration Tab displays numeric and graphical summaries of the data based on sidebar selections made."),
                 img(src = "https://content.r9cdn.net/rimg/dimg/e7/e2/a092e93b-city-13998-1641eaba8a3.jpg?width=1366&height=768&xhint=1016&yhint=1024&crop=true",
                     height = "600px", width = "1200px")),
        
        tabPanel(title = "Data Download",
                 downloadButton("downloadData", "Download Data"),
                 DT::DTOutput("house_table")),
        
        tabPanel(title = "Data Exploration",
                 radioButtons(inputId = "sum_choice",
                              label = "Summaries to Display",
                              choices = c("Categorical", "Numerical")),
                 )
        
      )

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
                label = paste("Values of:", input$num_var_1),
                min = num_range_1[1],
                max = num_range_1[2],
                value = num_range_1)
  })
  
  ## create slider for second numeric variable
  output$slider2 <- renderUI({
    
    num_range_2 <- range(house_data[[input$num_var_2]], na.rm = TRUE)
    
    sliderInput(inputId = "num_range_2",
                label = paste("Values of:", input$num_var_2),
                min = num_range_2[1],
                max = num_range_2[2],
                value = num_range_2)
  })
  
  ## create sample from full data set based on user selections
  house_sample <- reactiveValues(data = house_data)
  
  observeEvent(input$sample_data, {
    
    house_sample$data <- house_data |>
      filter(between(!!sym(input$num_var_1), input$num_range_1[1], input$num_range_1[2]),
             between(!!sym(input$num_var_2), input$num_range_2[1], input$num_range_2[2]),
             Type == input$house_type | input$house_type == "All",
             Regionname == input$house_region | input$house_region == "All")
  })
  
  output$house_table <- DT::renderDT({
    house_sample$data
  })
  
  ## download the data set
  output$downloadData <- downloadHandler(
    filename = function() {
      paste('melbourne_sample_data', Sys.Date(), '.csv', sep = '')
    },
    content = function(con) {
      write.csv(house_sample$data, con)
    }
  )
  
  ## create summaries based on user selection

  
}

## Run the application 
shinyApp(ui = ui, server = server)