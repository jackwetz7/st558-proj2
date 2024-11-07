library(shiny)
library(shinyalert)
library(shinycssloaders)
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
                    "Number of Carspots" = "Car",
                    "Land Size in Meters" = "Landsize",
                    "Year the House was Built" = "YearBuilt")
      ),
      
      uiOutput("slider1"),
      
      selectInput(
        inputId = "num_var_2",
        label = "Choose another Numerical Variable:",
        choices = c("Sale Price" = "Price",
                    "Number of Rooms" = "Rooms",
                    "Number of Carspots" = "Car",
                    "Land Size in Meters" = "Landsize",
                    "Year the House was Built" = "YearBuilt")
      ),
      
      uiOutput("slider2"),
      
      actionButton("sample_data", "Get a Data Sample")
      
    ),
    mainPanel(
      
      tabsetPanel(
        
        ## about tab
        tabPanel(title = "About",
                 p("The purpose of this app is to dynamically analyze and explore housing market data from Melbourne, Australia."),
                 p("The data provides information about houses sold in Melbourne, Australia from 2016-2018.
                   The variables include the price the house was sold at along with information about the house and where it is located.
                   More information about this data can be found at:", 
                   a(href = "https://www.kaggle.com/datasets/anthonypino/melbourne-housing-market", "This Link")),
                 p("The sidebar allows the user to create a subset of the data based on specific variables and data values."),
                 p("The About Tab (this tab) provides information about the app and the data."),
                 p("The Data Download Tab allows the user to download a csv file of the data, subsetted if sidebar selections were made."),
                 p("The Data Exploration Tab displays tables and graphical summaries of the data based on the subsetted dataset.
                   The user can choose to create categorical or numerical summaries and which variables to use for the summaries."),
                 img(src = "https://content.r9cdn.net/rimg/dimg/e7/e2/a092e93b-city-13998-1641eaba8a3.jpg?width=1366&height=768&xhint=1016&yhint=1024&crop=true",
                     height = "600px", width = "1200px")),
        
        ## data download tab
        tabPanel(title = "Data Download",
                 downloadButton("downloadData", "Download Data"),
                 DT::DTOutput("house_table")),
        
        ## data exploration tab
        tabPanel(title = "Data Exploration",
                 radioButtons(inputId = "sum_choice",
                              label = "Summaries to Display:",
                              choices = c("Categorical", "Numerical")),
                 actionButton("sum_data", "Start Summarizing"),
                 
                 uiOutput("cat_select_1"),
                 uiOutput("cat_select_2"),
                 uiOutput("cat_select_num"),
                 
                 conditionalPanel(
                   condition = "input.sum_choice == 'Categorical'",
                   uiOutput("table_title_1"),
                   tableOutput("one_way_1"),
                   uiOutput("table_title_2"),
                   tableOutput("one_way_2"),
                   uiOutput("table_title_3"),
                   tableOutput("two_way"),
                   withSpinner(plotOutput("bar_plot")),
                   withSpinner(plotOutput("bar_plot2")),
                   withSpinner(plotOutput("heatmap_plot"))
                 ),
                 
                 uiOutput("num_select_1"),
                 uiOutput("num_select_2"),
                 uiOutput("num_select_cat"),
                 
                 conditionalPanel(
                   condition = "input.sum_choice == 'Numerical'",
                   uiOutput("sum_title"),
                   tableOutput("num_sum"),
                   withSpinner(plotOutput("hist_plot")),
                   withSpinner(plotOutput("scatter_plot")),
                   withSpinner(plotOutput("violin_plot"))
                 ))
      )
    )
  )
)


## Define server logic
server <- function(input, output, session) {
  
  ## prevent user from choosing the same numeric variables to subset on
  observeEvent(c(input$num_var_1, input$num_var_2), {
    
    num_var_1 <- input$num_var_1
    num_var_2 <- input$num_var_2
    choices <- c("Sale Price" = "Price",
                 "Number of Rooms" = "Rooms",
                 "Number of Carspots" = "Car",
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
    
    if (nrow(house_sample$data) == 0) {
      shinyalert(title = "Invalid Data", "The subsetted dataset has no rows.", type = "error")
    }
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
  
  ## create categorical summary ui based on user selection
  output$cat_select_1 <- renderUI({

    if (input$sum_choice == "Categorical") {
      
      selectInput(inputId = "cat_choice_1",
                  label = "Choose a Categorical Variable:",
                  choices = c("Type of House" = "Type",
                              "Method of Sale" = "Method",
                              "Postal Code" = "Postcode",
                              "Governing Council" = "CouncilArea",
                              "General Region" = "Regionname"))
    }
  })
  
  output$cat_select_2 <- renderUI({
    
    if (input$sum_choice == "Categorical") {
      
      selectInput(inputId = "cat_choice_2",
                  label = "Choose another Categorical Variable:",
                  choices = c("Type of House" = "Type",
                              "Method of Sale" = "Method",
                              "Postal Code" = "Postcode",
                              "Governing Council" = "CouncilArea",
                              "General Region" = "Regionname"))
    }
  })
  
  output$cat_select_num <- renderUI({
    
    if (input$sum_choice == "Categorical") {
      
      selectInput(inputId = "cat_choice_num",
                  label = "Choose a Color Variable:",
                  choices = c("Sale Price" = "Price",
                              "Number of Rooms" = "Rooms",
                              "Number of Carspots" = "Car",
                              "Land Size in Meters" = "Landsize",
                              "Year the House was Built" = "YearBuilt"))
    }
  })
  
  ## prevent user from choosing the same categorical variables to summarize on
  observeEvent(c(input$cat_choice_1, input$cat_choice_2), {
    
    cat_choice_1 <- input$cat_choice_1
    cat_choice_2 <- input$cat_choice_2
    choices <- c("Type of House" = "Type",
                 "Method of Sale" = "Method",
                 "Postal Code" = "Postcode",
                 "Governing Council" = "CouncilArea",
                 "General Region" = "Regionname")
    
    if (cat_choice_1 == cat_choice_2){
      choices <- choices[-which(choices == cat_choice_1)]
      updateSelectizeInput(session,
                           "cat_choice_2",
                           choices = choices)
    }
  })
  
  ## create numerical summary ui based on user selection
  output$num_select_1 <- renderUI({
    
    if (input$sum_choice == "Numerical") {
      
      selectInput(inputId = "num_choice_1",
                  label = "Choose a Numerical Variable:",
                  choices = c("Sale Price" = "Price",
                              "Number of Rooms" = "Rooms",
                              "Number of Carspots" = "Car",
                              "Land Size in Meters" = "Landsize",
                              "Year the House was Built" = "YearBuilt"))
    }
  })
  
  output$num_select_2 <- renderUI({
    
    if (input$sum_choice == "Numerical") {
      
      selectInput(inputId = "num_choice_2",
                  label = "Choose another Numerical Variable:",
                  choices = c("Sale Price" = "Price",
                              "Number of Rooms" = "Rooms",
                              "Number of Carspots" = "Car",
                              "Land Size in Meters" = "Landsize",
                              "Year the House was Built" = "YearBuilt"))
    }
  })
  
  output$num_select_cat <- renderUI({
    
    if (input$sum_choice == "Numerical") {
      
      selectInput(inputId = "num_choice_cat",
                  label = "Choose a Categorical Variable:",
                  choices = c("Type of House" = "Type",
                              "Method of Sale" = "Method",
                              "Postal Code" = "Postcode",
                              "Governing Council" = "CouncilArea",
                              "General Region" = "Regionname"))
    }
  })
  
  ## prevent user from choosing the same numerical variables to summarize on
  observeEvent(c(input$num_choice_1, input$num_choice_2), {
    
    num_choice_1 <- input$num_choice_1
    num_choice_2 <- input$num_choice_2
    choices <- c("Sale Price" = "Price",
                 "Number of Rooms" = "Rooms",
                 "Number of Carspots" = "Car",
                 "Land Size in Meters" = "Landsize",
                 "Year the House was Built" = "YearBuilt")
    
    if (num_choice_1 == num_choice_2){
      choices <- choices[-which(choices == num_choice_1)]
      updateSelectizeInput(session,
                           "num_choice_2",
                           choices = choices)
    }
  })
  
  ## tracking if summarize data button / radio button is clicked
  sum_tracker <- reactiveVal(FALSE)
  
  observeEvent(input$sum_choice, {
    sum_tracker(FALSE)
  })
  
  observeEvent(input$sum_data, {
    sum_tracker(TRUE)
  })
  
  ## summarizing data based on categorical variable selections
  one_way_table_1 <- eventReactive(input$sum_data, {
    table(house_sample$data[[input$cat_choice_1]], useNA = "always")
  })
  
  output$table_title_1 <- renderUI({
    req(sum_tracker(), input$sum_choice == "Categorical")
    p(paste("One-Way Contingency Table of", input$cat_choice_1))
  })
  
  output$one_way_1 <- renderTable({
    req(sum_tracker(), input$sum_choice == "Categorical")
    one_way_table_1()
  })
  
  one_way_table_2 <- eventReactive(input$sum_data, {
    table(house_sample$data[[input$cat_choice_2]], useNA = "always")
  })
  
  output$table_title_2 <- renderUI({
    req(sum_tracker(), input$sum_choice == "Categorical")
    p(paste("One-Way Contingency Table of", input$cat_choice_2))
  })
  
  output$one_way_2 <- renderTable({
    req(sum_tracker(), input$sum_choice == "Categorical")
    one_way_table_2()
  })
  
  two_way_table <- eventReactive(input$sum_data, {
    table(house_sample$data[[input$cat_choice_1]], house_sample$data[[input$cat_choice_2]], useNA = "always")
  })
  
  output$table_title_3 <- renderUI({
    req(sum_tracker(), input$sum_choice == "Categorical")
    p(paste("Two-Way Contingency Table of", input$cat_choice_1, "and", input$cat_choice_2))
  })
  
  output$two_way <- renderTable({
    req(sum_tracker(), input$sum_choice == "Categorical")
    two_way_table()
  })
  
  bar_plot_data <- eventReactive(input$sum_data, {
    ggplot(house_sample$data, aes(x = !!sym(input$cat_choice_1))) +
      geom_bar(fill = "red", color = "black") +
      labs(x = input$cat_choice_1, y = "Count", title = paste("Counts of", input$cat_choice_1))
  })
  
  output$bar_plot <- renderPlot({
    req(sum_tracker(), input$sum_choice == "Categorical")
    bar_plot_data()
  })
  
  bar_plot_data2 <- eventReactive(input$sum_data, {
    ggplot(house_sample$data, aes(x = !!sym(input$cat_choice_1))) +
      geom_bar(position = "dodge", fill = "blue", color = "black") +
      labs(x = input$cat_choice_1, y = "Count", 
           title = paste("Counts of", input$cat_choice_1, "by", input$cat_choice_2)) +
      facet_wrap(as.formula(paste("~", input$cat_choice_2)))
  })
  
  output$bar_plot2 <- renderPlot({
    req(sum_tracker(), input$sum_choice == "Categorical")
    bar_plot_data2()
  })
  
  heatmap_data <- eventReactive(input$sum_data, {
    ggplot(house_sample$data, aes(x = !!sym(input$cat_choice_1), y = !!sym(input$cat_choice_2), fill = !!sym(input$cat_choice_num))) +
      geom_tile() +
      labs(x = input$cat_choice_1, y = input$cat_choice_1, fill = input$cat_choice_num, 
           title = paste("Heatmap of", input$cat_choice_1, "and", input$cat_choice_2, "by", input$cat_choice_num))
  })
  
  output$heatmap_plot <- renderPlot({
    req(sum_tracker(), input$sum_choice == "Categorical")
    heatmap_data()
  })
  
  ## summarizing data based on numerical variable selections
  num_sum_data <- eventReactive(input$sum_data, {
    house_sample$data |>
      group_by(!!sym(input$num_choice_cat)) |>
      summarize(
        across(
          c(!!sym(input$num_choice_1), !!sym(input$num_choice_2)),
          list(mean = ~ mean(.x, na.rm = TRUE), sd = ~ sd(.x, na.rm = TRUE)),
          .names = "{.fn}_{.col}"
        )
      )
  })
  
  output$sum_title <- renderUI({
    req(sum_tracker(), input$sum_choice == "Numerical")
    p(paste("Numerical Summary Table of", input$num_choice_1, "and", input$num_choice_2, "by", input$num_choice_cat))
  })
  
  output$num_sum <- renderTable({
    req(sum_tracker(), input$sum_choice == "Numerical")
    num_sum_data()
  })
  
  hist_plot_data <- eventReactive(input$sum_data, {
    ggplot(house_sample$data |> drop_na(input$num_choice_1), aes(x = !!sym(input$num_choice_1))) +
      geom_histogram(fill = "green", color = "black") +
      labs(x = input$num_choice_1, y = "Count", 
           title = paste("Distribution of", input$num_choice_1))
  })
  
  output$hist_plot <- renderPlot({
    req(sum_tracker(), input$sum_choice == "Numerical")
    hist_plot_data()
  })
  
  scatter_plot_data <- eventReactive(input$sum_data, {
    ggplot(house_sample$data |> drop_na(input$num_choice_1, input$num_choice_2, input$num_choice_cat), 
           aes(x = !!sym(input$num_choice_1), y = !!sym(input$num_choice_2),
                                  color = !!sym(input$num_choice_cat))) +
      geom_point() +
      labs(x = input$num_choice_1, y = input$num_choice_2,
           title = paste(input$num_choice_1, "and", input$num_choice_2, "by", input$num_choice_cat))
  })
  
  output$scatter_plot <- renderPlot({
    req(sum_tracker(), input$sum_choice == "Numerical")
    scatter_plot_data()
  })
  
  violin_plot_data<- eventReactive(input$sum_data, {
    ggplot(house_sample$data |> drop_na(input$num_choice_1, input$num_choice_cat), 
           aes(x = !!sym(input$num_choice_cat), y = !!sym(input$num_choice_1))) +
      geom_violin(fill = "orange", color = "black") +
      labs(x = input$num_choice_cat, y = input$num_choice_1,
           title = paste("Distribution of", input$num_choice_1, "by", input$num_choice_cat))
  })
  
  output$violin_plot <- renderPlot({
    req(sum_tracker(), input$sum_choice == "Numerical")
    violin_plot_data()
  })

}

## Run the application 
shinyApp(ui = ui, server = server)