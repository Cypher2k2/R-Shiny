# Load required libraries
library(shiny)
library(shinydashboard)
library(ggplot2)  # For better plotting


# Define UI for the dashboard
ui <- dashboardPage(
  dashboardHeader(title = "Data Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Data Upload", tabName = "data_upload", icon = icon("upload")),
      menuItem("Analysis", tabName = "analysis", icon = icon("chart-line")),
      menuItem("Preprocessing", tabName = "preprocessing", icon = icon("edit")),
      menuItem("Forecasting", tabName = "forecasting", icon = icon("forward"))
    )
  ),
  dashboardBody(
    tabItems(
      # Data Upload Tab
      tabItem(tabName = "data_upload",
              fluidRow(
                fileInput('file1', 'Choose CSV File',
                          accept = c('text/csv', 
                                     'text/comma-separated-values,text/plain', 
                                     '.csv'))
              )
      ),
      # Analysis Tab with Sub-Tabs
      # Analysis Tab with Sub-Tabs
      # Analysis Tab with Sub-Tabs
      tabItem(tabName = "analysis",
              fluidRow(
                tabsetPanel(type = "tabs",
                            tabPanel("Original Data", 
                                     div(style = "overflow-x: scroll;", tableOutput("originalData"))),
                            tabPanel("Summary", verbatimTextOutput("dataSummary")),
                            tabPanel("Visualization", "Visualization tools will be here."),
                            tabPanel("Advanced Analysis", 
                                     radioButtons("analysisDim", "Choose Analysis Dimension:",
                                                  choices = c("Unidimensional" = "uni", 
                                                              "Bidimensional" = "bi")),
                                     uiOutput("columnSelector"),
                                     verbatimTextOutput("advancedAnalysisResults"),
                                     plotOutput("unidimPlot"), # Add this line for the plot
                                     plotOutput("bidimPlot")  # Add this line for the bidimensional plot
                                     
                            )
                            
                )
              )
      ),
      # Preprocessing Tab
      tabItem(tabName = "preprocessing",
              fluidRow(
                h4("Preprocessing Options"),
                actionButton("preprocessBtn", "Run Preprocessing")
              )
      ),
      # Forecasting Tab
      tabItem(tabName = "forecasting",
              fluidRow(
                h4("Forecasting Options"),
                actionButton("forecastBtn", "Run Forecasting")
              )
      )
    )
  )
)

# Define the server logic
server <- function(input, output) {
  # Reactive value to store and share the data across tabs
  values <- reactiveValues(data = NULL)
  
  # Observing the file input for data upload
  observe({
    inFile <- input$file1
    
    if (is.null(inFile)) {
      return(NULL)
    }
    values$data <- read.csv(inFile$datapath)
  })
  
  # Output for Original Data tab
  output$originalData <- renderTable({
    req(values$data)  # Ensure that the data is available
    values$data
  })
  
  # Output for Summary tab
  output$dataSummary <- renderPrint({
    req(values$data)  # Ensure that the data is available
    summary(values$data)
  })
  
  
  output$columnSelector <- renderUI({
    req(values$data)  # Ensure that the data is available
    if (input$analysisDim == 'uni') {
      selectInput("selectedColumn", "Choose a column:", 
                  choices = names(values$data))
    } else if (input$analysisDim == 'bi') {
      selectInput("selectedColumns", "Choose two columns:", 
                  choices = names(values$data), multiple = TRUE, selectize = TRUE)
    }
  })
  
  # Output for Advanced Analysis results
  output$advancedAnalysisResults <- renderPrint({
    req(values$data)  # Ensure that the data is available
    
    # Unidimensional Analysis
    if (input$analysisDim == 'uni' && !is.null(input$selectedColumn)) {
      colData <- values$data[[input$selectedColumn]]
      cat("Unidimensional Analysis for", input$selectedColumn, "\n")
      cat("Number of Categories (if qualitative):", length(unique(colData[!is.na(colData)])), "\n")
      cat("Number of Missing Values:", sum(is.na(colData)), "\n")
      # Add here any additional statistics or checks for outliers if the data is numeric
      if(is.numeric(colData)) {
        # Simple outlier detection
        Q1 <- quantile(colData, 0.25, na.rm = TRUE)
        Q3 <- quantile(colData, 0.75, na.rm = TRUE)
        IQR <- Q3 - Q1
        outlier_bounds <- c(Q1 - 1.5 * IQR, Q3 + 1.5 * IQR)
        num_outliers <- sum(colData < outlier_bounds[1] | colData > outlier_bounds[2], na.rm = TRUE)
        cat("Number of Potential Outliers:", num_outliers, "\n")
      }
    }
    
    # Bidimensional Analysis
    else if (input$analysisDim == 'bi' && !is.null(input$selectedColumns) && length(input$selectedColumns) == 2) {
      colData1 <- values$data[[input$selectedColumns[1]]]
      colData2 <- values$data[[input$selectedColumns[2]]]
      cat("Bidimensional Analysis for", paste(input$selectedColumns, collapse=", "), "\n")
      
      # You might want to provide specific analyses based on the types of the two variables (e.g., correlation if both are numeric)
      # As a simple case, if both are numeric:
      if(is.numeric(colData1) && is.numeric(colData2)) {
        cat("Correlation:", cor(colData1, colData2, use = "complete.obs"), "\n")
        # Add other bidimensional statistics or visualizations as needed
      }
      # If one or both are qualitative, consider contingency tables or other appropriate analyses
    }
  })
  
  output$unidimPlot <- renderPlot({
    req(values$data)  # Ensure that the data is available
    
    if (input$analysisDim == 'uni' && !is.null(input$selectedColumn)) {
      colData <- values$data[[input$selectedColumn]]
      
      if(is.numeric(colData)) {
        # Plot histogram for quantitative variable
        ggplot(data = values$data, aes_string(x = input$selectedColumn)) +
          geom_histogram(binwidth = diff(range(colData, na.rm = TRUE))/30, 
                         fill = 'blue', color = 'black') +
          theme_minimal() +
          xlab(input$selectedColumn) +
          ylab('Frequency') +
          ggtitle(paste('Histogram of', input$selectedColumn))
      } else {
        # Plot bar chart for qualitative variable
        ggplot(data = values$data, aes_string(x = input$selectedColumn)) +
          geom_bar(fill = 'tomato', color = 'black') +
          theme_minimal() +
          xlab(input$selectedColumn) +
          ylab('Count') +
          ggtitle(paste('Bar Chart of', input$selectedColumn))
      }
    }
  })
  output$bidimPlot <- renderPlot({
    req(values$data)  # Ensure that the data is available
    
    if (input$analysisDim == 'bi' && !is.null(input$selectedColumns) && length(input$selectedColumns) == 2) {
      colData1 <- values$data[[input$selectedColumns[1]]]
      colData2 <- values$data[[input$selectedColumns[2]]]
      
      # Both variables are quantitative
      if(is.numeric(colData1) && is.numeric(colData2)) {
        ggplot(values$data, aes_string(x = input$selectedColumns[1], y = input$selectedColumns[2])) +
          geom_point(alpha = 0.5) +
          theme_minimal() +
          xlab(input$selectedColumns[1]) +
          ylab(input$selectedColumns[2]) +
          ggtitle(paste('Scatter Plot of', input$selectedColumns[1], 'vs', input$selectedColumns[2]))
      }
      # Both variables are qualitative
      else if(!is.numeric(colData1) && !is.numeric(colData2)) {
        # Consider a mosaic plot or stacked/clustered bar chart if both are qualitative
        # Placeholder for mosaic or other appropriate chart for two qualitative variables
        print("Visualizations for two qualitative variables are not yet implemented.")
      }
      # One variable is quantitative and the other is qualitative
      else {
        # Boxplot or violin plot could be useful here
        qualitativeVar <- ifelse(is.numeric(colData1), input$selectedColumns[2], input$selectedColumns[1])
        quantitativeVar <- ifelse(is.numeric(colData1), input$selectedColumns[1], input$selectedColumns[2])
        ggplot(values$data, aes_string(x = qualitativeVar, y = quantitativeVar)) +
          geom_boxplot() +
          theme_minimal() +
          xlab(qualitativeVar) +
          ylab(quantitativeVar) +
          ggtitle(paste('Boxplot of', quantitativeVar, 'by', qualitativeVar))
      }
    }
  })
  
  
  
  # Output for Visualization tab
  # Implement visualization code here based on your preferences and data type
  # This can be scatter plots, histograms, box plots, etc.
  # Example: output$visualization <- renderPlot({ plot(...) })
}

# Run the application 
shinyApp(ui = ui, server = server)
