library(shiny)
library(ggplot2)
library(DT)
library(stringr)
library(dplyr)
library(tools)
library(lubridate)


# read the dataframe and necessary data wrangling
stocks_df = read.csv("stocks_merged.csv", header = T)
stocks_df$Date = as.Date(stocks_df$Date)
# ----

# Define UI for application that plots features of stocks -----------
ui <- fluidPage(

  # Sidebar layout with a input and output definitions --------------
  sidebarLayout(

    # Inputs: Select variables to plot ------------------------------
    sidebarPanel(

      # Select variable for y-axis ----------------------------------
      selectInput(inputId = "y",
                  label = "Y-axis:",
                  choices = c("Open", "Close", "Volume"),
                  selected = "Close"),

      # Select variable for stocks ----------------------------------
      # Only Date so far (need to revisit this)
      selectInput(inputId = "x",
                  label = "X-axis:",
                  choices = c("Date"),
                  selected = "Date"),
      
      # Select variable for stocks ----------------------------------
      # Only Date so far (need to revisit this)
      selectInput(inputId = "stocks",
                  label = "Stocks:",
                  choices = unique(stocks_df$Ticker),
                  selected = "AAPL"),
      
      # Set dates to filter ----------------------------------------------
      dateRangeInput(inputId = "date_range", 
                  label = "Date:", 
                  start = "2020-01-02", end = "2021-02-12",
                  min = "2020-01-02", max = "2021-02-12",
                  format = "yyyy-mm-dd", separator = "to"),
      
    #   # Select the stocks to plot ------------------------
    #   checkboxGroupInput(inputId = "selected_type",
    #                      label = "Select Stock(s):",
    #                      choices = c("AAL",
    #                                  "AAPL",
    #                                  "AMZN",
    #                                  "CSCO",
    #                                  "CVS",
    #                                  "EXPE",
    #                                  "FB",
    #                                  "GME",
    #                                  "GOOG",
    #                                  "LYFT",
    #                                  "MAR",
    #                                  "NFLX",
    #                                  "PFE",
    #                                  "TSLA",
    #                                  "UBER",
    #                                  "ZM"),
    #                      selected = "AAPL"),
    # ),
    
    # # Show data table ---------------------------------------------
    checkboxInput(inputId = "show_data",
                  label = "Show data table",
                  value = TRUE),

    
    # Horizontal line for visual separation -----------------------
    hr(),
    
    # # Group variables by sectors -----------------------------------
    # # used for data table
    checkboxGroupInput(inputId = "stocks_industry_type",
                       label = "Show stocks by:",
                       choices = unique(stocks_df$Industry),
                       selected = "Sector"),
    ),
    
    # Output: Show lineplot --------------------------------------
    mainPanel(
      plotOutput(outputId = "lineplot"),
      # Show data table ---------------------------------------------
      dataTableOutput(outputId = "stockstable")
    )
  )
)

# Define server function required to create the lineplot ---------
server <- function(input, output) {

  stocks_subset <- reactive({
    req(input$x) # ensure availablity of value before proceeding
    filter(stocks_df, Ticker %in% input$stocks &
             Date >= input$date_range[1] & Date <= input$date_range[2])
  })
  
  # Create lineplot object the plotOutput function is expecting --
  output$lineplot <- renderPlot({
    ggplot(data = stocks_subset(), aes_string(x = input$x, y = input$y)) + geom_line(color = "darkblue") +
      labs(x = "Date", y = input$y) +
      theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust=1)) + theme_bw()
                                      # color = input$z)) +
  })
  
  # # Print data table if checked -------------------------------------
  output$stockstable <- renderDataTable(
    if(input$show_data){
      datatable(data = filter(stocks_df, Industry %in% input$stocks_industry_type),
                    options = list(pageLength = 10),
                    rownames = FALSE)
    }
  )
}

# Run the application -----------------------------------------------
shinyApp(ui = ui, server = server)


