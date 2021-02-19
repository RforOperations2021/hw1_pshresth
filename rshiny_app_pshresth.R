library(shiny)
library(ggplot2)
library(DT)
library(stringr)
library(dplyr)
library(tools)
library(lubridate)
library(bslib)

# ------
# Data Input and Wrangling


stocks_df = read.csv("stocks_merged.csv", header = T)
stocks_df$Date = as.Date(stocks_df$Date) # convert the data variable to date type

# a new variable that calculates weekly percentage change in stock value
stocks_df = stocks_df %>% group_by(Ticker) %>%
  mutate(WeeklyChangePct = (Close - lag(Close, n = 6)) * 100 / lag(Close, n = 6))

stocks_df$WeeklyChangePct = round(stocks_df$WeeklyChangePct, digits = 4) # round the percentage to 4 decimal places

# a new variable that creates portfolio value for equal comparison
# e.g. you have $1000 and you bought however much stocks you could of the companies
  # and see how that $1000 value changes

stocks_start = stocks_df %>% group_by(Ticker) %>% 
  summarize(start_stock = round(1000 / Close[1], digits = 4)) # purchase stocks worth $1000

num_companies = stocks_df %>% group_by(Industry) %>% 
  summarize(num_companies = n() / 282) # normalize stocks industry by number of companies
                                      # some industries have more companies then others

stocks_df = left_join(stocks_df, stocks_start, by = "Ticker")
stocks_df = left_join(stocks_df, num_companies, by = "Industry")

stocks_df$portfolioValue = stocks_df$Close * stocks_df$start_stock # portfolio value is the price you get if you sold your stocks on that day
stocks_df$industry_adj_value = stocks_df$portfolioValue / stocks_df$num_companies
stocks_df = subset(stocks_df, select = -c(start_stock))

# stock performance (used for bar plot later)
stock_val = stocks_df %>% group_by(Ticker) %>% 
  summarize(stock_change_pct = (Close[282] - Close[1]) * 100 / Close[1])

stock_val$stock_change_pct = round(stock_val$stock_change_pct, digits = 3)

# ---------
# Dark Theme

light <- bs_theme()
dark <- bs_theme(bg = "#1e1d20", fg = "white", primary = "#02baf9")

# ---------
# Define UI for application that plots features of stocks

# dark theme source: Dynamic Themeing in RShiny
 # https://rstudio.github.io/bslib/articles/theming.html#dynamic-theming-in-shiny

ui <- fluidPage(
  theme = light, 
  div(
    class = "custom-control custom-switch", 
    tags$input(
      id = "dark_mode", type = "checkbox", class = "custom-control-input",
      onclick = HTML("Shiny.setInputValue('dark_mode', document.getElementById('dark_mode').value);")
    ),
    tags$label(
      "Dark mode", `for` = "dark_mode", class = "custom-control-label"
    )
  ),
  # Sidebar layout with a input and output definitions --------------
  sidebarLayout(

    # Inputs: Select variables to plot ------------------------------
    sidebarPanel(

      # Select variable for y-axis ----------------------------------
      selectInput(inputId = "y",
                  label = "Y-axis:",
                  choices = c("Close", "portfolioValue", "Volume", "WeeklyChangePct"),
                  selected = "portfolioValue"),
      
      # Select variable for stocks ----------------------------------
      selectInput(inputId = "stocks",
                  label = "Choose first stock:",
                  choices = unique(stocks_df$Ticker),
                  selected = "AMZN"),
      
      # Select variable for stocks ----------------------------------
      selectInput(inputId = "stocks2",
                  label = "Choose second stock:",
                  choices = unique(stocks_df$Ticker),
                  selected = "AMZN"),
      
      # Set dates to filter ----------------------------------------------
      dateRangeInput(inputId = "date_range", 
                  label = "Date:", 
                  start = "2020-01-02", end = "2021-02-12",
                  min = "2020-01-02", max = "2021-02-12",
                  format = "yyyy-mm-dd", separator = "to"),
    
    # Horizontal line for visual separation -----------------------
    hr(),
    
    # # Show data table ---------------------------------------------
    checkboxInput(inputId = "show_data",
                  label = "Show data table",
                  value = TRUE),

    hr(),
    
    # Write sampled data as csv ------------------------------------------
    downloadButton(outputId = "write_csv", 
                 label = "Download data")
    
    ),
    
    # end of side bar panel
    
    # Output: Show line plot --------------------------------------
    mainPanel(
      # create three different tabs that show individual stocks, 
      tabsetPanel(type = "tabs",
                  tabPanel("Individual Stocks", plotOutput("lineplot")),
                  tabPanel("Stocks by Sector", plotOutput("groupplot")),
                  tabPanel("Stock Performance", plotOutput("barplot"))),
      
      # a little bit of visual separation
      br(),          
      br(),
      
      # Show data table ---------------------------------------------
      dataTableOutput(outputId = "stockstable")
      
    )
  )
)

# Define server function required to create the lineplot ---------
server <- function(input, output, session) {
  # change the theme to dark mode if user changes the toggle button
  observe({
    session$setCurrentTheme(
      if (isTRUE(input$dark_mode)) {
        dark
        } else {
          light
        }
    )
  })
  
  stocks_subset <- reactive({
    req(input$stocks) # ensure availablity of value before proceeding
    filter(stocks_df, Ticker %in% c(input$stocks, input$stocks2) &
             Date >= input$date_range[1] & 
             Date <= input$date_range[2])
  })
  
  
  # create line plot --
  output$lineplot <- renderPlot({
    ggplot(data = stocks_subset(), aes_string(x = "Date", y = input$y, color = "Ticker")) +
      geom_line(size = 0.8) +
      labs(x = "Date", y = input$y) +
      theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust=1)) +
      theme_bw()
                                      
  })
  
  # create line plot grouped by industry
  # the idea here is to see which industry did better or worse since the start of 2020
  output$groupplot <- renderPlot({
    ggplot(data = filter(stocks_df, Ticker != "GME"), # remove GME (GameStop) stock because of its exponential growth
                                                      # which dwarfs all other industries
           aes_string(x = "Date", y = "industry_adj_value", color = "Industry")) +
      geom_line(size = 0.8) +
      labs(x = "Date", y = "Portfolio Value") +
      theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust=1)) +
      theme_bw()
  })
  
  # bar graph to show the stock's growth/decline since 2020
  # the dataframe used here is `stock_val` which I create earlier
  output$barplot <- renderPlot({
  ggplot(data = stock_val, mapping = aes(x = reorder(Ticker, -stock_change_pct), y = stock_change_pct)) + 
    geom_bar(stat = "identity", fill = "#457b9d") +
    labs(x = "Ticker", y = "Percentage Change") +
    theme(plot.title = element_text(hjust = 0.5))
  })
  
  # print data table if checked -------------------------------------
  output$stockstable <- renderDataTable(
    if(input$show_data){
      datatable(data = filter(stocks_df, Ticker %in% c(input$stocks, input$stocks2)), # stocks have to be in the drop down user selected
                    options = list(pageLength = 10),
                    rownames = FALSE)
    }
  )
  
  # write sampled data as csv file---------------------------------------
  output$write_csv <- downloadHandler(
    filename <- function(){paste0("stocks_", str_replace_all(Sys.time(), ":|\ ", "_"), ".csv")}, # asks user where to save the csv file
    content = function(filname){
      write.csv(stocks_subset(), file = filname, row.names = FALSE)
    }
  )
}

# end of server function

# -----
# run the application
shinyApp(ui = ui, server = server)