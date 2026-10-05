# ==============================================================================
# BDA400 - Assignment 6: Technical Analysis & Visualization Dashboard
# Description: Interactive R Shiny dashboard for stock data analysis, 
#              technical indicators (SMA, RSI, MACD), and trading signal annotations.
# ==============================================================================

# Step 1: Install and Load Required Packages
required_packages <- c("shiny", "ggplot2", "quantmod", "TTR", "dplyr")
new_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages)

library(shiny)
library(ggplot2)
library(quantmod)
library(TTR)
library(dplyr)

# ------------------------------------------------------------------------------
# UI Setup
# ------------------------------------------------------------------------------
ui <- fluidPage(
  titlePanel("Technical Analysis & Portfolio Dashboard"),
  
  sidebarLayout(
    sidebarPanel(
      textInput("stock_symbol", "Stock Symbol:", value = "AAPL"),
      dateRangeInput("date_range", "Select Date Range:", 
                     start = "2023-01-01", end = "2023-07-01"),
      
      selectInput("chart_type", "Chart Type:", 
                  choices = c("Line Graph", "Area Chart")),
      
      checkboxGroupInput("technical_indicators", "Overlay Technical Indicators:",
                         choices = c("Moving Averages", "RSI", "MACD"),
                         selected = c("Moving Averages")),
      
      hr(),
      h4("Strategy Parameters"),
      numericInput("short_ma_period", "Short MA Period:", value = 20, min = 5, max = 50),
      numericInput("long_ma_period", "Long MA Period:", value = 50, min = 20, max = 200),
      
      actionButton("update", "Update Analysis", class = "btn-primary")
    ),
    
    mainPanel(
      plotOutput("stock_chart", height = "500px"),
      hr(),
      h4("Latest Trading Signal"),
      verbatimTextOutput("signal_summary")
    )
  )
)

# ------------------------------------------------------------------------------
# Server Logic
# ------------------------------------------------------------------------------
server <- function(input, output, session) {
  
  # Fetch data securely using eventReactive on button click / initial load
  stock_data <- eventReactive(input$update, {
    req(input$stock_symbol, input$date_range)
    
    tryCatch({
      data <- getSymbols(input$stock_symbol, src = "yahoo", 
                         from = input$date_range[1], 
                         to = input$date_range[2], 
                         auto.assign = FALSE)
      
      df <- data.frame(
        Date = index(data),
        Open = as.numeric(Op(data)),
        High = as.numeric(Hi(data)),
        Low = as.numeric(Lo(data)),
        Close = as.numeric(Cl(data)),
        Volume = as.numeric(Vo(data))
      )
      return(df)
    }, error = function(e) {
      showNotification(paste("Error fetching stock data:", e$message), type = "error")
      return(NULL)
    })
  }, ignoreNULL = FALSE)

  # Process Indicators & Signal Logic
  processed_data <- reactive({
    df <- stock_data()
    req(df)
    
    # Moving Averages
    df$Short_MA <- SMA(df$Close, n = input$short_ma_period)
    df$Long_MA <- SMA(df$Close, n = input$long_ma_period)
    
    # RSI & MACD
    df$RSI <- RSI(df$Close, n = 14)
    macd_res <- MACD(df$Close)
    df$MACD <- macd_res$macd
    df$Signal_Line <- macd_res$signal
    
    # Generate Crossover Signals
    df <- df %>%
      mutate(
        Signal = case_when(
          Short_MA > Long_MA & lag(Short_MA) <= lag(Long_MA) ~ "Buy",
          Short_MA < Long_MA & lag(Short_MA) >= lag(Long_MA) ~ "Sell",
          TRUE ~ "Hold"
        ),
        Signal_Label = ifelse(Signal %in% c("Buy", "Sell"), Signal, NA)
      )
    
    return(df)
  })

  # Render Plot
  output$stock_chart <- renderPlot({
    df <- processed_data()
    req(df)
    
    # Base Plot Creation
    p <- ggplot(df, aes(x = Date, y = Close))
    
    if (input$chart_type == "Area Chart") {
      p <- p + geom_area(fill = "skyblue", alpha = 0.4) + geom_line(color = "blue")
    } else {
      p <- p + geom_line(color = "darkblue", size = 0.8)
    }
    
    # Dynamic Overlay: Moving Averages
    if ("Moving Averages" %in% input$technical_indicators) {
      p <- p + 
        geom_line(aes(y = Short_MA), color = "orange", size = 0.8, linetype = "dashed") +
        geom_line(aes(y = Long_MA), color = "red", size = 0.8, linetype = "solid")
    }
    
    # Annotations for Signals
    p <- p + geom_point(data = filter(df, !is.na(Signal_Label)), 
                        aes(color = Signal_Label), size = 3) +
      geom_text(data = filter(df, !is.na(Signal_Label)), 
                aes(label = Signal_Label, vjust = ifelse(Signal_Label == "Buy", -1.2, 1.8)), 
                fontface = "bold") +
      scale_color_manual(values = c("Buy" = "green", "Sell" = "red"))
    
    # Styling
    p <- p + labs(title = paste(input$stock_symbol, "Price Chart & Signals"),
                  x = "Date", y = "Price (USD)", color = "Signal") +
      theme_minimal()
    
    print(p)
  })

  # Display Signal Summary
  output$signal_summary <- renderText({
    df <- processed_data()
    req(df)
    latest <- tail(df, 1)
    paste0("Date: ", latest$Date, "\n",
           "Closing Price: $", round(latest$Close, 2), "\n",
           "Current Signal Status: ", latest$Signal)
  })
}

shinyApp(ui = ui, server = server)