
# BDA400 Assignment 6 - Technical Analysis using R
# Visualization Phase

library(shiny)
library(ggplot2)
library(quantmod)
library(TTR)
library(dplyr)
library(lubridate)

# Fetch stock data from Yahoo Finance
fetch_stock_data <- function(symbol, start_date, end_date) {
  tryCatch({
    stock <- getSymbols(
      symbol,
      src = "yahoo",
      from = start_date,
      to = end_date,
      auto.assign = FALSE
    )
    
    data <- data.frame(
      Date = as.Date(index(stock)),
      coredata(stock)
    )
    
    colnames(data) <- c(
      "Date", "Open", "High", "Low",
      "Close", "Volume", "Adjusted"
    )
    
    data
  }, error = function(e) {
    NULL
  })
}

# Convert daily data into weekly or monthly data
aggregate_period <- function(data, timeframe) {
  
  if (timeframe == "Daily" || nrow(data) < 2) {
    return(data)
  }
  
  period <- if (timeframe == "Weekly") {
    format(data$Date, "%Y-%U")
  } else {
    format(data$Date, "%Y-%m")
  }
  
  data %>%
    mutate(Period = period) %>%
    group_by(Period) %>%
    summarise(
      Date = max(Date),
      Open = first(Open),
      High = max(High, na.rm = TRUE),
      Low = min(Low, na.rm = TRUE),
      Close = last(Close),
      Volume = sum(Volume, na.rm = TRUE),
      Adjusted = last(Adjusted),
      .groups = "drop"
    ) %>%
    select(Date, Open, High, Low, Close, Volume, Adjusted) %>%
    arrange(Date)
}

# Calculate technical indicators and trading signals
add_indicators <- function(data) {
  
  data$MA20 <- as.numeric(SMA(data$Close, n = 20))
  data$MA50 <- as.numeric(SMA(data$Close, n = 50))
  data$RSI14 <- as.numeric(RSI(data$Close, n = 14))
  
  macd_result <- MACD(data$Close, nFast = 12, nSlow = 26, nSig = 9)
  
  data$MACD <- as.numeric(macd_result[, 1])
  data$MACD_Signal <- as.numeric(macd_result[, 2])
  data$MACD_Hist <- data$MACD - data$MACD_Signal
  
  data$Signal <- "Hold"
  
  # Buy signal: MA20 crosses above MA50
  buy_index <- which(
    data$MA20 > data$MA50 &
      dplyr::lag(data$MA20) <= dplyr::lag(data$MA50)
  )
  
  # Sell signal: MA20 crosses below MA50
  sell_index <- which(
    data$MA20 < data$MA50 &
      dplyr::lag(data$MA20) >= dplyr::lag(data$MA50)
  )
  
  data$Signal[buy_index] <- "Buy"
  data$Signal[sell_index] <- "Sell"
  
  data
}

# User interface
ui <- fluidPage(
  
  titlePanel("Stock Technical Analysis Dashboard"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      textInput("symbol", "Stock Symbol", value = "AAPL"),
      
      dateRangeInput(
        "dates",
        "Select Date Range",
        start = Sys.Date() - 365,
        end = Sys.Date()
      ),
      
      selectInput(
        "timeframe",
        "Select Time Frame",
        choices = c("Daily", "Weekly", "Monthly")
      ),
      
      checkboxGroupInput(
        "indicators",
        "Technical Indicators",
        choices = c(
          "Moving Averages" = "MA",
          "RSI" = "RSI",
          "MACD" = "MACD"
        ),
        selected = c("MA", "RSI", "MACD")
      )
    ),
    
    mainPanel(
      h4(textOutput("summary")),
      plotOutput("price_plot", height = "450px"),
      plotOutput("rsi_plot", height = "250px"),
      plotOutput("macd_plot", height = "250px"),
      tableOutput("signals_table")
    )
  )
)

# Server
server <- function(input, output, session) {
  
  stock_data <- reactive({
    
    req(input$symbol, input$dates)
    
    raw_data <- fetch_stock_data(
      toupper(trimws(input$symbol)),
      as.Date(input$dates[1]),
      as.Date(input$dates[2])
    )
    
    validate(
      need(
        !is.null(raw_data) && nrow(raw_data) > 0,
        "No stock data found. Check symbol or date range."
      )
    )
    
    filtered_data <- aggregate_period(raw_data, input$timeframe)
    
    add_indicators(filtered_data)
  })
  
  # Summary
  output$summary <- renderText({
    
    data <- stock_data()
    
    paste(
      "Symbol:", toupper(input$symbol),
      "| Latest Closing Price:", round(tail(data$Close, 1), 2),
      "| Signal:", tail(data$Signal, 1)
    )
  })
  
  # Main stock price chart
  output$price_plot <- renderPlot({
    
    data <- stock_data()
    
    p <- ggplot(data, aes(x = Date)) +
      geom_line(aes(y = Close), color = "steelblue", linewidth = 0.7) +
      labs(
        title = paste(toupper(input$symbol), "Closing Price"),
        x = "Date",
        y = "Price"
      ) +
      theme_minimal()
    
    if ("MA" %in% input$indicators) {
      
      p <- p +
        geom_line(aes(y = MA20), color = "orange", na.rm = TRUE) +
        geom_line(aes(y = MA50), color = "purple", na.rm = TRUE)
    }
    
    signal_data <- data %>%
      filter(Signal != "Hold")
    
    if (nrow(signal_data) > 0) {
      
      p <- p +
        geom_point(
          data = signal_data,
          aes(y = Close, color = Signal),
          size = 3
        )
    }
    
    p
  })
  
  # RSI chart
  output$rsi_plot <- renderPlot({
    
    req("RSI" %in% input$indicators)
    
    data <- stock_data()
    
    ggplot(data, aes(x = Date, y = RSI14)) +
      geom_line(color = "darkgreen") +
      geom_hline(yintercept = c(30, 70), linetype = "dashed") +
      coord_cartesian(ylim = c(0, 100)) +
      labs(
        title = "Relative Strength Index (14)",
        x = "Date",
        y = "RSI"
      ) +
      theme_minimal()
  })
  
  # MACD chart
  output$macd_plot <- renderPlot({
    
    req("MACD" %in% input$indicators)
    
    data <- stock_data()
    
    ggplot(data, aes(x = Date)) +
      geom_line(aes(y = MACD), color = "blue", na.rm = TRUE) +
      geom_line(aes(y = MACD_Signal), color = "orange", na.rm = TRUE) +
      geom_col(aes(y = MACD_Hist), alpha = 0.4, na.rm = TRUE) +
      labs(
        title = "MACD Indicator (12, 26, 9)",
        x = "Date",
        y = "MACD"
      ) +
      theme_minimal()
  })
  
  # Recent Buy and Sell signals
  output$signals_table <- renderTable({
    
    stock_data() %>%
      filter(Signal != "Hold") %>%
      select(Date, Close, MA20, MA50, Signal) %>%
      arrange(desc(Date)) %>%
      head(10)
  })
}

# Launch dashboard
shinyApp(ui = ui, server = server)
