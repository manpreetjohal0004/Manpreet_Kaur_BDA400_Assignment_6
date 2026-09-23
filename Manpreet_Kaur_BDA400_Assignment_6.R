# BDA400 - Data Science Tools and Techniques
# Assignment 6 - Technical Analysis Portfolio Dashboard
# Student: Manpreet Kaur
#
# Purpose:
# Build an interactive Shiny dashboard that retrieves stock data from Yahoo Finance,
# visualizes the selected stock, calculates technical indicators (SMA, RSI, MACD),
# and generates Buy/Sell/Hold signals using a moving-average crossover strategy.

# -----------------------------
# 1. Packages
# -----------------------------
required_packages <- c("shiny", "ggplot2", "quantmod", "TTR", "dplyr", "xts")

new_packages <- required_packages[!(required_packages %in% installed.packages()[, "Package"])]
if (length(new_packages) > 0) {
  install.packages(new_packages, dependencies = TRUE)
}

library(shiny)
library(ggplot2)
library(quantmod)
library(TTR)
library(dplyr)
library(xts)

# -----------------------------
# 2. User Interface
# -----------------------------
ui <- fluidPage(
  titlePanel("Portfolio Dashboard - Technical Analysis"),

  sidebarLayout(
    sidebarPanel(
      textInput("symbol", "Stock Symbol:", value = "AAPL"),
      dateRangeInput(
        "date_range",
        "Select Date Range:",
        start = "2023-01-01",
        end = "2023-07-01"
      ),
      selectInput(
        "time_frame",
        "Select Time Frame:",
        choices = c("Daily", "Weekly", "Monthly"),
        selected = "Daily"
      ),
      selectInput(
        "chart_type",
        "Select Chart Type:",
        choices = c("Line", "Area", "Candlestick"),
        selected = "Line"
      ),
      checkboxGroupInput(
        "technical_indicators",
        "Technical Indicators:",
        choices = c("Moving Averages", "RSI", "MACD"),
        selected = c("Moving Averages", "RSI", "MACD")
      ),
      helpText("Trading rule: SMA(20) above SMA(50) = Buy; below = Sell.")
    ),

    mainPanel(
      h3("Stock Price and Trading Signals"),
      plotOutput("stock_chart", height = "500px"),

      conditionalPanel(
        condition = "input.technical_indicators.indexOf('RSI') >= 0",
        h3("Relative Strength Index (RSI)"),
        plotOutput("rsi_chart", height = "260px")
      ),

      conditionalPanel(
        condition = "input.technical_indicators.indexOf('MACD') >= 0",
        h3("MACD"),
        plotOutput("macd_chart", height = "300px")
      ),

      h3("Latest Trading Signal"),
      verbatimTextOutput("latest_signal")
    )
  )
)

# -----------------------------
# 3. Server Logic
# -----------------------------
server <- function(input, output, session) {

  # Fetch historical stock data from Yahoo Finance.
  stock_data <- reactive({
    req(input$symbol, input$date_range)

    validate(
      need(input$date_range[1] < input$date_range[2],
           "The start date must be earlier than the end date.")
    )

    symbol <- toupper(trimws(input$symbol))

    tryCatch(
      {
        getSymbols(
          Symbols = symbol,
          src = "yahoo",
          from = input$date_range[1],
          to = input$date_range[2] + 1,
          auto.assign = FALSE,
          warnings = FALSE
        )
      },
      error = function(e) {
        showNotification(
          paste("Unable to retrieve", symbol, "from Yahoo Finance."),
          type = "error"
        )
        NULL
      }
    )
  })

  # Convert daily data to the selected time frame.
  period_data <- reactive({
    x <- stock_data()
    req(x)

    result <- switch(
      input$time_frame,
      "Daily" = x,
      "Weekly" = to.weekly(x, indexAt = "endof", drop.time = TRUE),
      "Monthly" = to.monthly(x, indexAt = "endof", drop.time = TRUE)
    )

    result
  })

  # Prepare a data frame and calculate indicators and signals.
  analysis_data <- reactive({
    x <- period_data()
    req(x)

    validate(
      need(NROW(x) >= 50,
           "At least 50 observations are required. Select a longer date range.")
    )

    df <- data.frame(
      Date = as.Date(index(x)),
      Open = as.numeric(Op(x)),
      High = as.numeric(Hi(x)),
      Low = as.numeric(Lo(x)),
      Close = as.numeric(Cl(x)),
      Volume = as.numeric(Vo(x)),
      Adjusted = as.numeric(Ad(x))
    )

    # Technical indicators
    df$SMA20 <- SMA(df$Close, n = 20)
    df$SMA50 <- SMA(df$Close, n = 50)
    df$RSI14 <- RSI(df$Close, n = 14)

    macd_values <- MACD(df$Close, nFast = 12, nSlow = 26, nSig = 9,
                        maType = "EMA", percent = FALSE)
    df$MACD <- as.numeric(macd_values[, 1])
    df$MACD_Signal <- as.numeric(macd_values[, 2])
    df$MACD_Histogram <- df$MACD - df$MACD_Signal

    # Moving-average trading rule
    df$Signal <- ifelse(
      is.na(df$SMA20) | is.na(df$SMA50),
      "Hold",
      ifelse(df$SMA20 > df$SMA50, "Buy",
             ifelse(df$SMA20 < df$SMA50, "Sell", "Hold"))
    )

    # Annotate only when the signal changes, rather than labeling every point.
    df$SignalChange <- df$Signal != dplyr::lag(df$Signal, default = "Hold")
    df$Annotation <- ifelse(
      df$SignalChange & df$Signal %in% c("Buy", "Sell"),
      df$Signal,
      NA_character_
    )

    df
  })

  # -----------------------------
  # 4. Main Stock Visualization
  # -----------------------------
  output$stock_chart <- renderPlot({
    df <- analysis_data()
    req(df)

    p <- ggplot(df, aes(x = Date))

    if (input$chart_type == "Line") {
      p <- p + geom_line(aes(y = Close), linewidth = 0.7)
    } else if (input$chart_type == "Area") {
      p <- p +
        geom_area(aes(y = Close), alpha = 0.35) +
        geom_line(aes(y = Close), linewidth = 0.6)
    } else {
      # Candlestick chart using segments and thick vertical bodies.
      p <- p +
        geom_segment(
          aes(x = Date, xend = Date, y = Low, yend = High),
          linewidth = 0.4
        ) +
        geom_segment(
          aes(x = Date, xend = Date, y = Open, yend = Close),
          linewidth = 3.2
        )
    }

    # Overlay moving averages when selected.
    if ("Moving Averages" %in% input$technical_indicators) {
      p <- p +
        geom_line(aes(y = SMA20, linetype = "SMA 20"), linewidth = 0.7,
                  na.rm = TRUE) +
        geom_line(aes(y = SMA50, linetype = "SMA 50"), linewidth = 0.7,
                  na.rm = TRUE) +
        labs(linetype = "Moving Average")
    }

    # Add Buy/Sell annotations at crossover changes.
    signal_points <- df %>%
      filter(!is.na(Annotation))

    if (nrow(signal_points) > 0) {
      p <- p +
        geom_point(
          data = signal_points,
          aes(y = Close, shape = Signal),
          size = 3
        ) +
        geom_text(
          data = signal_points,
          aes(y = Close, label = Annotation),
          vjust = -1,
          size = 3.5
        ) +
        labs(shape = "Signal")
    }

    p +
      labs(
        title = paste(toupper(input$symbol), "-", input$time_frame, "Stock Data"),
        subtitle = "Yahoo Finance data with SMA crossover trading signals",
        x = "Date",
        y = "Price"
      ) +
      theme_minimal(base_size = 12) +
      theme(
        plot.title = element_text(face = "bold"),
        legend.position = "bottom"
      )
  })

  # -----------------------------
  # 5. RSI Indicator
  # -----------------------------
  output$rsi_chart <- renderPlot({
    req("RSI" %in% input$technical_indicators)
    df <- analysis_data()

    ggplot(df, aes(x = Date, y = RSI14)) +
      geom_line(linewidth = 0.7, na.rm = TRUE) +
      geom_hline(yintercept = 70, linetype = "dashed") +
      geom_hline(yintercept = 30, linetype = "dashed") +
      coord_cartesian(ylim = c(0, 100)) +
      labs(
        title = "RSI (14)",
        x = "Date",
        y = "RSI"
      ) +
      theme_minimal(base_size = 12)
  })

  # -----------------------------
  # 6. MACD Indicator
  # -----------------------------
  output$macd_chart <- renderPlot({
    req("MACD" %in% input$technical_indicators)
    df <- analysis_data()

    ggplot(df, aes(x = Date)) +
      geom_col(aes(y = MACD_Histogram), alpha = 0.35, na.rm = TRUE) +
      geom_line(aes(y = MACD, linetype = "MACD"), linewidth = 0.7,
                na.rm = TRUE) +
      geom_line(aes(y = MACD_Signal, linetype = "Signal"), linewidth = 0.7,
                na.rm = TRUE) +
      labs(
        title = "Moving Average Convergence Divergence (MACD)",
        x = "Date",
        y = "MACD",
        linetype = "Series"
      ) +
      theme_minimal(base_size = 12) +
      theme(legend.position = "bottom")
  })

  # -----------------------------
  # 7. Latest Signal
  # -----------------------------
  output$latest_signal <- renderText({
    df <- analysis_data()
    latest <- df[nrow(df), ]

    paste0(
      "Stock: ", toupper(input$symbol),
      "\nDate: ", latest$Date,
      "\nClose: ", round(latest$Close, 2),
      "\nSMA 20: ", round(latest$SMA20, 2),
      "\nSMA 50: ", round(latest$SMA50, 2),
      "\nSignal: ", latest$Signal
    )
  })
}

# -----------------------------
# 8. Run the Shiny Application
# -----------------------------
shinyApp(ui = ui, server = server)
