# This is the main app file which contains UI and Server elements
library(plotly)
library(shiny)
library(bslib)
library(bsicons)
library(dplyr)
library(zoo)
library(quantmod)

source("scripts.R") # this calls essential functions from scripts.R file

# Variables
stock_list = c("AAPL", "NVDA", "MSFT", "META", "AMZN", "GOOGL", "TSLA", "TSM", "COST")

# Modules
stock_picker <- selectInput(
  "stock_picker", "Choose ticker:",
  stock_list, selected = "AAPL"
)

ui <- page_sidebar(
  title = "Stock Screener",
  sidebar = stock_picker,
  layout_columns(
    fill = FALSE,
    value_box(
      title = "Previous Close",
      value = textOutput("price"),
      showcase = bsicons::bs_icon("cash")
    ),
    value_box(
      title = "Volume",
      value = textOutput("volume"),
      showcase = bsicons::bs_icon("bar-chart")
    ),
    value_box(
      title = "Market Cap",
      value = textOutput("market_cap"),
      showcase = bsicons::bs_icon("bank")
    )
  ),
  layout_columns(
    plotlyOutput("main_plot")
  )
)

server <- function(input, output) {
  
  stock_data <- reactive({getStockPrice(input$stock_picker)})
  
  output$main_plot <- renderPlotly({
    plot_ly(stock_data(), type="scatter", mode="lines", x=~Date, y=~Close)
  })
  
  current_data <- reactive({tail(stock_data(), 1)})
  
  # Note: current_data must be called with parenthesis as it is a function
  output$price <- renderText({
    format(current_data()[["Close"]], big.mark = ",")
    })
  output$volume <- renderText({
    format(current_data()[["Volume"]], big.mark = ",")
    })
  output$market_cap <- renderText({
    format(current_data()[["Close"]]*current_data()[["Volume"]], big.mark = ",")
    })
}

shinyApp(ui, server)