# This is the main app file which contains UI and Server elements
library(plotly)
library(shiny)
library(bslib)
library(bsicons)
library(dplyr)
library(zoo)
library(quantmod)
library(jsonlite)
library(tidyr)
library(stringr)

source("scripts.R") # this calls essential functions from scripts.R file

# Variables
stock_list = c("AAPL", "NVDA", "MSFT", "META", "AMZN", "GOOGL", "TSLA", "TSM", "COST")

company_tickers <- fromJSON("company_tickers.json") %>% tibble() %>% unnest(cols = ".") %>%
  dplyr::mutate(index = rep(1:3, length.out = length(.))) %>%
  dplyr::group_by(index) %>%
  dplyr::mutate(row = row_number()) %>%
  tidyr::pivot_wider(names_from = "index", values_from = ".") %>%
  dplyr::select("Ticker" = `2`, "Company" = `3`) %>%
  dplyr::mutate("Ticker" = as.character(Ticker), "Company" = as.character(Company)) %>%
  dplyr::mutate("Company" = stringr::str_to_title(`Company`)) %>%
  dplyr::mutate("Company + Ticker" = paste0(Company, " (", Ticker, ")"))

# Modules
stock_picker <- selectizeInput(
  # choices are defined in server element for better performance
  "stock_picker", "Choose ticker:", choices = NULL 
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

server <- function(input, output, session) {
  
  updateSelectizeInput(session, 'stock_picker', choices = company_tickers[["Company + Ticker"]], server = TRUE)
  
  stock_data <- reactive({
    req(input$stock_picker != "") # req() only renders plots once ticker selected
    # filter finds the corresponding ticker for the stock name and 'pipes' this into stock price function
    filter(company_tickers, `Company + Ticker` == !!input$stock_picker)[["Ticker"]] %>%
      getStockPrice()
    })
  
  output$main_plot <- renderPlotly({
    plot_ly(stock_data(), type="scatter", mode="lines", x=~Date, y=~Close)
  })
  
  current_data <- reactive({
    req(input$stock_picker != "")
    tail(stock_data(), 1)
    })
  
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