# This is the main app file which contains UI and Server elements

# Dependencies ----
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
library(shinycssloaders)
library(lubridate)

source("scripts.R") # this calls essential functions from scripts.R file

# Variables ----
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

# Modules ----
stock_picker <- selectizeInput(
  # choices are defined in server element for better performance
  "stock_picker", label = NULL, choices = NULL 
)

stock_search <- textInput( 
  "stock_search",
  label = NULL,
  placeholder = "Enter text..."
)

search_button <- actionButton("search_button", "Search")

date_range_input <- dateRangeInput(inputId = "date",
                                   label = h2("Date Range:"),
                                   start = Sys.Date() - years(2),
                                   end = Sys.Date(),
                                   min = "2007-01-01",
                                   max = Sys.Date(),
                                   format = "dd-M-yyyy")

# UI ----
ui <- page_sidebar(
  title = "Stock Screener",
  sidebar = sidebar(width = 290,
    bg = "white",
    date_range_input,
    accordion(id = "main_accordion",
              multiple = FALSE,
      accordion_panel(value = "standard",
        "Choose ticker:",
        stock_picker,
        open = TRUE
      ),
      accordion_panel(value = "advanced",
        "Search ticker",
        stock_search,
        search_button,
        open = FALSE
      )
    )
  ),
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
    plotlyOutput("main_plot") %>%
      shinycssloaders::withSpinner()
  )
)

# Server ----
server <- function(input, output, session) {
  
  # Main chart ----
  
  updateSelectizeInput(session, 'stock_picker', choices = company_tickers[["Company + Ticker"]], server = TRUE)

  # Collects stock data when valid ticker is selected, uses functions from scripts.R
  stock_data <- reactive({
    # Determines whether standard or advanced accordion is open and uses stock_picker or stock_search inputs accordingly
    if (input$main_accordion == "standard") {
      req(input$stock_picker != "") # req() only renders plots once ticker selected
      # filter finds the corresponding ticker for the stock name and 'pipes' this into stock price function
      filter(company_tickers, `Company + Ticker` == !!input$stock_picker)[["Ticker"]] %>%
        getStockPrice(dateRange = input$date) 
    } else if (input$main_accordion == "advanced") {
      req(input$stock_search != "")
      getStockPrice(toupper(input$stock_search), dateRange = input$date)
    }
  })
  
  # Generates plot as soon as stock is selected
  stock_plot <- reactive({
    # only renders plot once an accordion is selected
    plot_ly(stock_data(), type="scatter", mode="lines", x=~Date, y=~Close)
  }) 
  
  # Generates plot only once search_button pressed (used in advanced accordion)
  stock_plot_w_button <- eventReactive(input$search_button, {
    plot_ly(stock_data(), type="scatter", mode="lines", x=~Date, y=~Close)
  })
  
  which_plot <- reactive({
    if (input$main_accordion == "standard") {
      stock_plot()
    } else if (input$main_accordion == "advanced") {
      stock_plot_w_button()
    }
  })
  
  # Links with 'plotlyOutput' in UI file to render plot
  output$main_plot <- renderPlotly({
    req(input$main_accordion == "standard" | input$main_accordion == "advanced")
    which_plot()
  })
  
  # Value boxes ----
  
  # Gets most recent close data 
  current_data <- reactive({
    tail(stock_data(), 1)
    })
  
  # Collates and formats recent close data
  value_boxes <- reactive({
    c(format(current_data()[["Close"]], big.mark = ","),
      format(current_data()[["Volume"]], big.mark = ","),
      format(current_data()[["Close"]]*current_data()[["Volume"]], big.mark = ",")
    )
  })
  
  # Generates value boxes only when button pressed
  value_boxes_w_button <- eventReactive(input$search_button, {
    value_boxes()
  })
  
  # Determines whether to use value boxes with/without button depending on which accordion is in use
  which_box <- reactive({
    req(input$main_accordion == "standard" | input$main_accordion == "advanced")
    if (input$main_accordion == "standard") {
      value_boxes()
    } else if (input$main_accordion == "advanced") {
      value_boxes_w_button()
    }
  })

  # Link with value_box in UI to calculate and show latest close, volume and market cap
  output$price <- renderText({
    which_box()[1]
    }) 
  output$volume <- renderText({
    which_box()[2]
  })
  output$market_cap <- renderText({
    which_box()[3]
  })
}

shinyApp(ui, server)