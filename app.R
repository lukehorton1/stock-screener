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
library(cryptoQuotes)

source("stocks.R") 
source("crypto.R")
source("plotly_config.R")

# Modules ----
stock_picker <- selectizeInput(
  # choices are defined in server element for better performance
  "stock_picker", label = NULL, choices = NULL
)

stock_search <- textInput( 
  "stock_search",
  label = NULL,
  placeholder = "Enter ticker (e.g. VTI)"
)

stock_picker_multiple <- selectizeInput(
  # choices are defined in server element for better performance
  "stock_picker_multiple", label = NULL, choices = NULL, multiple = TRUE
)

stock_search_multiple <- textInput( 
  "stock_search_multiple",
  label = NULL,
  placeholder = "Enter ticker(s) (e.g. VTI, VOO)"
)

search_button_multiple <- actionButton("search_button_multiple", "Search")

search_button <- actionButton("search_button", "Search")

date_range_input <- dateRangeInput(inputId = "date",
                                   label = "Date Range:",
                                   start = Sys.Date() - years(2),
                                   end = Sys.Date(),
                                   min = "2007-01-01",
                                   max = Sys.Date(),
                                   format = "dd-M-yyyy",
                                   weekstart = 1)

coin_picker <- selectizeInput(
  "coin_picker", label = NULL, choices = NULL, selected = NULL
)

# UI ----
ui <- page_navbar(
  title = "Stock Screener",
  theme = bs_theme(),
  id = "nav",
  sidebar = sidebar(width = 290,
                    bg = "white",
                    date_range_input,
                    conditionalPanel("input.nav == 'Overview'",
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
                    conditionalPanel("input.nav == 'Comparison'",
                                     accordion(id = "main_accordion_multiple",
                                               multiple = FALSE,
                                               accordion_panel(value = "standard_multiple",
                                                               "Choose ticker:",
                                                               stock_picker_multiple,
                                                               open = TRUE
                                               ),
                                               accordion_panel(value = "advanced_multiple",
                                                               "Search ticker",
                                                               stock_search_multiple,
                                                               search_button_multiple,
                                                               open = FALSE
                                               )
                                     )
                    ),
                    conditionalPanel("input.nav == 'Crypto'",
                                     "Choose cryptocurrency:",
                                     coin_picker
                                     )
  ),
  nav_panel(title = "Overview",
      layout_columns(
        fill = FALSE,
        value_box(
          title = "Previous Close",
          value = textOutput("price"),
          showcase = bsicons::bs_icon("cash")
        ),
        value_box(
          title = "Day Change",
          value = textOutput("day_change"),
          showcase = bsicons::bs_icon("arrow-down-up")
        ),
        value_box(
          title = "Total Change",
          value = textOutput("total_change"),
          showcase = bsicons::bs_icon("graph-up")
        )
      ),
      layout_column_wrap(width = NULL,
                         style = css(grid_template_columns = "3fr 1fr"),
        card(height = 700,
          card_header("Stock History",
            class = "d-flex justify-content-between",
            checkboxInput("candlestick", "Candlestick", FALSE)
          ),
          # class = "align-items-center",
          plotlyOutput("main_plot") %>%
            shinycssloaders::withSpinner()
        ),
        card(fill = FALSE,
          card_header(
            class = "d-flex justify-content-between",
          ),
          card_body(
            class = "align-items-center class, p-0",
            tableOutput("stock_summary_table") %>%
              shinycssloaders::withSpinner()
          )
        )
      )
  
  ),
  nav_panel(title = "Comparison",
            card(height = 700,
                 min_height = 700,
                 card_header("Stock Comparison",
                             class = "d-flex justify-content-between",
                 ),
                 plotlyOutput("stock_comparison_plot") %>%
                   shinycssloaders::withSpinner()
                 # card_body(
                 #   class = "align-items-center",
                 #   uiOutput("stock_comparison_plot") %>%
                 #     shinycssloaders::withSpinner()
                 # )
            )
            # card(fill = FALSE,
            #      card_header(
            #        class = "d-flex justify-content-between",
            #      ),
            #      card_body(
            #        class = "align-items-center class, p-0",
            #        tableOutput("stock_summary_table") %>%
            #          shinycssloaders::withSpinner()
            #      )
            # )
  
  ),
  nav_panel(title = "Crypto",
            layout_columns(
              fill = FALSE,
              value_box(
                title = "Price (USD)",
                value = textOutput("coin_price_box"),
                showcase = bsicons::bs_icon("coin")
              ),
              value_box(
                title = "Day Change",
                value = textOutput("coin_day_change"),
                showcase = bsicons::bs_icon("arrow-down-up")
              ),
              value_box(
                title = "Total Change",
                value = textOutput("coin_total_change"),
                showcase = bsicons::bs_icon("graph-up")
              )
            ),
            layout_column_wrap(width = NULL,
                               style = css(grid_template_columns = "3fr 1fr"),
                               card(height = 700,
                                    card_header("Coin History",
                                                class = "d-flex justify-content-between",
                                                checkboxInput("coin_candlestick", "Candlestick", FALSE)
                                    ),
                                    plotlyOutput("crypto_plot") %>%
                                      shinycssloaders::withSpinner()
                               ),
                               card(fill = FALSE,
                                    card_header("Fear-Greed Index",
                                                tooltip(
                                                  bs_icon("info-circle"),
                                                  tagList("This measures market sentiment and can help to determine whether Bitcoin is under or overvalued. 
                                                  Find out more here:", a("Alternative.me", href="https://alternative.me/crypto/fear-and-greed-index/"))
                                                ),
                                      class = "d-flex justify-content-between",
                                    ),
                                    plotlyOutput("fgi_plot") %>%
                                      shinycssloaders::withSpinner()
                               )
            )
            )
)

# Server ----
server <- function(input, output, session) {
  
  # Main chart ----
  
  updateSelectizeInput(session, 'stock_picker', choices = company_tickers[["Company + Ticker"]], server = TRUE)
  updateSelectizeInput(session, 'stock_picker_multiple', choices = company_tickers[["Company + Ticker"]], server = TRUE)
  
  # Collects stock data when valid ticker is selected, uses functions from scripts.R
  stock_data_filtered <- reactive({
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
  
  # Collects stock data when mutiple valid tickers are selected, uses functions from scripts.R
  stock_data_filtered_multiple <- reactive({
    # Determines whether standard or advanced accordion is open and uses stock_picker or stock_search inputs accordingly
    if (input$main_accordion_multiple == "standard_multiple") {
      req(input$stock_picker_multiple != "") # req() only renders plots once ticker selected
      # filter finds the corresponding ticker for the stock name and 'pipes' this into stock price function
      filter(company_tickers, `Company + Ticker` %in% !!input$stock_picker_multiple)[["Ticker"]] %>%
        getStockPrice(dateRange = input$date) 
    } else if (input$main_accordion_multiple == "advanced_multiple") {
      req(input$stock_search_multiple != "")
      getStockPrice(toupper(input$stock_search_multiple), dateRange = input$date)
    }
  })
  
  # Separate function needed for current stock data for use in summary table
  stock_data_current <- reactive({
    if (input$main_accordion == "standard") {
      req(input$stock_picker != "") # req() only renders table once ticker selected
      filter(company_tickers, `Company + Ticker` == !!input$stock_picker)[["Ticker"]] %>%
        getStockPrice() 
    } else if (input$main_accordion == "advanced") {
      req(input$stock_search != "")
      getStockPrice(toupper(input$stock_search))
    }
  })
  
  # takes checkbox input and returns plot type
  stock_plot_type <- reactive({
    if (input$candlestick == TRUE) {
      "candlestick"
    } else {
      "scatter"
    }
  })

  # Generates plot as soon as stock is selected
  stock_plot <- reactive({
    # only renders plot once an accordion is selected
    getStockPlot(df=stock_data_filtered(), type=stock_plot_type())
  }) 
  
  # Generates plot only once search_button pressed (used in advanced accordion)
  stock_plot_w_button <- eventReactive(input$search_button, {
    getStockPlot(df=stock_data_filtered(), type=stock_plot_type())
  })

  # Links with 'plotlyOutput' in UI file to render plot
  output$main_plot <- renderPlotly({
    req(input$main_accordion == "standard" | input$main_accordion == "advanced")
    if (input$main_accordion == "advanced") {
      stock_plot_w_button()
    } else {
      stock_plot()
    }
  })
  
  # Summary Table ----
  
  stock_summary <- reactive({
    getStockSummary(df=stock_data_current())
  })
  
  stock_summary_w_button <- eventReactive(input$search_button | input$main_accordion == "standard", {
    getStockSummary(df=stock_data_current())
  })
  
  output$stock_summary_table <- renderTable({
    req(input$main_accordion)
    if (input$main_accordion == "advanced") {
      stock_summary_w_button() }
    else {
      stock_summary() 
    }
  })
  
  # Value boxes ----
  
  # Gets most recent close data 
  tail_stock_data_filtered <- reactive({
    tail(stock_data_filtered(), 1)
  })
  
  # Collates and formats recent close data
  value_boxes <- reactive({
    c(format(tail_stock_data_filtered()[["Close"]], big.mark = ","), # doesnt change, always latest
      paste0(round(tail_stock_data_filtered()[["Change"]], 1), "%"), # changes according to datepicker
      paste0(round(tail_stock_data_filtered()[["Growth"]], 1), "%")
    )
  })
  
  # Generates value boxes only when button pressed
  value_boxes_w_button <- eventReactive(input$search_button, {
    value_boxes()
  })
  
  # Determines whether to use value boxes with/without button depending on which accordion is in use
  value_boxes_array <- reactive({
    req(input$main_accordion == "standard" | input$main_accordion == "advanced")
    if (input$main_accordion == "standard") {
      value_boxes()
    } else if (input$main_accordion == "advanced") {
      value_boxes_w_button()
    }
  })

  # Link with value_box in UI to calculate and show latest close, volume and market cap
  output$price <- renderText({
    value_boxes_array()[1]
    }) 
  output$day_change <- renderText({
    value_boxes_array()[2]
  })
  output$total_change <- renderText({
    value_boxes_array()[3]
  })
  
  # Stock Comparison ----
  
  # Generates plot as soon as stock is selected
  stock_plot_multiple <- reactive({
    # only renders plot once an accordion is selected
    getStockPlot(df=stock_data_filtered_multiple(), type="growth")
  }) 
  
  # Generates plot only once search_button pressed (used in advanced accordion)
  stock_plot_w_button_multiple <- eventReactive(input$search_button_multiple, {
    getStockPlot(df=stock_data_filtered_multiple(), type="growth")
  })
  
  # Links with 'plotlyOutput' in UI file to render plot
  output$stock_comparison_plot <- renderPlotly({
    req(input$main_accordion_multiple == "standard_multiple" | input$main_accordion_multiple == "advanced_multiple")
    if (input$main_accordion_multiple == "advanced_multiple") {
     stock_plot_w_button_multiple()
    } else {
      stock_plot_multiple()
    }
  })
  
  # Crypto ----
  updateSelectizeInput(session, 'coin_picker', choices = coin_list_names, selected = "BTC", server = TRUE)

  coin_price <- reactive({
    req(input$coin_picker != "") # stops charts from timing out when making a selection
    # filter finds the corresponding ticker for the coin name and 'pipes' this into coin price function
    filter(coin_df, `Coin` == !!input$coin_picker)[["Ticker"]] %>%
      getCoinPrice(dateRange=input$date)
  })
  
  coin_price_now <- reactive({
    req(input$coin_picker != "")
    filter(coin_df, `Coin` == !!input$coin_picker)[["Ticker"]] %>% 
      getCoinPrice(dateRange=c(Sys.time()-days(1), Sys.time()), interval="1m")
    })
  
  coin_plot_type <- reactive({
    if (input$coin_candlestick == TRUE) {
      "candlestick"
    } else {
      "scatter"
    }
  })

  output$crypto_plot <- renderPlotly({
    # plot_ly(coin_price(), type="scatter", mode="lines", x=~Date, y=~Close)
    getStockPlot(coin_price(), type = coin_plot_type())
  })
  
  output$fgi_plot <- renderPlotly({
    getFgiPlot()
  })
  
  output$coin_price_box <- renderText({
    tail(coin_price_now(), 1)[["Close"]]
  })
  
  output$coin_day_change <- renderText({
    paste0(round(tail(coin_price(), 1)[["Change"]], 1), "%")
  })
  
  output$coin_total_change <- renderText({
    paste0(round(tail(coin_price(), 1)[["Growth"]], 1), "%")
  })

}

shinyApp(ui, server)