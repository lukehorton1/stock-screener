# This file contains functions to read stock data using quantmod

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

# Functions

getStockPrice <- function(ticker="AAPL", dateRange = c(Sys.Date()-years(2), Sys.Date()), 
                          calculateReturns = FALSE) {
  # searches for stock price data 
  tryCatch(
    stock_price <- getSymbols(ticker, auto.assign = FALSE),
    error = function(e) { stop(paste0("Ticker not found")) }
  )
  # tidies stock price data 
  stock_price <- stock_price %>%
    fortify.zoo() %>% # makes index its own column so it is recognised by tibble 
    tibble() %>%
    dplyr::rename("Date" := Index) %>% 
    dplyr::filter(between(Date, as.Date(dateRange[1]), as.Date(dateRange[2])))
  
  # removes ticker name from column names
  colnames(stock_price) <- gsub(paste0(ticker,"."), "", colnames(stock_price))
  
  # Calculates daily and cumulative returns
  stock_price <- stock_price %>%
    # change measures % change in stock price each day/week/month
    dplyr::mutate(Change = (Close / lag(Close) - 1) * 100) %>%
    # cumulative change measures % change in stock price since data begins
    dplyr::mutate(Cumulative = (Close / first(Close) - 1) * 100)
    
  return(stock_price)
}

getStockSummary <- function(df=getStockPrice()) {
  summary <- tibble(
    `Open` = as.character(round(df[[nrow(df), "Open"]], 2)),
    `High` = as.character(round(df[[nrow(df), "High"]], 2)),
    `Low` = as.character(round(df[[nrow(df), "Low"]], 2)),
   ) 
  if (nrow(df) > 5) {
    summary <- dplyr::mutate(summary, 
          `1-Week Range` = paste0(round(min(tail(df[["Low"]], 5)), 2), " - ",
                                  round(max(tail(df[["High"]], 5)), 2)))
  }
  if (nrow(df) > 251) {
    summary <- dplyr::mutate(summary, 
          `52-Week Range` = paste0(round(min(tail(df[["Low"]], 251)), 2), " - ",
                                   round(max(tail(df[["High"]], 251)), 2)))
  }
  summary <- pivot_longer(summary, everything(), names_to = "Category", values_to = "Value") %>%
    dplyr::rename(!!format(df[[nrow(df), "Date"]], "%d %b %Y") := "Value")
  return(summary)
}

getStockPlot <- function(df=getStockPrice(), type="scatter") {
  if (type=="candlestick") {
    p <- plot_ly(df, type="candlestick", x=~Date,
            open=~Open, close=~Close,
            high=~High, low=~Low) %>% plotly_config()
  } else {
    p <- plot_ly(df, type="scatter", mode="lines", x=~Date, y=~Close) %>% plotly_config()
  }
  return(p)
}

# Demo Plots

# getStockPlot()

# getStockPlot(type="candlestick")

