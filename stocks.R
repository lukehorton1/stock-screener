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
  
  if (calculateReturns) {
    stock_price <- stock_price %>%
      dplyr::select(Date, Close) %>%
      # change measures % change in stock price each day/week/month
      dplyr::mutate(Change = (Close / lag(Close) - 1) * 100) %>%
      # cumulative change measures % change in stock price since data begins
      dplyr::mutate(Cumulative = (Close / first(Close) - 1) * 100)
  }
  return(stock_price)
}

# Demo Plots

plot_ly(getStockPrice(), type="scatter", mode="lines", x=~Date, y=~Close)

plot_ly(getStockPrice(calculateReturns=FALSE, dateRange = c(Sys.Date()-30, Sys.Date())), type="candlestick", x=~Date,
        open=~Open, close=~Close,
        high=~High, low=~Low)
