# This file contains essential functions for the app to work

# Functions

getStockPrice <- function(stock="AAPL", dateRange = c(Sys.Date()-1825, Sys.Date()), 
                          calculateReturns = FALSE) {
  # gets stock price and tidies into dplyr tibble
  stock_price <- getSymbols(stock, auto.assign = FALSE) %>%
    fortify.zoo() %>% # makes index its own column so it is recognised by tibble 
    tibble() %>%
    dplyr::rename("Date" := Index) %>% 
    dplyr::filter(between(Date, as.Date(dateRange[1]), as.Date(dateRange[2])))
  
  # removes ticker name from column names
  colnames(stock_price) <- gsub(paste0(stock,"."), "", colnames(stock_price))
  
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
