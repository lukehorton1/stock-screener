# This file contains functions to read stock data using the cryptoQuotes package

# Variables
# Filters for only tickers which show exchange between a coin and USDT
coin_list <- cryptoQuotes::available_tickers() %>%
  .[grep("USDT", .)]

coin_list_names <- gsub("USDT", "", coin_list)

coin_df <- tibble("Ticker" = coin_list, "Coin" = coin_list_names)

# Functions

getCoinPrice <- function(ticker="BTCUSDT", dateRange = c(Sys.Date()-years(1), Sys.Date()), 
                           interval="1d", coin_data_type = "price",
                           calculateReturns = FALSE) {
  # searches for crypto price data 
  # tryCatch(
    if (coin_data_type == "lsratio") {
      coin_data <- cryptoQuotes::get_lsratio(ticker, interval=interval)
    } else {
      coin_data <- cryptoQuotes::get_quote(ticker, interval=interval,
                                           from=dateRange[[1]], to=dateRange[[2]])
    }#,
  #   error = function(e) { stop(paste0("Ticker not found")) }
  # )
  # tidies crypto price data 
  coin_data <- coin_data %>%
    fortify.zoo() %>% # makes index its own column so it is recognised by tibble 
    tibble() %>%
    dplyr::rename("Date" := Index)

  colnames(coin_data) <- stringr::str_to_title(colnames(coin_data))
  
  if (calculateReturns) {
    if (coin_data_type == "price") {
      coin_data <- coin_data %>%
        dplyr::select(Date, Close) %>%
        # change measures % change in crypto price each day/week/month
        dplyr::mutate(Change = (Close / lag(Close) - 1) * 100) %>%
        # cumulative change measures % change in crypto price since data begins
        dplyr::mutate(Cumulative = (Close / first(Close) - 1) * 100)
    } else {
      message("Cannot calculate returns for long-short ratio.")
    }
  }
  return(coin_data)
}

# Demo Plots

plot_ly(getCoinPrice(), type="scatter", mode="lines", x=~Date, y=~Close)

plot_ly(getCoinPrice(interval = "1h", calculateReturns=FALSE, dateRange = c(Sys.Date()-7, Sys.Date())), type="candlestick", x=~Date,
        open=~Open, close=~Close,
        high=~High, low=~Low)
