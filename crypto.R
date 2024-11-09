# This file contains functions to read stock data using the cryptoQuotes package

# Variables

crypto_source = "kraken" # run cryptoQuotes::available_exchanges()
# Filters for only tickers which show exchange between a coin and USDT
coin_list <- cryptoQuotes::available_tickers(source = crypto_source) %>%
  .[grep("PF_", .)] # PF: Perpetual Linear Multi-Collateral Derivatives

coin_list_names <- gsub("PF_", "", coin_list) %>%
  gsub("USD", "", .) %>%
  gsub("XBT", "BTC", .) # replaces XBT (bitcoin) with BTC

coin_df <- tibble("Ticker" = coin_list, "Coin" = coin_list_names)

# Kraken allows only 2000 rows per api request, this function makes best use of these 
# rows by increasing granularity (interval) when smaller date ranges are used
intervalChecker <- function(dateRange) {
  daysRequested <- as.numeric(dateRange[2]-dateRange[1])
  hoursRequested <- daysRequested*24
  if (between(hoursRequested, 500, 1000)) {
    return("30m")
  } else if (between(hoursRequested, 1000, 2000)) {
    return("1h")
  } else if (between(hoursRequested, 2000, 8000)) {
    return("4h")
  } else if (between(hoursRequested, 8000, 24000)) {
    return("12h")
  } else if (between(daysRequested, 1000, 2000)) {
    return("1d")
  } else if (daysRequested >= 2000) {
    return("1w")
  } else {
    return("15m")
  }
}

# Functions

getCoinPrice <- function(ticker="PF_XBTUSD", dateRange = c(Sys.Date()-years(1), Sys.Date()),
                         coin_data_type = "price",
                         source = crypto_source,
                         interval="",
                         calculateReturns = FALSE) {
  # searches for crypto price data 
  # tryCatch(
    if (coin_data_type == "lsratio") {
      coin_data <- cryptoQuotes::get_lsratio(ticker, interval="30m", source=source)
    } else {
      coin_data <- cryptoQuotes::get_quote(ticker, 
                                           # uses custom interval if specified, otherwise uses above intervalChecker()
                                           interval=ifelse(interval=="", intervalChecker(dateRange), interval),
                                           source=source,
                                           from=dateRange[[1]], to=dateRange[[2]])
    }
  # tidies crypto price data 
  coin_data <- coin_data %>%
    fortify.zoo() %>% # makes index its own column so it is recognised by tibble 
    tibble() %>%
    dplyr::rename("Date" := Index)

  colnames(coin_data) <- stringr::str_to_title(colnames(coin_data))
  
  if (coin_data_type == "price") {
    coin_data <- coin_data %>%
      # change measures % change in crypto price each day/week/month
      dplyr::mutate(Change = (Close / lag(Close) - 1) * 100) %>%
      # cumulative change measures % change in crypto price since data begins
      dplyr::mutate(Cumulative = (Close / first(Close) - 1) * 100)
  } else {
    message("Cannot calculate returns for long-short ratio.")
  }
  
  return(coin_data)
}

getFgiPlot <- function() {
  fgIndex <- as.numeric(cryptoQuotes::get_fgindex(Sys.Date()))
  
  fgiTibble <- tibble(`Sentiment` = c("Fear", "Greed"),
                      `Index` = c(100-fgIndex, fgIndex)
  )
  
  fig <- plot_ly(fgiTibble, labels = ~Sentiment, values = ~Index, type = "pie",
                 marker = list(colors = c("#DC143C", "#50C878")),
                 textinfo='value',
                 hoverinfo='label',
                 showlegend = FALSE) %>%
    config(displayModeBar = FALSE) %>%
    layout(margin = list(pad=0, l=18))
  
  return(fig)
}

# Demo Plots

plot_ly(getCoinPrice(), type="scatter", mode="lines", x=~Date, y=~Close)

plot_ly(getCoinPrice(calculateReturns=FALSE, dateRange = c(Sys.Date()-7, Sys.Date())), type="candlestick", x=~Date,
        open=~Open, close=~Close,
        high=~High, low=~Low)
