# press ctrl+I to enable/disable copilot suggestion
# run pip install -r requirements.txt
# run pip freeze > requirements.txt to save packages to requirements

#if struggling to import packages, change python interpreter using ctrl+shift+p and typing interpreter, then make sure 'path-to-venv' is selected.

import yfinance as yf
import yahooquery as yq
import plotly.express as px
import pandas as pd
from datetime import datetime, timedelta

# Get watchlists from yahoo finance using Yahooquery's screener
# create instance
s = yq.Screener()
#s.available_screeners # view screeners
#s.get_screeners # returns dictionary of screeners, we then select key (i.e. 'quotes')
# use get_screeners[...].keys() to view list of keys, then use get_screeners[...].get(key) with chosen key


def get_watchlist_tickers(watchlist="",
                          n=5,
                          query='quotes',
                          summary=True):
    if watchlist == "":
        return("Set watchlist to one of: ",s.available_screeners)
    else:
        df = pd.DataFrame(s.get_screeners([watchlist], n).get(watchlist).get(query))
        df_summary = df[['shortName', 'symbol', 'regularMarketPrice', 'regularMarketChangePercent', 'fiftyTwoWeekChangePercent']]
        if summary:
            return(df_summary)
        else:
            return(df)

# Headlines
day_gainers = get_watchlist_tickers("day_gainers")
day_losers = get_watchlist_tickers("day_losers")
year_gainers = get_watchlist_tickers("fifty_two_wk_gainers", 10)
year_losers = get_watchlist_tickers("fifty_two_wk_losers", 10)


def get_stock_history(ticker="GOOG AMZN AAPL META MSFT NVDA TSLA",
                      period="3mo",
                      interval="1d",
                      start = "", # datetime.now()-timedelta(days=365),
                      end = "", # datetime.now() 
                      ):
 
    df = yq.Ticker(ticker).history(period = period, interval = interval, start = start, end = end).reset_index(level="symbol")   # reset index with level parameter keeps only datetime as an index rather than ticker

    df["interval_return"] = df.groupby("symbol")["close"].pct_change()*100 # return since the previous data point
    df["cumulative_return"] = df.groupby("symbol")["interval_return"].cumsum() # return since the first data point

    return(df)

# Magnificent 7
mag7_3mo = get_stock_history(ticker="GOOG AMZN AAPL META MSFT NVDA TSLA", period="3mo", interval="60m")
mag7_1y = get_stock_history(ticker="GOOG AMZN AAPL META MSFT NVDA TSLA", period="1y")

# Day Gainers
day_gainers_3mo = get_stock_history(day_gainers['symbol'].tolist(), interval="60m")

# Day Losers
day_losers_3mo = get_stock_history(day_losers['symbol'].tolist(), interval="60m")
    
# Year Gainers
year_gainers_1y = get_stock_history(year_gainers['symbol'].tolist(), period="1y")

# Year Losers
year_losers_1y = get_stock_history(year_losers['symbol'].tolist(), period="1y")

    
def plot_stock_history(ticker="GOOG AMZN AAPL META MSFT NVDA TSLA", 
                       period = "3mo", 
                       interval = "1d", 
                       start = "", 
                       end = "",
                       cumulative = True, 
                       facet=False, 
                       ):
    df = get_stock_history(ticker = ticker, period = period, interval = interval, start = start, end = end)
    fig = px.line(df, x=df.index,
                  y="cumulative_return" if cumulative else "close", # uses inline if to decide whether to use return or close
                  facet_col="symbol" if facet else None,
                  facet_col_wrap=5 if facet else 0,
                  color="symbol")
    return(fig.show())

# make pie charts for both sector and top 10 holdings chart (top 10 holdings 33%, other 66% for example)

def get_fund_holdings(ticker="VWRP.L", includeSectors=True): 

    holdings = pd.DataFrame(yq.Ticker(ticker).fund_holding_info.get(ticker)["holdings"]).set_index("symbol")
    holdings.loc["Other"] = ["Other", 1-sum(holdings["holdingPercent"])] # adds new row at index 'Other' to calculate remaining percentage
    if includeSectors:
        sectors = (
            pd.DataFrame(yq.Ticker(ticker).fund_holding_info.get(ticker)["sectorWeightings"])
            .stack() # stack() pivots from wide to long format
            .reset_index(level=0, name = "sectorWeighting") # converts level 0 index to column with the name Sector Weighting
            .drop(columns="level_0") # drops redundant level 0 index left over from stack pivot
        )
        return([holdings, sectors])
    
    return(holdings)

def plot_stock_holdings(ticker="VWRP.L", sectors=False):
    if sectors:
        df = get_fund_holdings(ticker, includeSectors=True)[1]
        fig = px.pie(df,
                     values = "sectorWeighting",
                     names = df.index,
        )
    else:
        df = get_fund_holdings(ticker, includeSectors=False)
        fig = px.pie(df,
                     values = "holdingPercent",
                     names = df.index,
        )
    fig.update_traces(hole=0.5, hoverinfo="percent+name")
    fig.update_layout(
        annotations=[
            dict(
                text="Sectors" if sectors else "Holdings",
                x=0.5,
                y=0.5,
                font_size=20,
                xanchor="center",
                showarrow=False
                )
            ]
        )
    return(fig.show())
                

# similar to get_fund_holdings but works on a portfolio of ETFs and funds
# where more than one fund is given, calculates mean holdngs based on given allocations
def get_portfolio_xray(ticker="VWRP.L VUAG.L IITU.L DAGB.L", allocation = [0.25, 0.25, 0.25, 0.25]):
    
    df = yq.Ticker(ticker).fund_holding_info

    n = len(df.keys())

    for i in range(n):
        pass
    pass

# next add all of these to streamlit

# then add calendar events to charts. 

# also get headlines from the yahooquery packages
    

#next make custom plotly layout wrapper?
 
    

# Ticker details
#dir(yq.Ticker("NVDA"))



# # Gets info about ticker 

# def get_ticker_info(ticker):
#     try:
#         return(yf.Ticker(ticker).info)
#     except Exception as e:
#         print(f"Error getting ticker info: {e}")
#         return None

# def get_stock_history(ticker, period="1y"):
#     hist = yf.Ticker(ticker).history(period=period).reset_index()
#     return hist


# nvda_hist = get_stock_history("NVDA")

# nvda_fig = px.line(nvda_hist, x="Date", y="Close")
# nvda_fig.show()

