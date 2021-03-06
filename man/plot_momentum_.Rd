% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fct_plot_momentum.R
\name{plot_momentum_}
\alias{plot_momentum_}
\title{Plot momentum of a stock}
\usage{
plot_momentum_(
  symbol,
  source = "tws",
  ema1 = 5,
  ema2 = 20,
  ema3 = 200,
  macd1 = 12,
  macd2 = 26,
  macd3 = 9,
  period = 500,
  zoom = 0.3
)
}
\arguments{
\item{symbol}{stock symbol such as "AAPL"}

\item{source}{where is the data from, "yahoo" for Yahoo Finance or "tws" for
for Interactive Brokers TWS. Need a account and data subscription for TWS.}

\item{ema1}{integer, short exponential moving average (EMA)}

\item{ema2}{middle EMA}

\item{ema3}{long EMA}

\item{macd1}{integer, MACD fast MA}

\item{macd2}{MACD slow MA}

\item{macd3}{MACD signal}

\item{period}{number of days to look back}

\item{zoom}{numeric scale of the MACD to display}
}
\description{
Plot momentum of a stock
}
