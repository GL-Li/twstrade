#' download_stock 
#'
#' @description Download price data of a stock
#' 
#' @param symbol stock symbol such as "AAPL"
#' @param source where is the data from, "yahoo" for Yahoo Finance or "tws" for
#' for Interactive Brokers TWS. Need a account and data subscription for TWS.
#' @param period number of days to look back
#'
#' @return data frame
#'
#' @export

download_stock <- function(symbol, source, period = 500) {
  if (str_detect(symbol, "=")){
    label <- str_extract(symbol, "[^=]+$") %>%
      str_squish()
    symbol <- str_extract(symbol, "^[^=]+") %>%
      str_squish()
  } else {
    label <- symbol
  }
  
  if (source == "yahoo") {
    stk_0 <- tq_get(symbol, from = Sys.Date() - period)
    
  } else if (source == "tws") {
    # tws only allow 1 year data, older data from yahoo
    stk_yahoo <- tq_get(symbol, from = Sys.Date() - period)
    
    # recent 5 days data from tws
    equity <- twsSTK(symbol)
    stk_0 <- reqHistoricalData(tws, equity, duration = "5 D") %>%
      as_tibble(rownames = "date") %>%
      mutate(symbol = symbol,
             date = as.Date(date),
             adjusted = 1) %>%  # as place holder
      relocate(symbol, .before = date)
    
    new_col <- names(stk_0) %>%
      str_remove("^[A-Z]*\\.") %>%
      tolower()
    
    names(stk_0) <- new_col
    stk_0 <- stk_0 %>%
      select(all_of(names(stk_yahoo)))
    
    # combine yahoo and tws
    stk_yahoo_old <- stk_yahoo %>%
      filter(! date %in% stk_0$date)
    
    stk_0 <- bind_rows(stk_yahoo_old, stk_0)
  }
  
  return(stk_0)
}
