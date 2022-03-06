#' Follow short term trend after a price jump
#'
#' @param stocks vector of stock symbols such as c("C", "AAPL")
#' @param period number of days to look back
#' @param description description of the chart, stock symbol by default
#'
#' @export
#'
plot_momentum <- function(stocks, source = "tws",
                           ema1 = 5,
                           ema2 = 20,
                           ema3 = 200,
                           macd1 = 12,
                           macd2 = 26,
                           macd3 = 9,
                           period = 500,
                           zoom = 0.3){
  for (i in seq_along(stocks)){
    st <- stocks[i]
    cat(st)
    
    gg <- plot_momentum_(st, source, ema1, ema2, ema3, macd1,
                         macd2, macd3, period, zoom)
    print(gg)
    
    if (i == length(stocks)) break
    
    key <- readline(
      prompt = paste0("Press [Enter] to view ", i + 1, "/", length(stocks),
                      " stock, [Esc] to exit: ")
    )
  }
}



#' Plot momentum of a stock
#'

plot_momentum_ <- function(symbol, source = "tws",
                           ema1 = 5,
                           ema2 = 20,
                           ema3 = 200,
                           macd1 = 12,
                           macd2 = 26,
                           macd3 = 9,
                           period = 500,
                           zoom = 0.3){
  
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
  
  
  
  if (nrow(stk_0) > ema3){
    stk <- stk_0 %>%
      # add EMA
      tq_mutate(select = close, mutate_fun = EMA, n = ema1, col_rename = "ema1") %>%
      tq_mutate(select = close, mutate_fun = EMA, n = ema2, col_rename = "ema2") %>%
      tq_mutate(select = close, mutate_fun = EMA, n = ema3, col_rename = "ema3") %>%
      # add MACD
      tq_mutate(select = close,
                mutate_fun = MACD,
                nFast      = macd1,
                nSlow      = macd2,
                nSig       = macd3,
                maType     = EMA) %>%
      mutate(diff = macd - signal) %>%
      # scale for plot
      mutate(diff = diff / max(abs(diff), na.rm = TRUE),
             macd_max = max(abs(macd), na.rm = TRUE),
             macd = macd / macd_max,
             signal = signal / macd_max,
             macd_change = macd - lag(macd)) %>%
      mutate(macd_switch = case_when(
        lead(macd_change) > 0 & macd_change <= 0 ~ "low",
        lead(macd_change) < 0 & macd_change >= 0 ~ "high",
        TRUE ~ ""
      )) %>%
      drop_na() %>%
      mutate(macd_high = ifelse(macd_switch == "high", macd, NA),
             macd_low = ifelse(macd_switch == "low", macd, NA))
    
    zoom <- max(stk$high) * zoom
    stk %>%
      ggplot(aes(x = date)) +
      geom_candlestick(aes(open = open, high = high, low = low, close = close)) +
      geom_line(aes(date, ema3), color = "red") +
      geom_line(aes(date, ema2), color = "orange") +
      geom_line(aes(date, ema1), color = "green") +
      geom_line(aes(date, zoom * macd), color = "green") +
      geom_line(aes(date, zoom * signal), color = "orange") +
      geom_col(aes(date, 5 * zoom * macd_change), fill = "cyan", alpha = 0.5) +
      geom_point(aes(date, zoom * macd_high), color = "red") +
      geom_point(aes(date, zoom * macd_low), color = "blue") +
      # geom_line(aes(date, zoom * signal), color = "orange") +
      # geom_col(aes(date, zoom * diff), fill = "blue", alpha = 0.5) +
      labs(title = label,
           x = NULL,
           y = NULL) +
      theme_bw()
  } else if (nrow(stk_0) > ema2){
    stk <- stk_0 %>%
      # add EMA
      tq_mutate(select = close, mutate_fun = EMA, n = ema1, col_rename = "ema1") %>%
      tq_mutate(select = close, mutate_fun = EMA, n = ema2, col_rename = "ema2") %>%
      # add MACD
      tq_mutate(select = close,
                mutate_fun = MACD,
                nFast      = macd1,
                nSlow      = macd2,
                nSig       = macd3,
                maType     = EMA) %>%
      mutate(diff = macd - signal) %>%
      # scale for plot
      mutate(diff = diff / max(abs(diff), na.rm = TRUE),
             macd_max = max(abs(macd), na.rm = TRUE),
             macd = macd / macd_max,
             signal = signal / macd_max,
             macd_change = macd - lag(macd)) %>%
      mutate(macd_switch = case_when(
        lead(macd_change) > 0 & macd_change <= 0 ~ "low",
        lead(macd_change) < 0 & macd_change >= 0 ~ "high",
        TRUE ~ ""
      )) %>%
      drop_na() %>%
      mutate(macd_high = ifelse(macd_switch == "high", macd, NA),
             macd_low = ifelse(macd_switch == "low", macd, NA))
    
    zoom <- max(stk$high) * zoom
    stk %>%
      ggplot(aes(x = date)) +
      geom_candlestick(aes(open = open, high = high, low = low, close = close)) +
      geom_line(aes(date, ema2), color = "orange") +
      geom_line(aes(date, ema1), color = "green") +
      geom_line(aes(date, zoom * macd), color = "green") +
      geom_line(aes(date, zoom * signal), color = "orange") +
      geom_col(aes(date, 5 * zoom * macd_change), fill = "cyan", alpha = 0.5) +
      geom_point(aes(date, zoom * macd_high), color = "red") +
      geom_point(aes(date, zoom * macd_low), color = "blue") +
      # geom_line(aes(date, zoom * signal), color = "orange") +
      # geom_col(aes(date, zoom * diff), fill = "blue", alpha = 0.5) +
      labs(title = label,
           x = NULL,
           y = NULL) +
      theme_bw()
  }
}
