library(tidyquant)


run_calcs <- function(df, col, width) {
  
  sub = dplyr::pull(df, !!enquo(col)) 
  # print(sub)
  runstats = sub %>%
  {tibble(runmax=runMax(., n=width), runmin=runMin(., n=width))} %>%
    mutate(runrange = log(runmax) - log(runmin))
  
  cbind(date = df$index, close=sub, runstats)
}


calc_mas <- function(df, col) { 
  
  result = tq_mutate(df, select = col, 
                     mutate_fun = rollapply, 
                     width = 22, FUN = mean, 
                     na.rm = TRUE, 
                     col_rename = 'SMA22')
  
  result = tq_mutate(result, select = col, 
                     mutate_fun = rollapply, 
                     width = 52.25, FUN = mean, 
                     na.rm = TRUE, 
                     col_rename = 'SMA52')    
  
  result = mutate(result, log_diff = log(close) - log(lag(close)))
  
  return(result)
  
}

# exponential, arima, theta benchmark from hyndman
eat_ensemble <- function(y, h = ifelse(frequency(y) > 1, 2*frequency(y), 14)) {
  require(forecast)
  fets <- forecast(ets(y), h)
  farima <- forecast(auto.arima(y), h)
  ftheta <- thetaf(y, h)
  comb <- fets
  comb$mean <-(fets$mean + farima$mean + ftheta$mean)/3
  comb$method <- "ETS-ARIMA-Theta Combination"
  return(comb)
}



get_vol <- function(df, col) {
  
  rolling_vol <- function(series) sd(series)*sqrt(365.25/7)
  
  result = tq_mutate(df, select = col, 
                     mutate_fun = rollapply, 
                     width = 52, FUN = rolling_vol, 
                     col_rename = 'annualized_vol')
  
  result = tq_mutate(result, select = col, 
                     mutate_fun = rollapply, 
                     width = 52, FUN = sd, 
                     col_rename = 'weekly_vol')
  return(result)
  
}


calc_quantile <- function(dt, df, col) {
  
  res = filter(df, date <= dt) %>% 
    pull(col) 
  
  Fn = res %>% 
  {ecdf(.)(tail(.,1))}
  
  return(tibble(date = dt, percentile = Fn))
  
}

# create new df
analyze_ticker <- function(df, col1, col2) {
  
  result = run_calcs(df, col1, 52)
  result = calc_mas(result, col1)
  result = get_vol(result, col2)
  
  dates <- result$date
  quantiles = map(dates[60:length(dates)], calc_quantile, result, 'log_diff') %>% 
    bind_rows 
  
  return(left_join(result, quantiles))
  
}

create_period<-function(df, func){
  
  res <- as.xts(dplyr::select(df,-date), order.by=df$date) 
  
  period_ts <- res %>% 
    func 
  #return(period_ts)
  
  timetk::tk_tbl(period_ts) %>%
    rename_all(str_replace_all, '\\..', '') %>% 
    rename_all(tolower) %>%
    rename(date=index)
  
}


create_volume <- function(ticker, period_fun) {
  monthly <- tq_transmute(ticker, mutate_fun = period_fun)
  #mvol <- tq_transmute(ticker, select = 'volume', mutate_fun = period_fun, FUN = sum) 
  dplyr::select(monthly, date, close) -> res
  left_join(res, mvol) %>% head()
}

# long to wide format for two columns
# combined_w <- data.table::dcast(
#   data.table::setDT(combined),  
#   formula = date ~ symbol, 
#   fun.aggregate = dplyr::first, 
#   value.var = c('close', 'adjusted')
# )


compare_runcor <- function(series1, series2, df=na.omit(combined_w), summarize = TRUE) { 
  
  
  col_pair = paste0(colnames(dplyr::select(df, series1, series2)), 
                    collapse = '_')
  
  result = filter(df, date > "2006-01-01") %>%
    map_df(diff) %>%
    {TTR::runCor(x = pull(., series1), y = pull(., series2), n=50)} %>% 
    tibble(date=df[['date']][2:dim(df)[1]], 
           col_pair, 
           running_cor = .)
  
  if (summarize) {
    result[['running_cor']] %>% 
    {tibble(
      mean = mean(., na.rm = TRUE), 
      med = median(., na.rm = TRUE), 
      std = sd(., na.rm = TRUE),
      pair = col_pair
    ) 
    } %>%
      return()
    
  } else {
    return(result)
  }
}


# charting financial time series

series_chart <- function(data, n=26, m=52, title="Candlesticks", facet=FALSE) {
  
  gg <- ggplot(data, aes(x = lubridate::as_date(date), y = close)) 
  
  if (facet==FALSE) {
    
    gg + geom_candlestick(aes(open = open, high = high, low = low, close = close), 
                          size=0.5, alpha = 0.7,  na.rm=TRUE) +
      geom_ma(ma_fun = SMA, n = n, linetype = 5, color = "seagreen4", size = .5, na.rm=T) +
      geom_ma(ma_fun = SMA, n = m, color = "red", size = 1, na.rm=T) + 
      labs(title = title, y = "closing price", x = "date") +
      #scale_x_date(date_breaks = "6 months", date_labels = "%b-%Y") +
      theme_tq() + 
      theme(axis.text.x=element_text(angle=90, hjust=1), 
            plot.title = element_text(hjust = 0.5))
    
  }
  else {
    gg + geom_line(aes(color=symbol)) +
      facet_wrap(~symbol, scales='free_y') + 
      labs(title = title, y = "closing price", x = "date")
  }
  
}

# check cointegration
check_coint <- function(ticker1, ticker2) {
  
  #SPY_ <- window(SPY, start = '2015-02-09')
  fit <- lm(formula = get_close(ticker1) ~ get_close(ticker2))
  
  adf <- fit$residuals %>% 
    data.frame() %>% 
    as.matrix() %>% 
    tseries::adf.test()
  
  list(adf, fit)
}


