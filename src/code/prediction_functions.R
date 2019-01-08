library(tidyquant)
library(forecast)

harmon <- function(x, y, freq, h=1, start, end, verbose = FALSE) {
  
  
  create_vars <- function(x, y, freq, h=h, start, end) {
    
    prev_time = end - 1/freq
    y_ = window(y, start = start, end = prev_time)
    x_ = as_tibble(fourier(y_, K = round(sqrt(freq))), xvar=window(x, start = start, end = prev_time))
    
    x1_ = add_column(timetk::tk_tbl(fourier(y_, K = round(sqrt(freq)), h=h)), xvar=window(x, start = end, end = end))
    list(y_, x_, x1_)
  }
  
  create_vars(ts(SPY$volume, frequency=5), 
              ts(SPY$close, frequency=5), 
              freq=5, h=1, start = 1, end=50)
  
  res = create_vars(x, y, freq, h=h, start, end)
  return(res)
  
  y_ = res[[1]]
  x_ = as.matrix(res[[2]])
  x1_ = as.matrix(res[[3]])
  
  if (verbose) {
    print(x1_)
    print(x_ %>% tail(1))
  }
  
  fit <- auto.arima(y_, xreg = x_, max.d=1)  
  forecast(fit, xreg = x1_)
  
}

# all predictions 

series_forecast <- function(series, freq, h, harmon = FALSE, xvars = weekly_analyzed, cols = c('week', 'close', 'weekly_vol')) {
  
  #filtered = filter(joined, series_id == series)
  #ts_ = ts(series, frequency = freq, start = filtered$timestamp[1] %>% {c(day(.), hour(.))})
  series = tsclean(series)
  ts_ = ts(series, frequency = freq) 
  st = time(ts_)[1]
  end = time(ts_)[length(ts_)[1]]
  x = xvars[,cols] %>% 
    ts(frequency = freq)
  
  #print('vars created')
  rw_preds <- rwf(ts_, drift = TRUE, h = h)
  
  #print('rw')
  
  #tb_preds = xts(x = filtered$consumption, order.by = filtered$timestamp) %>%   
  #tb_preds = ts_ %>% msts(seasonal.periods = c(freq, 52), ts.frequency = freq) %>% 
  tb_preds <- tbats(series) %>% forecast(h)
  
  stl_preds <- stlf(ts_, h = h) 
  
  #print('stl completed')
  theta_preds <- thetaf(series, h = h)
  arima_preds <- auto.arima(series) %>% forecast (h=h)
  #print('arima completed')
  expo_preds <- ets(series) %>% forecast (h=h)
  
  #print('exponential completed')
  
  if (length(ts_) > 104 & harmon) {
    
    
    
    out <- tryCatch( 
      {
        print(paste(length(ts_), 'data points, running harmonic regression'))
        harmon(x=x, y=lead(ts_, 1), freq = freq, h=h, 
               start = st, end = end)
      },
      
      error = function(cond) {
        message(paste('problem at row', end * freq))
        message("Here's the original error message:")
        message(cond)
        # Choose a return value in case of error
        return(NA)
      }
    )
    
    return(list(rw = rw_preds, harmonic = out, tbat = tb_preds, stl = stl_preds, theta = theta_preds, expo = expo_preds, arima_ = arima_preds))
  }
  
  else {
    return(list(rw = rw_preds, tbat = tb_preds, stl = stl_preds, theta = theta_preds, expo = expo_preds, arima_ = arima_preds))
  }
  
  
}

proc_df <- function(df) {
  # finalizing dataframe
  df <- mutate(df, price_y1 = lead(close,1), price_y2 = lead(close,2), price_y3 = lead(close, 3))
  # adding date feats
  df$week <- lubridate::week(df$date)
  df$year <- lubridate::year(df$date)
  return(df)
}


fcst_ticker <- function(df, start, end, freq, y_col='close', x_cols=c('week', 'log_diff', 'weekly_vol'), h, verbose=FALSE) {
  
  res <- filter(df, date >= !!enquo(start), date <= !!enquo(end)) 
  out <- series_forecast(series = dplyr::pull(res, y_col), freq=freq, xvars = res, h=h, cols=x_cols)
  if (verbose) {print.data.frame(res)}
  
  return(out)
}

# make 1 period predictions
create_preds_df <- function(df, series_len, start= '2018-10-01', end='2018-12-25', freq=5) {
  
  start_row <- sum(df$date <= start) + 1
  end_row <- sum(df$date <= end)
  
  stopifnot(start_row > series_len) 
  
  # create list of size df - series length
  fcst_list <- rep(list(1), length(start_row:end_row))
  
  dates = df$date
  # loop to build predictions df
  for (i in start_row:end_row) {
    
    print(i)
    print(c(dates[i-series_len], dates[i]))
    
    result  <- fcst_ticker(df, dates[i-series_len], 
                           dates[i], freq=freq, h = 1, x_cols =c('close', 'runrange')) %>% 
      map_df('mean')
    
    print(i-start_row+1)
    fcst_list[[i-start_row+1]] <- data.frame(date = dates[i], result)
  }
 
   return(map_df(fcst_list, rbind))
  
}

proc_df <- function(df) {
  # finalizing dataframe
  df <- mutate(df, price_y1 = lead(close,1), price_y2 = lead(close,2), price_y3 = lead(close, 3))
  # adding date feats
  df$week <- lubridate::week(df$date)
  df$year <- lubridate::year(df$date)
  return(df)
}
