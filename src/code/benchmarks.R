library(tidyquant)
library(forecast)
#source('prediction_functions.R')

mae <- function(preds, true) { mean(abs(true-preds)) }

mase <- function(preds, true) { 
  
  # non-seasonal calc, see https://en.wikipedia.org/wiki/Mean_absolute_scaled_error
  abs_error = abs(true-preds)
  diff = abs(true - dplyr::lag(true, 1))
  
  return(mean(abs_error, na.rm = TRUE) / mean(diff, na.rm = TRUE))
  
}

create_err_df <- function(func, df) {
  
  bench <- data.frame(
    
    random_walk = func(df$rw, df$price_y1),  
    tbat = func(df$tbat, df$price_y1),
    stl = func(df$stl, df$price_y1),
    exponential = func(df$expo, df$price_y1),
    arima = func(df$arima_, df$price_y1),
    harmon = func(df$harmon, df$price_y1)
    
  )
  return(bench)
  
}

run_benchmark <- function(df=weekly_analyzed, freq=22, from=150, to=160, step=10, start='2018-12-01') {
  tic('benchmark complete')
  series_lens <- seq.int(from, to, step)
  
  err_list <- rep(list(1), length(series_lens))
  i=1
  
  for (num in series_lens) {
    
    preds_df <- create_preds_df(df, series_len=num, freq=freq, start = start)  
    res <- left_join(preds_df, df)
    new_ <- proc_df(res)
    err_list[[i]] <- cbind(create_err_df(mase, new_), series_len = num, sample_size = dim(new_)[1])
    
    print(paste('cycle', i, 'complete out of', length(series_lens)))
    i = i + 1
  }
  
  res=bind_rows(err_list)
  toc()
  return(res)
}
