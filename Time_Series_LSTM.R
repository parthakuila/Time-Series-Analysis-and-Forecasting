# Library

# Core Tidyverse
library(tidyverse)
library(glue)
library(forcats)

# Time Series
library(timetk)
library(tidyquant)
library(tibbletime)

# Visualization
library(cowplot)

# Preprocessing
library(recipes)

# Sampling / Accuracy
library(rsample)
library(yardstick) 

# Modeling
library(keras)


# Data
#sun_spots <- datasets::sunspot.month %>%
  #tk_tbl() %>%
  #mutate(index = as_date(index)) %>%
  #as_tbl_time(index = index)

#sun_spots
df <- read.csv("/home/partha/Desktop/Link3/CTG_subscribe_data.csv")

#install.packages("readODS", dependencies = T)
#devtools::install_github("r-lib/xml2")
#library(readODS)
#df <- read_ods("/home/partha/Documents/Model_Docs/OLT_tp.ods")
head(df)
na.omit(df)
lapply(df, class)
df$DT <- paste(substr(as.character(df$recorddate),5,17))
head(df$DT)
df$DT <- as.POSIXct(strptime(df$DT, format='%b %d %Y'))
df$usage <- df$usage/(1024*1024*1024)
df$olt <- as.factor(df$olt) 
df$tp <- df$tp/1000
OLT<- as.character(unique(df$olt))
length(df$usage[df$olt==OLT[1]])
OLTts <- df[df$olt==OLT[9],] 

#OLTts <- OLTts[, c(63, 2:62, 1)]  # replace column
#OLTts <- dt %>% select(-recorddate )  


#dates <- seq(as.Date("2017-11-09"), as.Date("2018-06-14"), "days")
#DT <- as.POSIXct(dates)

#xts.ob <- xts(OLTts$tp, order.by = OLTts$DT, deltat=1/7)
#print(plot(xts.ob))

OLTts <- OLTts %>%
  tk_tbl() %>%
  mutate(index = as_date(DT)) %>%
  as_tbl_time(index = index) %>%
  select(-recorddate,-DT)
OLTts

# Expoloratory Data Analysis

p1 <- OLTts %>%
  ggplot(aes(index, usage)) +
  geom_point(color = palette_light()[[1]], alpha = 0.5) +
  theme_tq() +
  labs(
    title = "From 2017-08-04 to 2018-08-08 (Full Data Set)"
  )

p2 <- OLTts %>%
  filter_time("start" ~ "2018-08-08") %>%
  ggplot(aes(index, usage)) +
  geom_line(color = palette_light()[[1]], alpha = 0.5) +
  geom_point(color = palette_light()[[1]]) +
  geom_smooth(method = "loess", span = 0.2, se = FALSE) +
  theme_tq() +
  labs(
    title = "2017-08-04 to 2018-08-08 (Zoomed In To Show Cycle)",
    caption = "OLTts"
  )

p_title <- ggdraw() + 
  draw_label("OLTts", size = 18, fontface = "bold", colour = palette_light()[[1]])

plot_grid(p_title, p1, p2, ncol = 1, rel_heights = c(0.1, 1, 1))

# Evaluating The ACF

tidy_acf <- function(data, usage, lags = 0:20) {
  
  value_expr <- enquo(usage)
  
  acf_values <- data %>%
    pull(usage) %>%
    acf(lag.max = tail(lags, 1), plot = FALSE) %>%
    .$acf %>%
    .[,,1]
  
  ret <- tibble(acf = acf_values) %>%
    rowid_to_column(var = "lag") %>%
    mutate(lag = lag - 1) %>%
    filter(lag %in% lags)
  
  return(ret)
}
max_lag <- 7 * 50

OLTts %>%
  tidy_acf(usage , lags = 0:max_lag)

OLTts %>%
  tidy_acf(usage, lags = 0:max_lag) %>%
  ggplot(aes(lag, acf)) +
  geom_segment(aes(xend = lag, yend = 0), color = palette_light()[[1]]) +
  geom_vline(xintercept = 120, size = 3, color = palette_light()[[2]]) +
  annotate("text", label = "4 months Mark", x = 130, y = 0.8, 
           color = palette_light()[[2]], size = 6, hjust = 0) +
  theme_tq() +
  labs(title = "ACF: OLTts")

OLTts %>%
  tidy_acf(usage, lags = 115:135) %>%
  ggplot(aes(lag, acf)) +
  geom_vline(xintercept = 120, size = 3, color = palette_light()[[2]]) +
  geom_segment(aes(xend = lag, yend = 0), color = palette_light()[[1]]) +
  geom_point(color = palette_light()[[1]], size = 2) +
  geom_label(aes(label = acf %>% round(2)), vjust = -1,
             color = palette_light()[[1]]) +
  annotate("text", label = "4 months Mark", x = 121, y = 0.8, 
           color = palette_light()[[2]], size = 5, hjust = 0) +
  theme_tq() +
  labs(title = "ACF: OLTts",
       subtitle = "Zoomed in on Lags 115 to 135")

optimal_lag_setting <- OLTts %>%
  tidy_acf(usage, lags = 115:135) %>%
  filter(acf == max(acf)) %>%
  pull(lag)
# Backtesting: Time series cross validation

periods_train <- 7 * 50
periods_test  <- 7 * 2
skip_span     <- 7 * 1

rolling_origin_resamples <- rolling_origin(
  OLTts,
  initial    = periods_train,
  assess     = periods_test,
  cumulative = FALSE,
  skip       = skip_span
)

rolling_origin_resamples

# Plotting function for a single split
plot_split <- function(split, expand_y_axis = TRUE, alpha = 1, size = 1, base_size = 10) {
  
# Manipulate data
  train_tbl <- training(split) %>%
    add_column(key = "training") 
  
  test_tbl  <- testing(split) %>%
    add_column(key = "testing") 
  
  data_manipulated <- bind_rows(train_tbl, test_tbl) %>%
    as_tbl_time(index = index) %>%
    mutate(key = fct_relevel(key, "training", "testing"))
  
# Collect attributes
  train_time_summary <- train_tbl %>%
    tk_index() %>%
    tk_get_timeseries_summary()
  
  test_time_summary <- test_tbl %>%
    tk_index() %>%
    tk_get_timeseries_summary()
  
# Visualize
  g <- data_manipulated %>%
    ggplot(aes(x = index, y = usage, color = key)) +
    geom_line(size = size, alpha = alpha) +
    theme_tq(base_size = base_size) +
    scale_color_tq() +
    labs(
      title    = glue("Split: {split$id}"),
      subtitle = glue("{train_time_summary$start} to {test_time_summary$end}"),
      y = "", x = ""
    ) +
    theme(legend.position = "none") 
  
  if (expand_y_axis) {
    
    OLTts_time_summary <- OLTts %>% 
      tk_index() %>% 
      tk_get_timeseries_summary()
    
    g <- g +
      scale_x_date(limits = c(OLTts_time_summary$start, 
                              OLTts_time_summary$end))
  }
  
  return(g)
}
rolling_origin_resamples$splits[[1]] %>%
  plot_split(expand_y_axis = TRUE) +
  theme(legend.position = "bottom")

# Plotting function that scales to all splits 
plot_sampling_plan <- function(sampling_tbl, expand_y_axis = TRUE, 
                               ncol = 3, alpha = 1, size = 1, base_size = 14, 
                               title = "Sampling Plan") {
  
# Map plot_split() to sampling_tbl
  sampling_tbl_with_plots <- sampling_tbl %>%
    mutate(gg_plots = map(splits, plot_split, 
                          expand_y_axis = expand_y_axis,
                          alpha = alpha, base_size = base_size))
  
# Make plots with cowplot
  plot_list <- sampling_tbl_with_plots$gg_plots 
  
  p_temp <- plot_list[[1]] + theme(legend.position = "bottom")
  legend <- get_legend(p_temp)
  
  p_body  <- plot_grid(plotlist = plot_list, ncol = ncol)
  
  p_title <- ggdraw() + 
    draw_label(title, size = 18, fontface = "bold", colour = palette_light()[[1]])
  
  g <- plot_grid(p_title, p_body, legend, ncol = 1, rel_heights = c(0.05, 1, 0.05))
  
  return(g)
  
}

rolling_origin_resamples %>%
  plot_sampling_plan(expand_y_axis = T, ncol = 3, alpha = 1, size = 1, base_size = 10, 
                     title = "Backtesting Strategy: Rolling Origin Sampling Plan")

rolling_origin_resamples %>%
  plot_sampling_plan(expand_y_axis = F, ncol = 3, alpha = 1, size = 1, base_size = 10, 
                     title = "Backtesting Strategy: Zoomed In")

# Modelling The Keras Statefull LSTM Model

split    <- rolling_origin_resamples$splits[[1]]
split_id <- rolling_origin_resamples$id[[1]]

plot_split(split, expand_y_axis = FALSE, size = 0.5) +
  theme(legend.position = "bottom") +
  ggtitle(glue("Split: {split_id}"))

# Data Setup
df_trn <- training(split)
df_tst <- testing(split)

df <- bind_rows(
  df_trn %>% add_column(key = "training"),
  df_tst %>% add_column(key = "testing")
) %>% 
  as_tbl_time(index = index)

df

# Preprocessing with recipes

rec_obj <- recipe(usage ~ ., df) %>%
  step_sqrt(usage) %>%
  step_center(usage) %>%
  step_scale(usage) %>%
  prep()

df_processed_tbl <- bake(rec_obj, df)

df_processed_tbl

center_history <- rec_obj$steps[[2]]$means["usage"]
scale_history  <- rec_obj$steps[[3]]$sds["usage"]

c("center" = center_history, "scale" = scale_history)

# LSTM Plan
# Model inputs
lag_setting  <- 14 # = nrow(df_tst)
batch_size   <- 7
train_length <- 350
tsteps       <- 1
epochs       <- 30


# 2D and 3D train/test

# Training Set
lag_train_tbl <- df_processed_tbl %>%
  mutate(tp_lag = lag(tp, n = lag_setting)) %>%
  filter(!is.na(tp_lag)) %>%
  filter(key == "training") %>%
  tail(train_length)

x_train_vec <- lag_train_tbl$tp_lag
x_train_arr <- array(data = x_train_vec, dim = c(length(x_train_vec), 1, 1))

y_train_vec <- lag_train_tbl$tp
y_train_arr <- array(data = y_train_vec, dim = c(length(y_train_vec), 1))

# Testing Set
lag_test_tbl <- df_processed_tbl %>%
  mutate(
    tp_lag = lag(tp, n = lag_setting)
  ) %>%
  filter(!is.na(tp_lag)) %>%
  filter(key == "testing")

x_test_vec <- lag_test_tbl$tp_lag
x_test_arr <- array(data = x_test_vec, dim = c(length(x_test_vec), 1, 1))

y_test_vec <- lag_test_tbl$tp
y_test_arr <- array(data = y_test_vec, dim = c(length(y_test_vec), 1))

# Building LSTM model

model <- keras_model_sequential()

model %>%
  layer_lstm(units            = 50, 
             input_shape      = c(tsteps, 1), 
             batch_size       = batch_size,
             return_sequences = TRUE, 
             stateful         = TRUE) %>% 
  layer_lstm(units            = 50, 
             return_sequences = FALSE, 
             stateful         = TRUE) %>% 
  layer_dense(units = 1)

model %>% 
  compile(loss = 'mae', optimizer = 'adam')

model

# Fitting the LSTM model

for (i in 1:epochs) {
  model %>% fit(x          = x_train_arr, 
                y          = y_train_arr, 
                batch_size = batch_size,
                epochs     = 1, 
                verbose    = 1, 
                shuffle    = FALSE)
  
  model %>% reset_states()
  cat("Epoch: ", i)
  
}

# Predicting the LSTM model

# Make Predictions
pred_out <- model %>% 
  predict(x_test_arr, batch_size = batch_size) %>%
  .[,1] 

# Retransform values
pred_tbl <- tibble(
  index   = lag_test_tbl$index,
  value   = (pred_out * scale_history + center_history)^2
) 

# Combine actual data with predictions
tbl_1 <- df_trn %>%
  add_column(key = "actual")

tbl_2 <- df_tst %>%
  add_column(key = "actual")

tbl_3 <- pred_tbl %>%
  add_column(key = "predict")

# Create time_bind_rows() to solve dplyr issue
time_bind_rows <- function(data_1, data_2, index) {
  index_expr <- enquo(index)
  bind_rows(data_1, data_2) %>%
    as_tbl_time(index = !! index_expr)
}

ret <- list(tbl_1, tbl_2, tbl_3) %>%
  reduce(time_bind_rows, index = index) %>%
  arrange(key, index) %>%
  mutate(key = as_factor(key))

ret

# Assessing Performance of the LSTM on a single split


calc_rmse <- function(prediction_tbl) {
  
  rmse_calculation <- function(data) {
    data %>%
      spread(key = key, value = usage) %>%
      select(-index) %>%
      filter(!is.na(predict)) %>%
      rename(
        truth    = actual,
        estimate = predict
      ) %>%
      rmse(truth, estimate)
  }
  
  safe_rmse <- possibly(rmse_calculation, otherwise = NA)
  
  safe_rmse(prediction_tbl)
  
}
optimal_lag_setting

calc_rmse(ret)

# Visualizing the single predicting

# Setup single plot function
plot_prediction <- function(data, id, alpha = 1, size = 2, base_size = 7) {
  
  rmse_val <- calc_rmse(data)
  
  g <- data %>%
    ggplot(aes(index, usage, color = key)) +
    geom_point(alpha = alpha, size = size) + 
    theme_tq(base_size = base_size) +
    scale_color_tq() +
    theme(legend.position = "none") +
    labs(
      title = glue("{id}, RMSE: {round(rmse_val, digits = 1)}"),
      x = "", y = ""
    )
  
  return(g)
}

ret %>% 
  plot_prediction(id = split_id, alpha = 0.65) +
  theme(legend.position = "bottom")

# Creating an LSTM prediction function

predict_keras_lstm <- function(split, epochs = 300, ...) {
  
  lstm_prediction <- function(split, epochs, ...) {
    
# 5.1.2 Data Setup
    df_trn <- training(split)
    df_tst <- testing(split)
    
    df <- bind_rows(
      df_trn %>% add_column(key = "training"),
      df_tst %>% add_column(key = "testing")
    ) %>% 
      as_tbl_time(index = index)
    
# 5.1.3 Preprocessing
    rec_obj <- recipe(tp ~ ., df) %>%
      step_sqrt(tp) %>%
      step_center(tp) %>%
      step_scale(tp) %>%
      prep()
    
    df_processed_tbl <- bake(rec_obj, df)
    
    center_history <- rec_obj$steps[[2]]$means["tp"]
    scale_history  <- rec_obj$steps[[3]]$sds["tp"]
    
 # 5.1.4 LSTM Plan
    lag_setting  <- 14 # = nrow(df_tst)
    batch_size   <- 7
    train_length <- 203
    tsteps       <- 1
    epochs       <- epochs
    
# 5.1.5 Train/Test Setup
    lag_train_tbl <- df_processed_tbl %>%
      mutate(tp_lag = lag(tp, n = lag_setting)) %>%
      filter(!is.na(tp_lag)) %>%
      filter(key == "training") %>%
      tail(train_length)
    
    x_train_vec <- lag_train_tbl$tp_lag
    x_train_arr <- array(data = x_train_vec, dim = c(length(x_train_vec), 1, 1))
    
    y_train_vec <- lag_train_tbl$tp
    y_train_arr <- array(data = y_train_vec, dim = c(length(y_train_vec), 1))
    
    lag_test_tbl <- df_processed_tbl %>%
      mutate(
        tp_lag = lag(tp, n = lag_setting)
      ) %>%
      filter(!is.na(tp_lag)) %>%
      filter(key == "testing")
    
    x_test_vec <- lag_test_tbl$tp_lag
    x_test_arr <- array(data = x_test_vec, dim = c(length(x_test_vec), 1, 1))
    
    y_test_vec <- lag_test_tbl$tp
    y_test_arr <- array(data = y_test_vec, dim = c(length(y_test_vec), 1))
    
# 5.1.6 LSTM Model
    model <- keras_model_sequential()
    
    model %>%
      layer_lstm(units            = 29, 
                 input_shape      = c(tsteps, 1), 
                 batch_size       = batch_size,
                 return_sequences = TRUE, 
                 stateful         = TRUE) %>% 
      layer_lstm(units            = 29, 
                 return_sequences = FALSE, 
                 stateful         = TRUE) %>% 
      layer_dense(units = 1)
    
    model %>% 
      compile(loss = 'mae', optimizer = 'adam')
    
# 5.1.7 Fitting LSTM
    for (i in 1:epochs) {
      model %>% fit(x          = x_train_arr, 
                    y          = y_train_arr, 
                    batch_size = batch_size,
                    epochs     = 1, 
                    verbose    = 1, 
                    shuffle    = FALSE)
      
      model %>% reset_states()
      cat("Epoch: ", i)
      
    }
    
# 5.1.8 Predict and Return Tidy Data
# Make Predictions
    pred_out <- model %>% 
      predict(x_test_arr, batch_size = batch_size) %>%
      .[,1] 
    
# Retransform values
    pred_tbl <- tibble(
      index   = lag_test_tbl$index,
      value   = (pred_out * scale_history + center_history)^2
    ) 
    
# Combine actual data with predictions
    tbl_1 <- df_trn %>%
      add_column(key = "actual")
    
    tbl_2 <- df_tst %>%
      add_column(key = "actual")
    
    tbl_3 <- pred_tbl %>%
      add_column(key = "predict")
    
# Create time_bind_rows() to solve dplyr issue
    time_bind_rows <- function(data_1, data_2, index) {
      index_expr <- enquo(index)
      bind_rows(data_1, data_2) %>%
        as_tbl_time(index = !! index_expr)
    }
    
    ret <- list(tbl_1, tbl_2, tbl_3) %>%
      reduce(time_bind_rows, index = index) %>%
      arrange(key, index) %>%
      mutate(key = as_factor(key))
    
    return(ret)
    
  }
  
  safe_lstm <- possibly(lstm_prediction, otherwise = NA)
  
  safe_lstm(split, epochs, ...)
  
}

predict_keras_lstm(split, epochs = 10)

# Mapping the LSTM prediction funtion

sample_predictions_lstm_tbl <- rolling_origin_resamples %>%
  mutate(predict = map(splits, predict_keras_lstm, epochs = 30))

sample_predictions_lstm_tbl

# Assessing the backtested performance

sample_rmse_tbl <- sample_predictions_lstm_tbl %>%
  mutate(rmse = map_dbl(predict, calc_rmse)) %>%
  select(id, rmse)

sample_rmse_tbl

sample_rmse_tbl %>%
  ggplot(aes(rmse)) +
  geom_histogram(aes(y = ..density..), fill = palette_light()[[1]], bins = 16) +
  geom_density(fill = palette_light()[[1]], alpha = 0.5) +
  theme_tq() +
  ggtitle("Histogram of RMSE")

sample_rmse_tbl %>%
  summarize(
    mean_rmse = mean(rmse),
    sd_rmse   = sd(rmse)
  )

# Visualizing the backtested result

plot_predictions <- function(sampling_tbl, predictions_col, 
                             ncol = 3, alpha = 1, size = 2, base_size = 14,
                             title = "Backtested Predictions") {
  
  predictions_col_expr <- enquo(predictions_col)
  
# Map plot_split() to sampling_tbl
  sampling_tbl_with_plots <- sampling_tbl %>%
    mutate(gg_plots = map2(!! predictions_col_expr, id, 
                           .f        = plot_prediction, 
                           alpha     = alpha, 
                           size      = size, 
                           base_size = base_size)) 
  
# Make plots with cowplot
  plot_list <- sampling_tbl_with_plots$gg_plots 
  
  p_temp <- plot_list[[1]] + theme(legend.position = "bottom")
  legend <- get_legend(p_temp)
  
  p_body  <- plot_grid(plotlist = plot_list, ncol = ncol)
  
  
  
  p_title <- ggdraw() + 
    draw_label(title, size = 18, fontface = "bold", colour = palette_light()[[1]])
  
  g <- plot_grid(p_title, p_body, legend, ncol = 1, rel_heights = c(0.05, 1, 0.05))
  
  return(g)
  
}

sample_predictions_lstm_tbl %>%
  plot_predictions(predictions_col = predict, alpha = 0.5, size = 1, base_size = 10,
                   title = "Keras Stateful LSTM: Backtested Predictions")

# Predicting the next 10 years
predict_keras_lstm_future <- function(data, epochs = 300, ...) {
  
  lstm_prediction <- function(data, epochs, ...) {
    
# 5.1.2 Data Setup (MODIFIED)
    df <- OLTts
    
# 5.1.3 Preprocessing
    rec_obj <- recipe(tp ~ ., df) %>%
      step_sqrt(tp) %>%
      step_center(tp) %>%
      step_scale(tp) %>%
      prep()
    
    df_processed_tbl <- bake(rec_obj, df)
    
    center_history <- rec_obj$steps[[2]]$means["tp"]
    scale_history  <- rec_obj$steps[[3]]$sds["tp"]
    
# 5.1.4 LSTM Plan
    lag_setting  <- 14 # = nrow(df_tst)
    batch_size   <- 7
    train_length <- 203
    tsteps       <- 1
    epochs       <- epochs
    
# 5.1.5 Train Setup (MODIFIED)
    lag_train_tbl <- df_processed_tbl %>%
      mutate(tp_lag = lag(tp, n = lag_setting)) %>%
      filter(!is.na(tp_lag)) %>%
      tail(train_length)
    
    x_train_vec <- lag_train_tbl$tp_lag
    x_train_arr <- array(data = x_train_vec, dim = c(length(x_train_vec), 1, 1))
    
    y_train_vec <- lag_train_tbl$tp
    y_train_arr <- array(data = y_train_vec, dim = c(length(y_train_vec), 1))
    
    x_test_vec <- y_train_vec %>% tail(lag_setting)
    x_test_arr <- array(data = x_test_vec, dim = c(length(x_test_vec), 1, 1))
    
# 5.1.6 LSTM Model
    model <- keras_model_sequential()
    
    model %>%
      layer_lstm(units            = 29, 
                 input_shape      = c(tsteps, 1), 
                 batch_size       = batch_size,
                 return_sequences = TRUE, 
                 stateful         = TRUE) %>% 
      layer_lstm(units            = 29, 
                 return_sequences = FALSE, 
                 stateful         = TRUE) %>% 
      layer_dense(units = 1)
    
    model %>% 
      compile(loss = 'mae', optimizer = 'adam')
    
# 5.1.7 Fitting LSTM
    for (i in 1:epochs) {
      model %>% fit(x          = x_train_arr, 
                    y          = y_train_arr, 
                    batch_size = batch_size,
                    epochs     = 1, 
                    verbose    = 1, 
                    shuffle    = FALSE)
      
      model %>% reset_states()
      cat("Epoch: ", i)
      
    }
    
# 5.1.8 Predict and Return Tidy Data (MODIFIED)
# Make Predictions
    pred_out <- model %>% 
      predict(x_test_arr, batch_size = batch_size) %>%
      .[,1] 
    
# Make future index using tk_make_future_timeseries()
    idx <- data %>%
      tk_index() %>%
      tk_make_future_timeseries(n_future = lag_setting)
    
# Retransform values
    pred_tbl <- tibble(
      index   = idx,
      value   = (pred_out * scale_history + center_history)^2
    )
    
 # Combine actual data with predictions
    tbl_1 <- df %>%
      add_column(key = "actual")
    
    tbl_3 <- pred_tbl %>%
      add_column(key = "predict")
    
# Create time_bind_rows() to solve dplyr issue
    time_bind_rows <- function(data_1, data_2, index) {
      index_expr <- enquo(index)
      bind_rows(data_1, data_2) %>%
        as_tbl_time(index = !! index_expr)
    }
    
    ret <- list(tbl_1, tbl_3) %>%
      reduce(time_bind_rows, index = index) %>%
      arrange(key, index) %>%
      mutate(key = as_factor(key))
    
    return(ret)
    
  }
  
  safe_lstm <- possibly(lstm_prediction, otherwise = NA)
  
  safe_lstm(data, epochs)
  
}

future_OLTts_tbl <- predict_keras_lstm_future(OLTts, epochs = 30)

future_OLTts_tbl %>%
  filter_time("2018-08-08" ~ "end") %>%
  plot_prediction(id = NULL, alpha = 0.4, size = 1.5) +
  theme(legend.position = "bottom") +
  ggtitle("Usage: 14 Days Forecast", subtitle = "Forecast Horizon:")

