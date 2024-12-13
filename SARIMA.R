library(tidyverse)
library(tidymodels)
library(vroom)
library(modeltime)
library(timetk)

# setwd("./StoreDemand")
train <- vroom("train.csv")
test <- vroom("test.csv")

#Store-Item combo 1

storeItemTrain_1 <- train %>%
  filter(store == 7, item == 27)
storeItemTest_1 <- test %>%
  filter(store == 7, item == 27)

my_recipe <- recipe(sales ~ date, data = storeItemTrain_1) %>%
  step_rm(removals = c("store", "item")) %>%
  step_date(date, features = "doy") %>%
  step_range(date_doy, min = 0, max = pi) %>%
  step_mutate(sinDOY=sin(date_doy), cosDOY=cos(date_doy)) %>%
  step_rm(date_doy)

cv_split <- time_series_split(storeItemTrain_1,
                              assess = "3 months",
                              cumulative = TRUE)
cv_plot_1 <- cv_split %>%
  tk_time_series_cv_plan() %>%
  plot_time_series_cv_plan(date, sales, .interactive = FALSE)
cv_plot_1

arima_mod <- arima_reg(seasonal_period = 365,
                       non_seasonal_ar = 5,
                       non_seasonal_ma = 5,
                       seasonal_ar = 2,
                       seasonal_ma = 2,
                       non_seasonal_differences = 2,
                       seasonal_differences = 2) %>%
  set_engine("auto_arima")

arima_wf <- workflow() %>%
  add_recipe(my_recipe) %>%
  add_model(arima_mod) %>%
  fit(data = training(cv_split))

cv_results <- modeltime_calibrate(arima_wf,
                                  new_data = testing(cv_split))

train_plot_1 <- cv_results%>%
  modeltime_forecast(
    new_data = testing(cv_split),
    actual_data = training(cv_split)
  ) %>%
  plot_modeltime_forecast(.interactive = FALSE)
train_plot_1

fullfit <- cv_results %>%
  modeltime_refit(data = storeItemTrain_1)

forecast_plot_1 <- fullfit %>%
  modeltime_forecast(
    new_data = storeItemTest_1,
    actual_data = storeItemTrain_1
  ) %>%
  plot_modeltime_forecast(.interactive = FALSE)
forecast_plot_1

#Store-Item combo 2

storeItemTrain_2 <- train %>%
  filter(store == 3, item == 16)
storeItemTest_2 <- test %>%
  filter(store == 3, item == 16)

cv_split <- time_series_split(storeItemTrain_2,
                              assess = "3 months",
                              cumulative = TRUE)
cv_plot_2 <- cv_split %>%
  tk_time_series_cv_plan() %>%
  plot_time_series_cv_plan(date, sales, .interactive = FALSE)
cv_plot_2

arima_mod <- arima_reg(seasonal_period = 365,
                       non_seasonal_ar = 5,
                       non_seasonal_ma = 5,
                       seasonal_ar = 2,
                       seasonal_ma = 2,
                       non_seasonal_differences = 2,
                       seasonal_differences = 2) %>%
  set_engine("auto_arima")

arima_wf <- workflow() %>%
  add_recipe(my_recipe) %>%
  add_model(arima_mod) %>%
  fit(data = training(cv_split))

cv_results <- modeltime_calibrate(arima_wf,
                                  new_data = testing(cv_split))

train_plot_2 <- cv_results%>%
  modeltime_forecast(
    new_data = testing(cv_split),
    actual_data = training(cv_split)
  ) %>%
  plot_modeltime_forecast(.interactive = FALSE)
train_plot_2

fullfit <- cv_results %>%
  modeltime_refit(data = storeItemTrain_2)

forecast_plot_2 <- fullfit %>%
  modeltime_forecast(
    new_data = storeItemTest_2,
    actual_data = storeItemTrain_2
  ) %>%
  plot_modeltime_forecast(.interactive = FALSE)
forecast_plot_2


plotly::subplot(cv_plot_1, cv_plot_2, forecast_plot_1, forecast_plot_2, nrows = 2)









