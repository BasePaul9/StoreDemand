library(tidyverse)
library(tidymodels)
library(vroom)
library(modeltime)
library(timetk)
library(prophet)

# setwd("./StoreDemand")
train <- vroom("train.csv")
test <- vroom("test.csv")

#Store-Item combo 1

storeItemTrain_1 <- train %>%
  filter(store == 7, item == 27)
storeItemTest_1 <- test %>%
  filter(store == 7, item == 27)

cv_split <- time_series_split(storeItemTrain_1,
                              assess = "3 months",
                              cumulative = TRUE)

prophet_mod <- prophet_reg() %>%
  set_engine("prophet") %>%
  fit(sales ~ date, data = training(cv_split))

cv_results <- modeltime_calibrate(prophet_mod,
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
  filter(store == 7, item == 27)
storeItemTest_2 <- test %>%
  filter(store == 7, item == 27)

cv_split <- time_series_split(storeItemTrain_2,
                              assess = "3 months",
                              cumulative = TRUE)

prophet_mod <- prophet_reg() %>%
  set_engine("prophet") %>%
  fit(sales ~ date, data = training(cv_split))

cv_results <- modeltime_calibrate(prophet_mod,
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
    actual_data = storeItemTrain_2) %>%
  plot_modeltime_forecast(.interactive = FALSE)
forecast_plot_2
  
plotly::subplot(train_plot_1, train_plot_2, forecast_plot_1, forecast_plot_2, nrows = 2)


  
  
  
  
  
  
  
  
  
  
  
  