library(tidyverse)
library(tidymodels)
library(vroom)
library(modeltime)
library(timetk)
library(prophet)

# setwd("./StoreDemand")
train <- vroom("train.csv")
test <- vroom("test.csv")

nStores <- max(train$store)
nItems <- max(train$item)

for(s in 1:nStores) {
  for(i in 1:nItems){
    storeItemTrain <- train %>%
      filter(store == s, item == i)
    storeItemTest <- test %>%
      filter(store == s, item == i)
    
    cv_split <- time_series_split(storeItemTrain,
                                  assess = "3 months",
                                  cumulative = TRUE)
    
    prophet_mod <- prophet_reg() %>%
      set_engine("prophet") %>%
      fit(sales ~ date, data = training(cv_split))
    
    model_tbl <- modeltime_table(prophet_mod)
    
    cv_results <- modeltime_calibrate(prophet_mod, new_data = testing(cv_split))
    
    fullfit <- cv_results %>%
      modeltime_refit(data = storeItemTrain)
    
    preds <- fullfit %>%
      modeltime_forecast(
        new_data = storeItemTest,
        actual_data = storeItemTrain) %>%
      mutate(sales = .value) %>%
      select(sales)
    
    if(s==1 & i==1){
      all_preds <- preds
    } else {
      all_preds <- bind_rows(all_preds, preds)
    }
  }
}

vroom_write(all_preds, file="submission.csv", delim=",")