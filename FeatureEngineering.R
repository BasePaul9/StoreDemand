library(tidyverse)
library(tidymodels)
library(vroom)
library(dbarts)
library(ranger)
library(glmnet)

# setwd("./StoreDemand")
train <- vroom("train.csv")
test <- vroom("test.csv")

storeItem <- train %>%
  filter(store == 7, item == 27)

my_recipe <- recipe(sales ~ date, data = storeItem) %>%
  step_rm(removals = c("store", "item")) %>%
  step_date(date, features = "doy") %>%
  step_range(date_doy, min = 0, max = pi) %>%
  step_mutate(sinDOY=sin(date_doy), cosDOY=cos(date_doy)) %>%
  step_rm(date)
  #step_date(date, features = c("dow", "month", "year"))
  

baked_data <- bake(prep(my_recipe), new_data = storeItem)

#### LINEAR REG ####

lm_model <- linear_reg(penalty = tune(),
                       mixture = tune()) %>%
  set_mode("regression") %>%
  set_engine("glmnet")

lm_wf <- workflow() %>%
  add_recipe(my_recipe) %>%
  add_model(lm_model)

tuning_grid <- grid_regular(penalty(),
                            mixture(c(0,1)),
                            levels = 5) ## L^2 total tuning possibilities

## Split data for CV
folds <- vfold_cv(storeItem, v = 5, repeats = 1)

cv_results <- lm_wf %>%
  tune_grid(resamples=folds,
            grid=tuning_grid,
            metrics = metric_set(smape))

show_best(cv_results, n = 1, metric = "smape")


#### BART ####

# bart_model <- parsnip::bart(
#   mode = "regression",
#   engine = "dbarts",
#   trees = 500,
#   prior_terminal_node_coef = tune(),
#   prior_terminal_node_expo = tune(),
#   prior_outcome_range = 2
# )
# 
# bart_wf <- workflow() %>%
#   add_recipe(my_recipe) %>%
#   add_model(bart_model)
# 
# ## Grid of values to tune over
# tuning_grid <- grid_regular(prior_terminal_node_coef(c(0.1, 1)),
#                             prior_terminal_node_expo(c(0.5, 2)),
#                             levels = 3) ## L^2 total tuning possibilities
# 
# ## Split data for CV
# folds <- vfold_cv(train, v = 5, repeats = 1)
# 
# cv_results <- bart_wf %>%
#   tune_grid(resamples=folds,
#             grid=tuning_grid)
# 
# show_best(cv_results, n = 1)

#### RANDOM FORESTS ####

# rf_mod <- rand_forest(mtry = tune(),
#                       min_n = tune(),
#                       trees = 500) %>%
#   set_engine("ranger") %>%
#   set_mode("regression")
# 
# rf_wf <- workflow() %>%
#   add_recipe(my_recipe) %>%
#   add_model(rf_mod)
# 
# tuning_grid <- grid_regular(mtry(range = (c(1,(ncol(baked_data))-1))),
#                             min_n(),
#                             levels = 5) ## L^2 total tuning possibilities
# 
# folds <- vfold_cv(train, v = 5, repeats=1)
# 
# cv_results <- rf_wf %>%
#   tune_grid(resamples = folds,
#             grid = tuning_grid,
#             metrics = metric_set(smape))
# 
# show_best(cv_results, n = 1)
# 
# bestTune <- cv_results %>%
#   select_best(metric_set("smape"))
#
# final_wf <- rf_wf %>%
#   finalize_workflow(bestTune) %>%
#   fit(data = train)
#
# rf_preds <- predict(final_wf, new_data = test)
#
# kag_sub <- rf_preds %>%
#   bind_cols(., test) %>% #Bind predictions with test data
#   select(id, .pred_1) %>% #Just keep datetime and prediction variables
#   rename(ACTION=.pred_1) %>% #rename pred to count (for submission to Kaggle)
#   mutate(ACTION=pmax(0, ACTION))
#
# vroom_write(x=kag_sub, file="./RandForest.csv", delim=",")