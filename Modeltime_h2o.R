# # The following two commands remove any previously installed H2O packages for R.
# #if ("package:h2o" %in% search()) { detach("package:h2o", unload=TRUE) }
# if ("h2o" %in% rownames(installed.packages())) { remove.packages("h2o") }
# 
# # Next, we download packages that H2O depends on.
# pkgs <- c("RCurl","jsonlite")
# for (pkg in pkgs) {
#   if (! (pkg %in% rownames(installed.packages()))) { install.packages(pkg) }
# }
# 
# # Now we download, install and initialize the H2O package for R.
# install.packages("h2o", type="source", repos="https://h2o-release.s3.amazonaws.com/h2o/rel-3.46.0/6/R")

# Finally, let's load H2O and start up an H2O cluster
library(h2o)
h2o.init()

library(tidymodels)
library(modeltime.h2o)
library(tidyverse)
library(timetk)
library(vroom)

# setwd("./StoreDemand")
train <- vroom("train.csv")
test <- vroom("test.csv")

storeItemTrain <- train %>%
  filter(store == 7, item == 27)
storeItemTest <- test %>%
  filter(store == 7, item == 27)

splits <- time_series_split(storeItemTrain, assess = "3 month", cumulative = TRUE)

recipe_spec <- recipe(sales ~ ., data = training(splits)) %>%
  step_timeseries_signature(date) 

train_tbl <- training(splits) %>% bake(prep(recipe_spec), .)
test_tbl  <- testing(splits) %>% bake(prep(recipe_spec), .)

# Initialize H2O
h2o.init(
  nthreads = -1,
  ip       = 'localhost',
  port     = 54321
)

# # Optional - Set H2O No Progress to remove progress bars
# h2o.no_progress()

model_spec <- automl_reg(mode = 'regression') %>%
  set_engine(
    engine                     = 'h2o',
    max_runtime_secs           = 5, 
    max_runtime_secs_per_model = 3,
    max_models                 = 3,
    nfolds                     = 5,
    exclude_algos              = c("DeepLearning"),
    verbosity                  = NULL,
    seed                       = 786
  ) 

model_spec

model_fitted <- model_spec %>%
  fit(sales ~ ., data = train_tbl)

cv_results <- modeltime_calibrate(model_fitted,
                                  new_data = test_tbl)

train_plot_1 <- cv_results%>%
  modeltime_forecast(
    new_data = test_tbl,
    actual_data = train_tbl
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