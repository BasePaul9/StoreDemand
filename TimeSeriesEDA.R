library(tidyverse)
library(tidymodels)
library(vroom)
library(patchwork)

# setwd("./StoreDemand")
train <- vroom("train.csv")
test <- vroom("test.csv")
sampleSub <- vroom("sample_submission.csv")

storeItem1 <- train %>%
  filter(store == 3, item == 1)

TS_plot_1 <- storeItem1 %>%
  ggplot(mapping = aes(x = date, y = sales)) +
  geom_line() +
  geom_smooth(se = FALSE)

ACF_mon_1 <- storeItem1 %>%
  pull(sales) %>%
  forecast::ggAcf(.)

ACF_yr_1 <- storeItem1 %>%
  pull(sales) %>%
  forecast::ggAcf(., lag.max = 730)

storeItem2 <- train %>%
  filter(store == 7, item == 27)

TS_plot_2 <- storeItem2 %>%
  ggplot(mapping = aes(x = date, y = sales)) +
  geom_line() +
  geom_smooth(se = FALSE)

ACF_mon_2 <- storeItem2 %>%
  pull(sales) %>%
  forecast::ggAcf(.)

ACF_yr_2 <- storeItem2 %>%
  pull(sales) %>%
  forecast::ggAcf(., lag.max = 730)


(TS_plot_1 | ACF_mon_1 | ACF_yr_1) / (TS_plot_2 | ACF_mon_2 | ACF_yr_2)












