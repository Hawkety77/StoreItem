library(tidyverse)
library(tidymodels)
library(vroom)
library(patchwork)

df_train <- vroom('train.csv')
df_test <- vroom('test.csv')

nStores <- max(df_train$store)
nItems <- max(df_train$item)
for(s in 1:nStores){
  for(i in 1:nItems){
    storeItemTrain <- df_train %>%
    filter(store==s, item==i)
    storeItemTest <- df_test %>%
    filter(store==s, item==i)
    }
  }


storeItemTrain <- df_train %>%
  filter(store==1, item==1)
plot1 <- storeItemTrain %>%
  pull(sales) %>%
  forecast::ggAcf(.)
storeItemTrain <- df_train %>%
  filter(store==2, item==2)
plot2 <- storeItemTrain %>%
  pull(sales) %>%
  forecast::ggAcf(.)
storeItemTrain <- df_train %>%
  filter(store==3, item==3)
plot3 <- storeItemTrain %>%
  pull(sales) %>%
  forecast::ggAcf(.)
storeItemTrain <- df_train %>%
  filter(store==4, item==4)
plot4 <- storeItemTrain %>%
  pull(sales) %>%
  forecast::ggAcf(.)

(plot1 + plot2) / (plot3 + plot4)


