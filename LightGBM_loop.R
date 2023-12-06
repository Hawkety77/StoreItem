library(tidyverse)
library(vroom)
library(tidymodels)
library(yardstick)
library(doParallel)
library(embed) # target encoding
library(lightgbm)
library(bonsai)

cl <- makePSOCKcluster(7)
registerDoParallel(cl)

df_train <- vroom('train.csv')
df_test <- vroom('test.csv')

my_model <- boost_tree(tree_depth=2,
                       trees=1000,
                       learn_rate=.01) %>%
             set_engine("lightgbm") %>% 
             set_mode("regression")

nStores <- max(df_train$store)
nItems <- max(df_train$item)
for(s in 1:nStores){
  for(i in 1:nItems){
    storeItemTrain <- df_train %>%
      filter(store==s, item==i)
    storeItemTest <- df_test %>%
      filter(store==s, item==i)
    
    print(paste('Store:', s))
    print(paste('Item:', i))
    
    my_recipe <- recipe(sales ~ ., data=storeItemTrain) %>%
      step_date(date, features = c('dow', 'month', 'year', 'doy')) %>% 
      step_range(date_doy, min = 0 , max = pi) %>%
      step_mutate(sinDOY = sin(date_doy), cosDOY = cos(date_doy)) %>%
      step_lencode_mixed(all_nominal_predictors(), outcome = vars(sales)) %>%
      step_rm(date, store, item) %>%
      step_normalize(all_numeric_predictors())
                
    final_workflow <- workflow() %>%
      add_recipe(my_recipe) %>%
      add_model(my_model) %>%
      fit(data = storeItemTrain)
    
    preds <- predict(final_workflow, 
                    new_data = storeItemTest) %>%
              bind_cols(storeItemTest) %>%
              rename(sales = .pred) %>%
              select(id, sales)
    
    if(s==1 & i==1){
      all_preds <- preds
    } else {
      all_preds <- bind_rows(all_preds, preds)
    }
    
  }
}


stopCluster(cl)