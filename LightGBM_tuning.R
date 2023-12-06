library(tidyverse)
library(vroom)
library(tidymodels)
library(yardstick)
library(doParallel)
library(embed) # target encoding
library(lightgbm)
library(bonsai)

df_train <- vroom('train.csv') # %>%
  # filter(store == 5, item == 16)
df_test <- vroom('test.csv') # %>%
  # filter(store == 5, item == 16)

cl <- makePSOCKcluster(7)
registerDoParallel(cl)

my_recipe <- recipe(sales ~ ., data=df_train) %>%
  step_date(date, features = c('dow', 'month', 'year', 'doy')) %>% 
  step_range(date_doy, min = 0 , max = pi) %>%
  step_mutate(sinDOY = sin(date_doy), cosDOY = cos(date_doy)) %>%
  step_lencode_mixed(all_nominal_predictors(), outcome = vars(sales)) %>%
  step_rm(date, store, item) %>%
  step_normalize(all_numeric_predictors())

prep <- prep(my_recipe)
baked <- bake(prep, new_data = NULL)

my_model <- boost_tree(tree_depth=tune(),
                       trees=tune(),
                       learn_rate=tune()) %>%
  set_engine("lightgbm") %>% 
  set_mode("regression")

rand_workflow <- workflow() %>%
  add_recipe(my_recipe) %>%
  add_model(my_model)

tuning_grid <- crossing(trees = c(300, 400, 500), 
                        learn_rate = c(.005, .01, .05), 
                        tree_depth = c(2, 3, 5))

folds <- vfold_cv(df_train, v = 5, repeats = 1)

tree_tune <- tune_grid(my_model,
                       my_recipe,
                       folds,
                       control = control_grid(save_workflow = TRUE),
                       metrics = metric_set(smape),
                       grid = tuning_grid)

runs <- collect_metrics(tree_tune)
show_best(tree_tune, metric = 'smape')

best_tune <- tree_tune %>%
  select_best("smape")

stopCluster(cl)