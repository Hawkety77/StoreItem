library(tidyverse)
library(tidymodels)
library(vroom)
library(embed)
library(lightgbm)
library(bonsai)

df_train <- vroom('train.csv')
df_test <- vroom('test.csv')

storeItem <- df_train %>%
  filter(store == 3, item == 48)

my_recipe <- recipe(sales ~ ., data = storeItem) %>%
  step_date(date, features = 'dow') %>% 
  step_date(date, features = 'month') %>%
  step_date(date, features = 'year') %>%
  step_date(date, features = 'doy') %>%
  step_date(date, features = 'decimal') %>%
  step_lencode_glm(date_dow, outcome = vars(sales)) %>%
  step_lencode_glm(date_month, outcome = vars(sales)) %>%
  step_mutate(date_doy = as.factor(date_doy)) %>%
  step_lencode_glm(date_doy, outcome = vars(sales))

prepped <- prep(my_recipe)
baked <- bake(prepped, new_data = NULL)  

my_model <- boost_tree(tree_depth=tune(),
                       trees=tune(),
                       learn_rate=tune()) %>%
  set_engine("lightgbm") %>% 
  set_mode("regression")

rand_workflow <- workflow() %>%
  add_recipe(my_recipe) %>%
  add_model(my_model)

tuning_grid <- grid_regular(trees(c(100, 300)), 
                            learn_rate(), 
                            tree_depth(c(3, 7)), 
                            levels = 3)

folds <- vfold_cv(df_train, v = 5, repeats = 1)

tree_tune <- tune_grid(my_model,
                       my_recipe,
                       folds,
                       control = control_grid(save_workflow = TRUE),
                       grid = tuning_grid, 
                       metrics = metric_set(smape))

collect_metrics(tree_tune)
show_best(tree_tune, metric = 'smape')

best_tune <- tree_tune %>%
  select_best("rmse")

#    300          7        0.1 smape   standard    13.3     5  0.0160 Preprocessor1_Model27

# final_workflow <- rand_workflow %>%
#   finalize_workflow(best_tune) %>%
#   fit(data = df_train)
# 
# ggg_predictions <- predict(final_workflow, 
#                            new_data = df_test, 
#                            type = 'class')
# 
# submission <- ggg_predictions %>%
#   bind_cols(df_test) %>%
#   select(id, .pred_class) %>%
#   rename(type = .pred_class)
# 
# write_csv(submission, 'submission_lightgbm.csv')


