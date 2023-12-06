library(tidyverse)
library(tidymodels)
library(vroom)
library(embed)
library(lightgbm)
library(bonsai)
library(plotly)
library(modeltime)
library(timetk)
library(prophet)

df_train <- vroom('train.csv')
df_test <- vroom('test.csv')

storeItem <- df_train %>%
  filter(store == 8, item == 20)

cv_split <- time_series_split(storeItem, assess="3 months", cumulative = TRUE)

cv_split %>%
  tk_time_series_cv_plan() %>% #Put into a data frame
  plot_time_series_cv_plan(date, sales, .interactive=FALSE)

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

prophet_model <- prophet_reg() %>%
  set_engine(engine = "prophet") %>%
  fit(sales ~ date, data = training(cv_split))

## Cross-validate to tune model
cv_results <- modeltime_calibrate(prophet_model,
                                  new_data = testing(cv_split))

## Visualize CV results
plot1 <- cv_results %>%
  modeltime_forecast(
    new_data = testing(cv_split),
    actual_data = storeItem
  ) %>%
  plot_modeltime_forecast(.interactive=TRUE)

## Evaluate the accuracy
cv_results %>%
  modeltime_accuracy() %>%
  table_modeltime_accuracy(
    .interactive = FALSE
  )

es_fullfit <- cv_results %>%
  modeltime_refit(data = storeItem)

es_preds <- es_fullfit %>%
  modeltime_forecast(h = "3 months") %>%
  rename(date=.index, sales=.value) %>%
  select(date, sales) %>%
  full_join(., y=df_test, by="date") %>%
  select(id, sales)

plot2 <- es_fullfit %>%
  modeltime_forecast(h = "3 months", actual_data = storeItem) %>%
  plot_modeltime_forecast(.interactive=FALSE)

###############################

storeItem <- df_train %>%
  filter(store == 9, item == 21)

cv_split <- time_series_split(storeItem, assess="3 months", cumulative = TRUE)
cv_split %>%
  tk_time_series_cv_plan() %>% #Put into a data frame
  plot_time_series_cv_plan(date, sales, .interactive=FALSE)

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

prophet_model <- prophet_reg() %>%
  set_engine(engine = "prophet") %>%
  fit(sales ~ date, data = training(cv_split))

## Cross-validate to tune model
cv_results <- modeltime_calibrate(prophet_model,
                                  new_data = testing(cv_split))

## Visualize CV results
plot3 <- cv_results %>%
  modeltime_forecast(
    new_data = testing(cv_split),
    actual_data = storeItem
  ) %>%
  plot_modeltime_forecast(.interactive=TRUE)

## Evaluate the accuracy
cv_results %>%
  modeltime_accuracy() %>%
  table_modeltime_accuracy(
    .interactive = FALSE
  )

es_fullfit <- cv_results %>%
  modeltime_refit(data = storeItem)

es_preds <- es_fullfit %>%
  modeltime_forecast(h = "3 months") %>%
  rename(date=.index, sales=.value) %>%
  select(date, sales) %>%
  full_join(., y=df_test, by="date") %>%
  select(id, sales)

plot4 <- es_fullfit %>%
  modeltime_forecast(h = "3 months", actual_data = storeItem) %>%
  plot_modeltime_forecast(.interactive=FALSE)

plotly::subplot(plot1, plot3, plot2, plot4, nrows = 2)
