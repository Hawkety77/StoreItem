library(tidyverse)
library(tidymodels)
library(vroom)
library(embed)
library(plotly)
library(modeltime)
library(timetk)
library(forecast)

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

arima_model <- arima_reg(seasonal_period=365,
                         non_seasonal_ar=5, # default max p to tune
                         non_seasonal_ma=5, # default max q to tune
                         seasonal_ar=2, # default max P to tune
                         seasonal_ma=2, #default max Q to tune
                         non_seasonal_differences=2, # default max d to tune
                         seasonal_differences=2 #default max D to tune
) %>%
  set_engine("auto_arima")

arima_wf <- workflow() %>%
  add_recipe(my_recipe) %>%
  add_model(arima_model) %>%
  fit(data=training(cv_split))

## Cross-validate to tune model
cv_results <- modeltime_calibrate(arima_wf,
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
  modeltime_forecast(new_data = storeItem, h = "3 months") %>%
  rename(date=.index, sales=.prediction) %>%
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

arima_model <- arima_reg(seasonal_period=365,
                         non_seasonal_ar=5, # default max p to tune
                         non_seasonal_ma=5, # default max q to tune
                         seasonal_ar=2, # default max P to tune
                         seasonal_ma=2, #default max Q to tune
                         non_seasonal_differences=2, # default max d to tune
                         seasonal_differences=2 #default max D to tune
) %>%
  set_engine("auto_arima")

arima_wf <- workflow() %>%
  add_recipe(my_recipe) %>%
  add_model(arima_model) %>%
  fit(data=training(cv_split))

## Cross-validate to tune model
cv_results <- modeltime_calibrate(arima_wf,
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
    .interactive = FALSE)

es_fullfit <- cv_results %>%
  modeltime_refit(data = storeItem)

es_preds <- es_fullfit %>%
  modeltime_forecast(h = "3 months") %>%
  rename(date=.index, sales=.prediction) %>%
  select(date, sales) %>%
  full_join(., y=df_test, by="date") %>%
  select(id, sales)

plot4 <- es_fullfit %>%
  modeltime_forecast(h = "3 months", actual_data = storeItem) %>%
  plot_modeltime_forecast(.interactive=FALSE)

plotly::subplot(plot1, plot3, plot2, plot4, nrows = 2)
