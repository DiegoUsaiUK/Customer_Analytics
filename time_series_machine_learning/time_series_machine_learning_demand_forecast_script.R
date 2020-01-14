
library(tidyverse)
library(lubridate)
library(readr)
library(TSstudio)
library(scales)
library(plotly)
library(h2o)
library(vip)
library(gridExtra)
library(knitr)

# Import orders
orders_tmp <- 
   read_rds("orders_tbl.rds")


### Initial exploration


revenue_tmp <- 
   orders_tmp %>% 
   # filter out final month of the series, which is incomplete
   filter(order_date <= "2007-06-25") %>% 
   select(order_date, revenue) %>%
   mutate(order_date = 
             floor_date(order_date, 
                        unit = 'week', 
                        # setting up week commencing Monday
                        week_start = getOption("lubridate.week.start", 1))) %>%
   group_by(order_date) %>%
   summarise(revenue   = sum(revenue)) %>%
   ungroup()


revenue_tmp %>% str()



#Let's take a closer look to see what's happening:
revenue_tmp %>% head(10)

#before converting my data to a `ts` object I need to:
   
#- __Add 1 observation at the beginning of the series__ to insure that the first year includes 52 weekly observations

#- __All is arranged in chronological order.__ This is especially important because the output may not be correctly mapped to the actual index of the series, leading to inaccurate results.

#- __Fill the gaps in incomplete datetime variables.__ The `pad` function from the `padr` library inserts a record for each of the missing time points (the default fill value is NA)

#- __Replace missing values with a zero__ as there are no sales recorded against the empty weeks. The `fill_by_value` function from the `padr` library helps with that.


revenue_tbl <- 
   revenue_tmp %>% 
   rbind(list('2004-01-05', NA, NA)) %>%
   arrange(order_date) %>% 
   padr::pad() %>% 
   padr::fill_by_value(value = 0) 

revenue_tbl %>% summary()


#Now I can take a look at __weekly revenue__, my response variable
(revenue_tbl %>% 
      ggplot(aes(x = order_date, y = revenue)) + 
      geom_line(colour = 'darkblue') +
      theme_light() +
      scale_y_continuous(labels = scales::dollar_format(scale = 1e-6, 
                                                        suffix = "m")) +
      labs(title = 'Weekly Revenue - 2004 to June 2007',
           x = "",
           y = 'Revenue ($m)')
) %>% ggplotly()

## Exploratory analysis


# define the `start` (or `end`) argument of the series
start_point_wk <-  c(1,1)


# I create the `ts` object by selecting the response variable (`revenue`) 
# as the data argument and specifying a frequency of 52 weeks. 
ts_weekly <- 
   revenue_tbl %>% 
   # filter(order_date <= "2006-12-31") %>% 
   select(revenue) %>%
   ts(start = start_point_wk,
      frequency = 52)

# Checking the series attributes with the `ts_info()` function
ts_info(ts_weekly)


### Time series components

# ts_decompose breaks down the series into its elements: 
# __Trend__, __Seasonality__, and __Random__ components. 
ts_decompose(ts_weekly, type = 'additive')


### Seasonal component

# Let's now zoom in on the __seasonal component__ of the series
ts_seasonal(ts_weekly, type = 'normal')


### Correlation analysis

# The autocorrelation function (ACF)
ts_acf(ts_weekly, lag.max = 52)


# Lag Visualizations - quarterly
ts_lags(ts_weekly, lags = c(13, 26, 39, 52))
```
# Lag Visualizations - yearly
ts_lags(ts_weekly, lags = c(52, 104, 156))


## Modelling 

# a visual representation of the strategy
revenue_tbl %>%
   filter(order_date >= "2005-01-03") %>% 
   ggplot(aes(order_date, revenue)) +
   geom_line(colour = 'black', size = 0.7) +
   geom_point(colour = 'black', size = 0.7) +
   geom_smooth(se = FALSE, colour = 'red', size = 1, linetype = "dashed") +
   theme_light() +
   scale_y_continuous(limits = c(0, 11.5e7),
                      labels = scales::dollar_format(scale = 1e-6, suffix = "m")) +
   labs(title    = 'Weekly Revenue - 2005 to June 2007',
        subtitle = 'Train, Test and Forecast Data Portions',
        x = "",
        y = 'Revenue ($m)') +
   
   # Train Portion
   annotate(x = ymd('2005-12-01'), y = (10.5e7), fill = 'black',
            'text',  label = 'Train\nPortion', size = 2.8) +
   
   # Test Portion
   annotate(x = ymd('2007-02-05'), y = (10.5e7),
            'text',  label = 'Test\nPortion', size = 2.8) +
   geom_rect(xmin = as.numeric(ymd('2006-12-18')),
             xmax = as.numeric(ymd('2007-03-25')),
             ymin = -Inf, ymax = Inf, alpha = 0.005,
             fill = 'darkturquoise') +
   
   # Forecast Portion
   annotate(x = ymd('2007-05-13'), y = (10.5e7),
            'text',  label = 'Forecast\nPortion', size = 2.8) +
   geom_rect(xmin = as.numeric(ymd('2007-03-26')),
             xmax = as.numeric(ymd('2007-07-01')),
             ymin = -Inf, ymax = Inf, alpha = 0.01,
             fill = 'cornflowerblue')

### Feature creation 

# Trend (a _trend_ and _trend squared_ )
# Lags (a _lag_13_ and _lag_52_)
# Seasonal - to deal with the _2-week-on, 2-week-off_ purchase frequency

model_data_tbl <- 
   revenue_tbl %>% 
   mutate(trend       = 1:nrow(revenue_tbl),
          trend_sqr   = trend^2,
          rev_lag_13  = lag(revenue, n = 13),
          rev_lag_52  = lag(revenue, n = 52),
          season      = case_when(revenue == 0 ~ 0,
                                  TRUE ~ 1)
   ) %>% filter(!is.na(rev_lag_52))


# train, test andforecast data frames
train_tbl <- 
   model_data_tbl %>% 
   filter(order_date <= "2007-03-19") 

test_tbl <- 
   model_data_tbl %>%
   filter(order_date >= "2006-10-02" &
             order_date <= "2007-03-19") 

forecast_tbl <- 
   model_data_tbl %>% 
   filter(order_date > "2007-03-19") %>%
   select(order_date:trend_sqr) %>% 
   cbind(season     = model_data_tbl %>%
            filter(between(order_date,
                           as.Date("2006-03-27"),
                           as.Date("2006-06-19"))) %>% select(season),
         rev_lag_52 = model_data_tbl %>%
            filter(between(order_date,
                           as.Date("2006-03-27"),
                           as.Date("2006-06-19"))) %>% select(rev_lag_52),
         rev_lag_13 = model_data_tbl %>%
            filter(between(order_date,
                           as.Date("2006-12-25"),
                           as.Date("2007-03-19"))) %>% select(rev_lag_13)
   )



### Modelling with H2O

# start a `H2O` instance and specify size of memory allocation pool cluster
h2o.init(max_mem_size = "16G")
# response variable
y <- "revenue"
# predictors set: remove response variable and order_date from the set
x <- setdiff(names(train_tbl %>% as.h2o()), c(y, "order_date"))
```

### A random forest
rft_model <- 
   h2o.randomForest(
      x = x, 
      y = y, 
      training_frame = train_tbl %>% as.h2o(),
      nfolds = 10,
      ntrees = 500,
      stopping_metric = "RMSE",
      stopping_rounds = 10,
      stopping_tolerance = 0.005,
      seed = 1975
   )

# visualise variable importance 
rft_model %>% h2o.varimp_plot()

# model_summary for information about models parameters. 
rft_model@model$model_summary

# model's performance 
h2o.performance(rft_model, newdata = test_tbl %>% as.h2o())


### Extend to many models
# fit a few more models and enable `cross-validation ` 

# gradient boosting machine model
gbm_model <-  
   h2o.gbm(
      x = x, 
      y = y, 
      training_frame = as.h2o(train_tbl),
      nfolds = 10,
      ntrees = 500,
      stopping_metric = "RMSE",
      stopping_rounds = 10,         
      stopping_tolerance = 0.005,
      seed = 1975
   )

# generalised linear model (a.k.a. elastic net model)
glm_model <- 
   h2o.glm(
      x = x, 
      y = y, 
      training_frame = as.h2o(train_tbl),
      nfolds = 10,
      family = "gaussian",
      seed = 1975
   )

# `automl` 
automl_model <-
    h2o.automl(
       x = x,
       y = y,
       training_frame     = as.h2o(train_tbl),
       nfolds             = 5,
       stopping_metric    = "RMSE",
       stopping_rounds    = 10,
       stopping_tolerance = 0.005,
       max_runtime_secs   = 60,
       seed               = 1975
    )

# Checking the leader board 
automl_model@leaderboard

# saviing the leader
aml_model <- automl_model@leader


## Performance assessment

# save models in one folder to access programmatically
 
# set path
path = "../02_models/final/"

# Save GLM model
h2o.saveModel(glm_model, path)
# Save RF model
h2o.saveModel(rft_model, path)
# Save GBM model
h2o.saveModel(gbm_model, path)
# Extracs and save leader autoML model
aml_model <- automl_model@leader
h2o.saveModel(aml_model, path)

### Variable importance plots with vip
p_glm <- vip(glm_model) + ggtitle("GLM")
p_rft <- vip(rft_model) + ggtitle("RF")
p_gbm <- vip(gbm_model) + ggtitle("GBM")
p_aml <- vip(aml_model) + ggtitle("AML")

grid.arrange(p_glm, p_rft, p_gbm, p_aml, nrow = 2)

### Performance metrics

# `h20.metric` function does not seem to work
perf_gbm_model %>% 
   h2o.metric()

## Error in paste0("No ", metric, " for ",
## class(object)) : argument "metric" is missing, with
##  no default

# other helper functions to extract the single metrics 
perf_gbm_model %>% h2o.r2()
perf_gbm_model %>% h2o.rmse()


# function to extract helper functions
performance_metrics_fct <- function(path, data_tbl) {
   
   model_h2o <- h2o.loadModel(path)
   perf_h2o  <- h2o.performance(model_h2o, newdata = as.h2o(data_tbl)) 
   
   R2   <- perf_h2o %>% h2o.r2()  
   RMSE <- perf_h2o %>% h2o.rmse()
   
   tibble(R2, RMSE)
}


# pass formula to `purrr::map` - also extract model's name from the path 
perf_metrics_test_tbl <- fs::dir_info(path = "../02_models/final/") %>%
   select(path) %>%
   mutate(metrics = map(path, performance_metrics_fct, data_tbl = test_tbl),
          path = str_split(path, pattern = "/", simplify = T)[,4] 
          %>% substr(1,3)) %>%
   rename(model = path) %>% 
   unnest(cols = c(metrics)) 

# visualise metrics to table
perf_metrics_test_tbl %>% 
   arrange(desc(R2)) %>% 
   knitr::kable()

### Actual vs Predicted plots

# function to calculate predictions for all models
predict_fct <- function(path, data_tbl) {
   
   model_h2o <- h2o.loadModel(path)
   pred_h2o  <- h2o.predict(model_h2o, newdata = as.h2o(data_tbl)) 
   
   pred_h2o %>% 
      as_tibble() %>% 
      cbind(data_tbl %>% select(order_date))
   
}


# pass formula to `purrr::map`compile `prediction` using the `test` data subset 
validation_tmp <- fs::dir_info(path = "../02_models/final/") %>%
   select(path) %>%
   mutate(pred = map(path, predict_fct, data_tbl = test_tbl),
          path = str_split(path, pattern = "/", simplify = T)[,4] %>% 
             substr(1,3)) %>%
   rename(model = path) 


# un-nest validation_tmp tibble, "pivot" predictions `order_date` 
# and add revenue as `actual`
validation_tbl <- 
   validation_tmp %>% 
   unnest(cols = c(pred)) %>% 
   pivot_wider(names_from = model, 
               values_from = predict) %>%
   cbind(test_tbl %>% 
            select(actual = revenue)) %>% 
   rename(date = order_date)

# actual bs predictef in plotly
validation_tbl %>% 
   plot_ly() %>% 
   add_lines(x = ~ date, y = ~ actual, name = 'Actual') %>% 
   add_lines(x = ~ date, y = ~ DRF, name = 'Random Forest', 
             line = list(dash = 'dot')) %>% 
   add_lines(x = ~ date, y = ~ GBM, name = 'Gradient Boosting Machine', 
             line = list(dash = 'dash')) %>% 
   add_lines(x = ~ date, y = ~ AML, name = 'Auto ML', 
             line = list(dash = 'dot')) %>% 
   add_lines(x = ~ date, y = ~ GLM, name = 'Generalised Linear Model', 
             line = list(dash = 'dash')) %>% 
   layout(title = 'Total Weekly Sales - Actual versus Predicted (various models)',
          yaxis = list(title = 'Millions of Dollars'),
          xaxis = list(title = ''),
          legend = list(orientation = 'h')
   )


## Forecasting

# re-use `performance_metrics_fct` for forecasting metrics

First, I take a look at the performance metrics

```{r}
perf_metrics_cast_tbl <- fs::dir_info(path = "../02_models/final/") %>%
   select(path) %>%
   mutate(metrics = map(path, performance_metrics_fct, data_tbl = forecast_tbl),
          path = str_split(path, pattern = "/", simplify = T)[,4] 
          %>% substr(1,3)) %>%
   rename(model = path) %>% 
   unnest(cols = c(metrics)) 

# visualise metrics to table
perf_metrics_cast_tbl %>% 
   arrange(desc(R2)) %>% 
   kable()

# re-use `predict_fct` for weekly forecasting
cast_tbl <- fs::dir_info(path = "../02_models/final/") %>%
   select(path) %>%
   mutate(pred = map(path, predict_fct, data_tbl = forecast_tbl),
          path = str_split(path, pattern = "/", simplify = T)[,4] %>% 
             substr(1,3)) %>%
   rename(model = path) %>% 
   unnest(cols = c(pred)) %>% 
   pivot_wider(names_from = model, values_from = predict) %>%
   cbind(forecast_tbl %>% select(actual = revenue)) %>% 
   rename(date = order_date)

# and visualise it with plotly
cast_tbl %>% 
   plot_ly() %>% 
   add_lines(x = ~ date, y = ~ actual, name = 'Actual') %>% 
   add_lines(x = ~ date, y = ~ DRF, name = 'Random Forest', 
             line = list(dash = 'dot')) %>% 
   add_lines(x = ~ date, y = ~ GBM, name = 'Gradient Boosting Machine', 
             line = list(dash = 'dash')) %>% 
   add_lines(x = ~ date, y = ~ AML, name = 'Auto ML', 
             line = list(dash = 'dot')) %>% 
   add_lines(x = ~ date, y = ~ GLM, name = 'Generalised Linear Model', 
             line = list(dash = 'dash')) %>% 
   layout(title = 'Total Weekly Sales - Actual versus Forecast (various models)',
          yaxis = list(title = 'Millions of Dollars'),
          xaxis = list(title = ''),
          legend = list(orientation = 'h')
   )


