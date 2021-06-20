
# Loading libraries -------------------------------------------------------

library(tidymodels)
library(tidyverse)
library(textrecipes)
library(here)
tidymodels_prefer()
source("step_isofor.R")

splits <- initial_split(mtcars)
train <- training(splits)
test <- testing(splits)
resamples <- bootstraps(train, times = 5)

rec_obj <- 
  recipe(mpg ~ ., data = mtcars) %>%
  step_dummy(all_nominal_predictors()) %>%
  step_isofor(all_predictors(), sample_size = tune(), max_depth = tune())

lm_mod <- linear_reg() %>%
  set_engine("lm") 

wf_linear <- workflow() %>%
  add_recipe(rec_obj) %>%
  add_model(lm_mod)

iso_param <- wf_linear %>%
  parameters() %>%
  update(sample_size = sample_size(c(1, 24)))

tuned_mod <- wf_linear %>%
  tune_grid(resamples = resamples, param_info = iso_param)

select_best(tuned_mod, "rmse")