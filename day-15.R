#nick Beegle

library(tidymodels)
library(palmerpenguins)  
library(rsample)
library(tidyverse)
library(parsnip)
library(workflowsets)
library(workflows)

data("penguins")
penguins <- na.omit(penguins)


set.seed(123)

penguins_split <- initial_split(penguins, prop = 0.7)

penguins_train <- training(penguins_split)
penguins_test <- testing(penguins_split)

penguins_folds <- vfold_cv(penguins_train, v = 10)

penguins_folds

log_reg_model <- logistic_reg() %>%
  set_engine("glm") %>%
  set_mode("classification")

rand_forest_model <- rand_forest() %>%
  set_engine("ranger") %>%  
  set_mode("classification")


model_workflows <- workflow_set(
  preproc = list(formula = species ~ .),
  models = list(logistic_regression = log_reg_model, random_forest = rand_forest_model)
)


model_results <- model_workflows %>%
  workflow_map("fit_resamples", resamples = penguins_folds, metrics = metric_set(accuracy))


model_results

## Comment: So, I think logistic regression was the best in lecture, but random forests seem like they might work better for some problems? I'm not totally sure how to tell which one is best yet, but I guess accuracy will help us compare them!