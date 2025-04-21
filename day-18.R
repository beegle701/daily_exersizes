# Nick Beegle
# ESS 330

library(tidyverse)
library(tidymodels)
library(readr)

set.seed(123)

url <- "https://raw.githubusercontent.com/mikejohnson51/csu-ess-330/refs/heads/main/resources/co-est2023-alldata.csv"
raw_data <- read_csv(url)

county_data <- raw_data %>%
  filter(SUMLEV == "050") %>% 
  select(STNAME, CTYNAME, POPESTIMATE2023, BIRTHS2023, DEATHS2023, NETMIG2023, RESIDUAL2023) %>%
  drop_na()

county_data <- county_data %>%
  mutate(
    birth_rate   = BIRTHS2023 / POPESTIMATE2023,
    net_mig_rate = NETMIG2023 / POPESTIMATE2023,
    residual_rate= RESIDUAL2023 / POPESTIMATE2023
  )

model_data <- county_data %>%
  select(DEATHS2023, POPESTIMATE2023, birth_rate, net_mig_rate, residual_rate) %>%
  rename(
    deaths = DEATHS2023,
    population = POPESTIMATE2023
  )

set.seed(123)
data_split <- initial_split(model_data, prop = 0.8)
train_data <- training(data_split)
test_data  <- testing(data_split)

death_recipe <- recipe(deaths ~ ., data = train_data) %>%
  step_normalize(all_predictors())

lm_model <- linear_reg() %>%
  set_engine("lm")

death_workflow <- workflow() %>%
  add_recipe(death_recipe) %>%
  add_model(lm_model)

death_fit <- death_workflow %>%
  fit(data = train_data)

predictions <- predict(death_fit, test_data) %>%
  bind_cols(test_data)

model_metrics <- metrics(predictions, truth = deaths, estimate = .pred)
print(model_metrics)

scatter_plot <- ggplot(predictions, aes(x = deaths, y = .pred)) +
  geom_point(color = "blue", size = 2, alpha = 0.8) +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed", linewidth = 1) +
  labs(
    title = "Scatter Plot: Actual vs. Predicted Deaths (2023)",
    x = "Actual Deaths",
    y = "Predicted Deaths"
  ) +
  theme_minimal()

print(scatter_plot)

ggsave("truth_vs_predicted_deaths.png", scatter_plot, width = 8, height = 6)