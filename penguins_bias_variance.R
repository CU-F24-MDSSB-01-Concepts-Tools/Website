library(tidyverse)
library(tidymodels)
library(palmerpenguins)
library(rpart.plot)

penguins_recipe <- recipe(body_mass_g ~ flipper_length_mm + species, data = penguins) |> 
 step_naomit(all_predictors()) 

penguins_tree <- decision_tree(cost_complexity = 0.01, min_n = 10) |>
 set_engine("rpart") |>
 set_mode("regression")

penguins_workflow <- workflow() |>
 add_recipe(penguins_recipe) |>
 add_model(penguins_tree)

set.seed(123)
penguins_split <- initial_split(penguins, prop = 0.7, strata = sex)
peng_train <- training(penguins_split)
peng_test <- testing(penguins_split)

peng_fit <- fit(penguins_workflow, data = peng_train)
peng_fit

rpart.rules(peng_fit$fit$fit$fit, style = "tall", roundint=FALSE)
rpart.plot(peng_fit$fit$fit$fit)

peng_pred <- predict(object = peng_fit, new_data = peng_train) |> 
 bind_cols(peng_train)
peng_pred |> rmse(truth = body_mass_g, estimate = .pred)
peng_pred |> rsq(truth = body_mass_g, estimate = .pred)

peng_pred <- predict(object = peng_fit, new_data = peng_test) |> 
 bind_cols(peng_test)
peng_pred |> rmse(truth = body_mass_g, estimate = .pred)
peng_pred |> rsq(truth = body_mass_g, estimate = .pred)

peng_sim <- expand_grid(flipper_length_mm = seq(170, 240, by = 1), 
                        species = unique(penguins$species))
peng_pred = predict(object = peng_fit, new_data = peng_sim) |> 
 bind_cols(peng_sim)

peng_pred |> ggplot(aes(x = flipper_length_mm, y = .pred, color = species)) + geom_line()



# Theoretical Bias Variance Tradoff example

## 1. Define "Real" Model
real_f <- function(x) x^2
ggplot() + 
 geom_function(fun = realmodel_f, color = "black")

## Sample from the model
sample_f <- function(n, irreducible_err_sd = 0.1) {
 tibble(x = runif(n, 0, 1), 
        y = real_f(x) + rnorm(n, mean = 0, sd = irreducible_err_sd))
}
sample_f(50) |> ggplot(aes(x = x, y = y)) + geom_point() + geom_function(fun = real_f, color = "blue")

s <- sample_f(1000)
linear_fit_sample <- function() lm(y ~ poly(x,2), data = s)
lm(y ~ poly(x,2), data = s)
lm(y ~ x.^2 + x + 1, data = s)
linear_fit_sample()
s |> mutate(.pred = predict(linear_fit_sample(), newdata = s)) |> 
 ggplot(aes(x = x, y = y)) + geom_point() + 
 geom_function(fun = real_f, color = "blue") + 
 geom_point(aes(y = .pred), color = "red")


linear_fit_sample()


