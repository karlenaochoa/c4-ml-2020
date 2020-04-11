## ----setup, include = FALSE--------------------------------------------------------------
knitr::opts_chunk$set(fig.width = 13, 
                      message = FALSE, 
                      warning = FALSE,
                      echo = TRUE)

library(tidyverse)

update_geom_defaults('path', list(size = 3, color = "cornflowerblue"))
update_geom_defaults('point', list(size = 5, color = "gray60"))
theme_set(theme_minimal(base_size = 25))


## ----forecast, echo = FALSE--------------------------------------------------------------
library(forecast)
USAccDeaths %>%
  tbats() %>%
  forecast() %>%
  autoplot() + 
  ggtitle("")


## ----train-test--------------------------------------------------------------------------
library(tidyverse)
set.seed(8675309)
train <- mtcars %>%
  sample_frac(.8)

test <- anti_join(mtcars, train)
nrow(train)
nrow(test)


## ----train-mods--------------------------------------------------------------------------
m1 <- lm(mpg ~ hp, train)
m2 <- lm(mpg ~ hp + disp, train)
m3 <- lm(mpg ~ hp + disp + cyl, train)
sundry::aic_weights(m1, m2, m3)


## ----test_preds--------------------------------------------------------------------------
test <- test %>%
  mutate(pred_mpg = predict(m2, newdata = test))

test


## ----diff--------------------------------------------------------------------------------
test %>%
  mutate(diff = pred_mpg - mpg)


## ----mse---------------------------------------------------------------------------------
test %>%
  summarize(mse = mean((pred_mpg - mpg)^2))


## ----rmse--------------------------------------------------------------------------------
test %>%
  summarize(rmse = sqrt(mean((pred_mpg - mpg)^2)))

