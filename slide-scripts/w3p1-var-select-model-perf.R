## ----setup, include = FALSE--------------------------------------------------------------
knitr::opts_chunk$set(fig.width = 13, 
                      message = FALSE, 
                      warning = FALSE,
                      echo = TRUE)
library(tidyverse)
theme_set(theme_minimal(25))


## ----col-spec----------------------------------------------------------------------------
library(tidyverse)

col_specs <- cols(
  .default = col_character(),
  id = col_double(),
  attnd_dist_inst_id = col_double(),
  attnd_schl_inst_id = col_double(),
  enrl_grd = col_double(),
  calc_admn_cd = col_logical(),
  partic_dist_inst_id = col_double(),
  partic_schl_inst_id = col_double(),
  lang_cd = col_character(), #<<
  score = col_double(),
  classification = col_double(),
  ncessch = col_double(),
  lat = col_double(),
  lon = col_double()
)


## ----readr-fast, message = TRUE----------------------------------------------------------
read_csv(here::here("data", "edld-654-spring-2020", "train.csv"),
         n_max = 10)


## ----load-data---------------------------------------------------------------------------
d <- vroom::vroom(
  here::here("data", "edld-654-spring-2020", "train.csv"),
  col_types = col_specs
)


## ----initial-prep------------------------------------------------------------------------
set.seed(123)
d <- d %>% 
  select(-classification)  %>% 
  mutate(lang_cd = ifelse(is.na(lang_cd), "E", lang_cd),
         ayp_lep = ifelse(is.na(ayp_lep), "G", ayp_lep),
         tst_dt = lubridate::as_date(lubridate::mdy_hms(tst_dt))) %>% 
  # only use a sample
  group_by(lang_cd) %>% 
  sample_frac(.25) %>% # Could even go lower
  ungroup() 


## ----inspect-d---------------------------------------------------------------------------
d %>% 
  mutate_if(is.character, as.factor) %>% 
  map(summary)


## ----rm-constants------------------------------------------------------------------------
d <- d %>% 
  # remove any rows or columns that are fully missing
  janitor::remove_empty(c("rows", "cols")) %>% 
  
  # drop any row that has a missing value
  drop_na() %>% 
  
  # drop any column that doesn't have more than 1 unique value
  select_if(~length(unique(.x)) > 1)


## ----initial-split-----------------------------------------------------------------------
library(tidymodels)
splt <- initial_split(d, strata = lang_cd)
train <- training(splt)


## ----check-strat1------------------------------------------------------------------------
d %>% 
  count(lang_cd) %>% 
  mutate(prop = n/sum(n))


## ----check-strat2------------------------------------------------------------------------
train %>% 
  count(lang_cd) %>% 
  mutate(prop = n/sum(n))


## ----cv----------------------------------------------------------------------------------
cv <- vfold_cv(train, strata = lang_cd)


## ----print-cv----------------------------------------------------------------------------
cv


## ----prop1-------------------------------------------------------------------------------
cv$splits[[1]] %>% 
  assessment() %>% #<<
  nrow()

3482 / (3482 + 31336)


## ----prop2-------------------------------------------------------------------------------
cv$splits[[1]] %>% 
  analysis() %>% #<<
  nrow()

31336 / (3482 + 31336)


## ----strata-fold-check-------------------------------------------------------------------
cv %>% 
  mutate(assessment_props = map(
    splits, ~
      assessment(.x) %>% 
      count(lang_cd) %>% 
      mutate(prop = n/sum(n))
      )
    ) %>% 
  unnest(assessment_props)


## ----set_model---------------------------------------------------------------------------
mod <- linear_reg()


## ----print-model-------------------------------------------------------------------------
mod


## ----set_engine--------------------------------------------------------------------------
mod <- mod %>% 
  set_engine("lm")
mod


## ----set_mode----------------------------------------------------------------------------
mod <- mod %>% 
  set_mode("regression")
mod


## ----rec1--------------------------------------------------------------------------------
baseline_rec <- recipe(score ~ gndr + sp_ed_fg, train)


## ----print-rec1--------------------------------------------------------------------------
baseline_rec


## ----fit_resamples1----------------------------------------------------------------------
m1 <- mod %>% 
  fit_resamples(preprocessor = baseline_rec,
                resamples = cv)
m1



## ----eval-metrics1-----------------------------------------------------------------------
m1_metrics <- m1 %>% 
  select(id, .metrics) %>% 
  unnest(.metrics) %>% 
  pivot_wider(names_from = .metric, 
              values_from = .estimate)

m1_metrics


## ----interaction_rec---------------------------------------------------------------------
interaction_rec <- recipe(score ~ gndr + sp_ed_fg, train) %>% 
  step_dummy(all_predictors()) %>% 
  step_interact(terms = ~ starts_with("gndr"):starts_with("sp_ed_fg"))
interaction_rec


## ----fit-m2-resamples--------------------------------------------------------------------
m2 <- mod %>% 
  fit_resamples(interaction_rec, cv)
m2


## ----eval-metrics2-----------------------------------------------------------------------
m2_metrics <- m2 %>% 
  select(id, .metrics) %>% 
  unnest(.metrics) %>% 
  pivot_wider(names_from = .metric, 
              values_from = .estimate)

m2_metrics


## ----join-metrics1-----------------------------------------------------------------------
metrics_joined <- left_join(m1_metrics, m2_metrics, 
          by = c("id", ".estimator"),
          suffix = c("_m1", "_m2"))

metrics_joined


## ----inspect-interaction-----------------------------------------------------------------
metrics <- metrics_joined %>% 
  pivot_longer(cols = contains("_"),
               names_to = c("metric", "model"),
               values_to = "estimate",
               names_sep = "_")
metrics


## ----rmse-min1---------------------------------------------------------------------------
metrics %>% 
  filter(metric == "rmse") %>% 
  group_by(id) %>% 
  summarize(estimate = min(estimate)) 


## ----rmse-min2---------------------------------------------------------------------------
metrics %>% 
  filter(metric == "rmse") %>% 
  group_by(id) %>% 
  summarize(estimate = min(estimate)) %>% 
  semi_join(metrics, .) #<<


## ----rmse-min3---------------------------------------------------------------------------
metrics %>% 
  filter(metric == "rmse") %>% 
  group_by(id) %>% 
  summarize(estimate = min(estimate)) %>% 
  semi_join(metrics, .) %>% 
  count(model) #<<


## ----change-rmse-------------------------------------------------------------------------
metrics_joined %>% 
  mutate(rmse_diff = round(rmse_m1 - rmse_m2, 3),
         rsq_diff  = round(rsq_m1 - rsq_m2, 3)) %>% 
  select(id, ends_with("diff"))


## ----vi1---------------------------------------------------------------------------------
library(vip)
m <- lm(score ~ gndr + sp_ed_fg + gndr:sp_ed_fg, data = train)
vi(m)


## ----vip1--------------------------------------------------------------------------------
vip(m)


## ----vip2--------------------------------------------------------------------------------
vip_1a <- vip(lm(score ~ ., train))


## ----show-vip2, fig.height = 6.5---------------------------------------------------------
vip_1a


## ----vip2b1------------------------------------------------------------------------------
vip_1b <- vi(lm(score ~ ., train))
vip_1b


## ----vip2a, cache = TRUE, fig.height = 5.75----------------------------------------------
vip_2a <- vip(lm(score ~ .^2, train))
vip_2a


## ----fit_resamples3----------------------------------------------------------------------
post_vip_rec <- recipe(score ~ tag_ed_fg + sp_ed_fg + econ_dsvntg +
                               tst_dt + ethnic_cd, 
                       data = train)

m3 <- mod %>% 
  fit_resamples(post_vip_rec, cv)

m3


## ----m3-metrics--------------------------------------------------------------------------
m3 %>% 
  select(id, .metrics) %>% 
  unnest(.metrics) %>% 
  pivot_wider(names_from = .metric, 
              values_from = .estimate)


## ----prep-full-train---------------------------------------------------------------------
train_prepped <- post_vip_rec %>% 
  prep() %>% 
  juice()


## ----print-train-prepped-----------------------------------------------------------------
train_prepped


## ----fit-full-training-------------------------------------------------------------------
test_mod <- mod %>% 
  fit(score ~ ., 
      data = train_prepped) 


## ----test--------------------------------------------------------------------------------
test <- testing(splt)
test


## ----prep-test---------------------------------------------------------------------------
# Apply recipe to test dataset
test_prepped <- post_vip_rec %>% 
  prep() %>% 
  bake(test) #<<

test_prepped


## ----preds-------------------------------------------------------------------------------
preds <- test_prepped %>% 
  mutate(estimate = predict(test_mod$fit, newdata = .)) %>% 
  select(truth = score, estimate)
preds


## ----test-eval---------------------------------------------------------------------------
bind_rows(
  rmse(preds, truth, estimate),
  rsq(preds, truth, estimate)
)


## ----huber-loss, out.height = "400px", echo = FALSE--------------------------------------
knitr::include_graphics("https://upload.wikimedia.org/wikipedia/commons/thumb/c/cc/Huber_loss.svg/1920px-Huber_loss.svg.png")

