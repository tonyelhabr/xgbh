
library(tidyverse)
library(xgbh)
library(recipes)
library(yardstick)

nms <- c('pokemon_multi_trn', 'pokemon_multi_tst')
data(list = nms)
df_trn <- pokemon_multi_trn
df_tst <- pokemon_multi_tst
rm(list = nms)
rm('nms')

col_y <- 'type_1'
col_id <- 'number'

form <- paste0(col_y, '~ .') %>% as.formula()
rec_init <-
  df_trn %>%
  select(-c(name)) %>%
  recipe(form, data = .) %>%
  update_role(c(number), new_role = 'id') %>%
  step_rm(any_of(c('type_2', 'total'))) %>%
  step_dummy(generation) %>%
  step_nzv(all_numeric_predictors())
rec_init

# checking ----
jui_trn <- rec_init %>% prep() %>% juice()
jui_trn

.to_xgb <- function(df) {
  jui <- rec_init %>% prep() %>% bake(new_data = df)
  jui %>% mutate(across(all_of(col_y), ~as.integer(.x) - 1L))
}
jui_trn_xgb <- df_trn %>% .to_xgb()
jui_trn_xgb
jui_tst_xgb <- df_tst %>% .to_xgb()
jui_tst_xgb

set.seed(42)

load_all()
col_y_sym <- sym(col_y)
n_class <- jui_trn %>% distinct(!!col_y_sym) %>% pull(!!col_y_sym) %>% length()
do_fit_partially <- partial(
  xgbh::do_fit,
  overwrite = FALSE,
  objective = 'multi:softprob',
  eval_metrics = list('mlogloss'),
  # eval_metrics = list('error'),
  num_class = n_class,
  col_y = col_y,
  col_id = col_id,
  ... =
)

do_fit_robustly <- partial(
  do_fit_partially,
  grid_params = jui_trn_xgb %>% select(-any_of(c(col_y, col_id))) %>% generate_grid_params(n_param = 50),
  # n_param = 50,
  nrounds = 1000,
  suffix = 'pokemon_robust',
  ... =
)
do_fit_robustly_timely <- time_it(do_fit_robustly)

c(tune, fit) %<-%
  do_fit_robustly_timely(
    data = jui_trn_xgb
  )
fit

load_all(); do_predict_partially <- partial(
  do_predict,
  overwrite = FALSE,
  fit = fit,
  use_y = TRUE,
  col_y = col_y,
  col_id = col_id,
  ... =
)
do_predict_timely <- time_it(do_predict_partially)

c(probs_trn, shap_trn) %<-%
  do_predict_timely(
    data = jui_trn_xgb,
    suffix = 'pokemon_trn'
  )
probs_trn

c(probs_tst, shap_tst) %<-%
  do_predict_timely(
    data = jui_tst_xgb,
    suffix = 'pokemon_tst'
  )
probs_tst

met_set <- yardstick::metric_set(yardstick::mn_log_loss, yardstick::roc_auc)
do_eval <- function(probs) {
  probs %>%
    mutate(across(all_of(col_y), factor)) %>%
    met_set(!!sym(col_y), matches('^[.]prob'))
}

probs_trn %>% do_eval()
probs_tst %>% do_eval()
