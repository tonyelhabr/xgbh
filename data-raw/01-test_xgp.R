
library(tidyverse)
library(xgbh)

nms <- c('xgp_fav_trn', 'xgp_fav_tst')
data(list = nms)
df_trn <- xgp_fav_trn
df_tst <- xgp_fav_tst
rm(list = nms)
rm('nms')

col_y <- 'favorite_count_trans'
col_id <- 'idx'

x_trn <- df_trn %>% select(-any_of(c(col_y, col_id)))
y_trn <- df_trn %>% select(all_of(col_y))
id_trn <- df_trn %>% select(all_of(col_id))
n_row <- 10

set.seed(42)
do_fit_partially <- partial(
  xgbh::do_fit,
  objective = 'reg:squarederror',
  eval_metrics = list('rmse'),
  col_y = col_y,
  col_id = col_id,
  ... =
)

do_fit_quickly <- partial(
  do_fit_partially,
  overwrite = FALSE,
  grid_params = x_trn %>% generate_grid_params() %>% slice(1),
  nrounds = 10,
  suffix = 'xgp',
  ... =
)
do_fit_timely <- time_it(do_fit_quickly)

c(tune, fit) %<-%
  do_fit_timely(
    data = df_trn %>% head(n_row)
  )
fit

do_predict_partially <- partial(
  do_predict,
  fit = fit,
  use_y = TRUE,
  overwrite = FALSE,
  # objective = 'binary:logistic',
  col_y = col_y,
  col_id = col_id,
  ... =
)
do_predict_timely <- time_it(do_predict_partially)

c(preds_trn, shap_trn) %<-%
  do_predict_timely(
    data = df_trn,
    suffix = 'xgp_trn'
  )
preds_trn

c(preds_tst, shap_tst) %<-%
  do_predict_timely(
    data = df_tst,
    suffix = 'xgp_tst'
  )
preds_tst

met_set <- yardstick::metric_set(yardstick::rmse, yardstick::rsq, yardstick::mape)
do_eval <- function(preds) {
  preds %>%
    met_set(!!sym(col_y), .pred)
}

preds_trn %>% do_eval()
preds_tst %>% do_eval()
