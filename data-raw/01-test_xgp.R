
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

# x_trn <- df_trn %>% select(-any_of(c(col_y, col_id)))
# y_trn <- df_trn %>% select(all_of(col_y))
# id_trn <- df_trn %>% select(all_of(col_id))
n_row <- 10

set.seed(42)
suffix_quick <- 'xgp_quick'
grid_params <- df_trn %>% select(-any_of(c(col_y, col_id))) %>% generate_grid_params(10)
do_fit_partially <- partial(
  do_fit,
  objective = 'reg:squarederror',
  eval_metrics = list('rmse'),
  col_y = col_y,
  col_id = col_id,
  ... =
)

do_fit_quickly <- partial(
  do_fit_partially,
  overwrite = TRUE,
  grid_params = grid_params %>% slice(1),
  nrounds = 10,
  suffix = suffix_quick,
  ... =
)
do_fit_timely <- time_it(do_fit_quickly)

 c(tune, fit) %<-%
  do_fit_quickly(
    data = df_trn %>% head(n_row)
  )
fit

suffix_robust <- 'xgp_robust'
do_fit_robustly <- partial(
  do_fit_partially,
  overwrite = TRUE,
  grid_params = grid_params,
  nrounds = 1000,
  suffix = suffix_robust,
  ... =
)
do_fit_timely <- time_it(do_fit_robustly)

c(tune, fit) %<-%
  do_fit_timely(
    data = df_trn
  )
fit

tune %>%
  pivot_longer(
    eta:min_child_weight
  ) %>%
  ggplot() +
  aes(x = value, y = rmse_tst) +
  geom_point(aes(color = name, group = idx)) +
  facet_wrap(~name, scales = 'free')

do_predict_partially <- partial(
  do_predict,
  fit = fit,
  use_y = TRUE,
  overwrite = TRUE,
  col_y = col_y,
  col_id = col_id,
  ... =
)
do_predict_timely <- time_it(do_predict_partially)

c(preds_trn, shap_trn) %<-%
  do_predict_timely(
    data = df_trn,
    suffix = 'xgp_robust_trn'
  )
preds_trn

c(preds_tst, shap_tst) %<-%
  do_predict_timely(
    data = df_tst,
    suffix = 'xgp_robust_tst'
  )
preds_tst

met_set <- yardstick::metric_set(yardstick::rmse, yardstick::rsq, yardstick::mape)
do_eval <- function(preds) {
  preds %>%
    met_set(!!sym(col_y), .pred)
}

preds_trn %>% do_eval()
preds_tst %>% do_eval()

do_plot_shap(
  shap = shap_trn,
  overwrite = TRUE,
  suffix = 'xgp_robust_trn',
  col_y = col_y,
  col_id = col_id
)
