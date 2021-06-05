
library(tidyverse)
library(xgbh)
library(yardstick)

nms <- c('speed_dating', 'speed_dating_holdout')
data(list = nms)
df_trn <- speed_dating
df_tst <- speed_dating_holdout
rm(list = nms)
rm('nms')

col_y <- 'match'
col_id <- 'idx'

x_trn <- df_trn %>% select(-any_of(c(col_y, col_id)))
y_trn <- df_trn %>% select(all_of(col_y))
id_trn <- df_trn %>% select(all_of(col_id))
n_row <- 10
set.seed(42)

do_fit_partially <- partial(
  xgbh::do_fit,
  overwrite = FALSE,
  objective = 'binary:logistic',
  eval_metrics = list('logloss'),
  # eval_metrics = list('error'),
  col_y = col_y,
  col_id = col_id,
  ... =
)

do_fit_quickly <- partial(
  do_fit_partially,
  grid_params = x %>% generate_grid_params() %>% slice(1),
  nrounds = 10,
  suffix = 'speed_dating_quick',
  ... =
)
do_fit_quickly_timely <- time_it(do_fit_quickly)

c(tune, fit) %<-%
  do_fit_quickly_timely(
    data = df_trn %>% head(n_row)
  )
fit

idx <- 1:n_row
c(tune, fit) %<-%
  do_fit_quickly_timely(
    x = x_trn[idx, ],
    y = y_trn[idx, ],
    id = id_trn[idx, ]
  )
fit

do_fit_robustly <- partial(
  do_fit_partially,
  grid_params = x_trn %>% generate_grid_params(n_param = 30),
  nrounds = 2000,
  suffix = 'speed_dating_robust',
  ... =
)
do_fit_robustly_timely <- time_it(do_fit_robustly)

c(tune, fit) %<-%
  do_fit_robustly_timely(
    data = df_trn
  )
fit

do_predict_partially <- partial(
  do_predict,
  overwrite = FALSE,
  fit = fit,
  # objective = 'binary:logistic',
  col_y = col_y,
  col_id = col_id,
  ... =
)
do_predict_timely <- time_it(do_predict_partially)

c(probs_trn, shap_trn) %<-%
  do_predict_timely(
    data = df_trn,
    use_y = TRUE,
    # col_y = col_y,
    suffix = 'speed_dating_trn'
  )
probs_trn

c(probs_tst, shap_tst) %<-%
  do_predict_timely(
    data = df_tst,
    use_y = FALSE,
    suffix = 'speed_dating_tst'
  )
assertthat::assert_that(!any(col_y == names(probs_tst)), msg = 'This data set shouldn\'t have a y column in the test set, but found it in the returned predicted probabilities.')

met_set <- yardstick::metric_set(yardstick::mn_log_loss, yardstick::roc_auc)
do_eval <- function(probs) {
  probs %>%
    mutate(across(all_of(col_y), factor)) %>%
    met_set(!!sym(col_y), .prob)
}

probs_trn %>% do_eval()
