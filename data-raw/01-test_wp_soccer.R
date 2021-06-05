
library(tidyverse)
library(xgbh)
df <- file.path('data-raw', 'model_data.rds') %>% read_rds()

match_ids <- df %>% distinct(match_id)
n_match <- match_ids %>% nrow()
n_match_trn <- floor(n_match * 0.8)

match_ids_trn <- match_ids %>% slice(c(1:n_match_trn))
match_ids_tst <- match_ids %>% slice(c((n_match_trn + 1):n_match))
df_trn <- df %>% semi_join(match_ids_trn)
df_tst <- df %>% semi_join(match_ids_tst)

col_y <- 'is_w'
col_id <- 'idx'
cols_extra <- c('league', 'season', 'match_id', 'minute')

x_trn <- df_trn %>% select(-any_of(c(col_y, col_id, cols_extra)))
y_trn <- df_trn %>% select(all_of(col_y))
id_trn <- df_trn %>% select(all_of(col_id))
n_row <- nrow(x_trn) # 30

set.seed(42)
folds <-
  xgbh::create_folds(
    y = df_trn %>% pull(match_id),
    k = 5,
    type = 'grouped',
    invert = TRUE
  )
folds

do_fit_partially <- partial(
  xgbh::do_fit,
  overwrite = FALSE,
  objective = 'binary:logistic',
  eval_metrics = list('logloss'),
  # eval_metrics = list('error'),
  col_y = col_y,
  col_id = col_id,
  cols_extra = cols_extra,
  fold_ids = folds,
  ... =
)

sets.seed(42)
grid_params <- x_trn %>% generate_grid_params(n_param = 40)
do_fit_quickly <- partial(
  do_fit_partially,
  grid_params = grid_params %>% slice(1),
  nrounds = 10,
  suffix = 'wp_soccer_quick',
  ... =
)
do_fit_quickly_timely <- time_it(do_fit_quickly)

c(tune, fit) %<-%
  do_fit_quickly_timely(
    data = df_trn %>% head(n_row)
  )
fit

do_fit_robustly <- partial(
  do_fit_partially,
  grid_params = grid_params,
  nrounds = 10000,
  suffix = 'wp_soccer_robust',
  ... =
)
do_fit_robustly_timely <- time_it(do_fit_robustly)

c(tune, fit) %<-%
  do_fit_robustly_timely(
    data = df_trn
  )
fit

