
library(tidyverse)
library(xgbh)
df <-
  file.path('..', 'sports_viz', '31-wp_soccer', 'model_data.parquet') %>%
  arrow::read_parquet() %>%
  mutate(
    across(target, ~.x + 1L),
    match_id = sprintf('%s_%s_%s_%s_%s', league, season, date, team_h, team_a),
    # Stoke city Man United 2017-09-09 has a 0 here for some reason
    across(max_minute, ~case_when(.x == 0L ~ 100L, TRUE ~ .x)),
    wt_decay = (1 - minute / (max_minute + 1))^2
  ) %>%
  mutate(
    across(matches('^(prob|imp)'), ~{.x * wt_decay}),
    # xgd_ratio = xgd / (exp(-pi * wt_decay))
    gd_wt = gd / wt_decay
  )
df
# df %>% skimr::skim()

match_ids <- df %>% distinct(match_id)
match_ids
n_match <- match_ids %>% nrow()
n_match_trn <- floor(n_match * 0.8)

match_ids_trn <- match_ids %>% slice(c(1:n_match_trn))
match_ids_tst <- match_ids %>% slice(c((n_match_trn + 1):n_match))
df_trn <- df %>% semi_join(match_ids_trn)
df_tst <- df %>% semi_join(match_ids_tst)

nms <- df %>% names()
col_y <- 'target'
col_id <- 'idx'
cols_x <-
  c(
    'minute',
    'is_h',
    'g',
    'g_opp',
    'xg',
    'xg_opp',
    'sub',
    'sub_opp',
    'cards_y',
    'cards_y_opp',
    'cards_r',
    'cards_r_opp',
    'send_off',
    'send_off_opp',
    'has_attendance',
    'prob_d',
    'prob',
    'prob_opp',
    'imp',
    'imp_opp',
    'gd_wt'
  )
cols_extra <- nms %>% setdiff(c(col_y, col_id, cols_x))
cols_extra

set.seed(42)
folds <-
  xgbh::create_folds(
    y = df_trn %>% pull(match_id),
    k = 10,
    type = 'grouped',
    invert = TRUE
  )
folds

sets.seed(42)
grid_params <- df_trn %>% select(-any_of(c(col_y, col_id, cols_extra))) %>% generate_grid_params(n_param = 20)
# c("minute", "is_h", "g", "g_opp", "xg", "xg_opp", "sub", "sub_opp", "cards_y", "cards_y_opp", "cards_r", "cards_r_opp", "send_off", "send_off_opp", "has_attendance", "prob_d", "prob", "prob_opp", "imp", "imp_opp", "gd_wt")
vec_constr <- '(1, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, -1, -1, -1, -1, -1, 1)'
# vec_constr %>% str_count(',')
# cols_x %>% length()

suffix <- 'wp_soccer_robust'
do_fit_timely <- time_it(do_fit)
c(tune, fit) %<-%
  do_fit_timely(
    data = df_trn,
    overwrite = FALSE,
    suffix = suffix,
    objective = 'multi:softprob',
    eval_metrics = list('mlogloss'),
    col_y = col_y,
    col_id = col_id,
    cols_extra = cols_extra,
    fold_ids = folds,
    grid_params = grid_params,
    num_class = 3L,
    # .params_tune = list(monotone_constraints = vec_constr),
    # .params = list(monotone_constraints = vec_constr),
    nrounds = 10000
  )
fit

# quick ----
suffix <- 'wp_soccer_quick'
fit <-
  xgbh::fit(
    data = df_trn,
    overwrite = TRUE,
    suffix = suffix,
    objective = 'multi:softprob',
    eval_metrics = list('mlogloss'),
    col_y = col_y,
    col_id = col_id,
    cols_extra = cols_extra,
    .params = list(monotone_constraints = vec_constr),
    nrounds = 1000,
    num_class = 3L
  )

c(preds_trn, shap_trn) %<-%
  xgbh::do_predict(
    data = df_trn,
    fit = fit,
    overwrite = TRUE,
    use_y = TRUE,
    # .do = list(shap = FALSE),
    suffix = suffix,
    col_y = col_y,
    col_id = col_id,
    cols_extra = cols_extra
  )
preds_trn

c(viz_shap_agg_trn, viz_shap_swarm_trn) %<-%
  do_plot_shap(
    shap = shap_trn,
    suffix = suffix,
    col_y = col_y,
    col_id = col_id,
    cols_extra = cols_extra,
    .do = list(swarm = TRUE)
  )
viz_shap_agg_trn
viz_shap_swarm_trn

match_ids
preds_trn %>%
  filter(season == 2018, team_h == 'Liverpool', team_a == 'Wolves') %>%
  select(idx, is_h, minute, g, g_opp) %>%
  pivot_longer(
    matches('^g')
  ) %>%
  ggplot() +
  aes(x = minute, y = value, color = name, group = name) +
  geom_line() +
  facet_wrap(~is_h, scales = 'free')

preds_trn %>%
  filter(season == 2018, team_h == 'Liverpool', team_a == 'Wolves') %>%
  select(idx, is_h, minute, matches('[.]prob')) %>%
  pivot_longer(
    matches('prob')
  ) %>%
  ggplot() +
  aes(x = minute, y = value, color = name, group = name) +
  geom_line() +
  facet_wrap(~is_h, scales = 'free')
preds_trn

df_tst_dummy <-
  crossing(
    is_h = c(0L, 1L),
    g = seq.int(0, 3),
    g_opp = seq.int(0, 3),
    xg = seq(0, 3, by = 0.25),
    xg_opp = seq(0, 3, by = 0.25)
  )
df_tst_dummy
