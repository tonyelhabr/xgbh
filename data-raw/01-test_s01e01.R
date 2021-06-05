
# shell(
#   'kaggle competitions download -c sliced-s01e01'
# )

library(tidyverse)
library(xgbh)
library(tonythemes)
theme_set_tony()

f_read <- function(x) {
  file.path('data-raw', 'sliced-s01e01', sprintf('%s.csv', x)) %>%
    read_csv(
      col_types = cols(
        .default = col_character(),
        game_id = col_double(),
        min_players = col_double(),
        max_players = col_double(),
        avg_time = col_double(),
        min_time = col_double(),
        max_time = col_double(),
        year = col_double(),
        # geek_rating = col_double(),
        num_votes = col_double(),
        age = col_double(),
        owned = col_double()
      )
    )
}

df_trn_init <- f_read('train') %>% mutate(across(geek_rating, as.double))
df_tst_init <- f_read('test')
df_trn_init %>% skimr::skim()
df_trn_init %>%
  count(category1, sort = TRUE) %>%
  mutate(frac = n / sum(n)) %>%
  mutate(rnk = row_number(desc(frac))) %>%
  ggplot() +
  aes(x = rnk, y = frac) +
  geom_col()
nms_numeric <- df_trn_init %>% select(where(is.numeric)) %>% names()
f_select <- function(data) { data %>% select(one_of(c(nms_numeric, 'category1', 'mechanic'))) }
df_trn <- df_trn_init %>% f_select()
df_tst <- df_tst_init %>% f_select()
df_trn %>% skimr::skim()
df_trn

col_y <- 'geek_rating'
col_id <- 'game_id'
suffix <- 's01e01_w_text'

library(tidymodels)
library(textrecipes)
form <- formula(sprintf('%s ~ .', col_y))

df_trn %>% count(category)
rec <-
  df_trn %>%
  recipe(form, .) %>%
  step_other(category1, threshold = 0.04) %>%
  step_dummy(category1) %>%
  step_tokenize(mechanic, custom_token = function(x) strsplit(x, split = ', ')) %>%
  step_tokenfilter(mechanic, min_times = 10) %>%
  step_tf(mechanic) %>%
  step_pca(matches('^(category|tf_mechanic)'), num_comp = 10)
rec
jui_trn <- rec %>% prep() %>% juice()
jui_trn
jui_tst <- rec %>% prep() %>% bake(df_tst)
jui_tst

x_trn <- df_trn %>% select(-any_of(c(col_y, col_id)))

set.seed(42)
params <- x_trn %>% generate_grid_params(20)

do_fit_timely <- time_it(xgbh::do_fit)

c(tune, fit) %<-%
  do_fit_timely(
    data = jui_trn,
    overwrite = TRUE,
    grid_params = params, #  %>% slice(1),
    nrounds = 2000,
    suffix = suffix,
    objective = 'reg:squarederror',
    eval_metrics = list('rmse'),
    col_y = col_y,
    col_id = col_id
  )
fit

do_predict_timely <- time_it(xgbh::do_predict);
c(preds_trn, shap_trn) %<-%
  do_predict_timely(
    data = jui_trn,
    suffix = suffix,
    fit = fit,
    use_y = TRUE,
    overwrite = TRUE,
    col_y = col_y,
    col_id = col_id
  )
preds_trn

c(preds_tst, shap_tst) %<-%
  do_predict_timely(
    data = jui_tst,
    suffix = suffix,
    fit = fit,
    use_y = FALSE,
    overwrite = TRUE,
    col_y = col_y,
    col_id = col_id
  )
preds_tst

preds_trn %>%
  select(.pred, geek_rating) %>%
  yardstick::rmse(.pred, geek_rating)

preds_tst_export <-
  preds_tst %>%
  select(game_id, geek_rating = .pred)
path_export <- here::here('inst/extdata/preds_s01e01_upload_v3.csv')
preds_tst_export %>% write_csv(path_export)

shell(
  glue::glue('kaggle competitions submit -f {path_export} -m "tony trying hard 3" sliced-s01e01')
)

shell(
  'kaggle competitions leaderboard sliced-s01e01 -s'
)



c(preds_tst, shap_tst) %<-%
  do_predict_timely(
    data = df_tst,
    suffix = suffix
  )

df_trn %>%
  mutate(idx = row_number()) %>%
  ggplot() +
  aes(x = idx, y = geek_rating) +
  geom_point()




