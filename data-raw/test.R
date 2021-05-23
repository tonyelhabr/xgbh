
library(xgbh)
library(xengagement)

token <- get_twitter_token()

dir_data <- xgbh::get_dir_data()
fs::dir_create(dir_data)
options(xengagement.dir_data = dir_data)

train <- TRUE
method <- ifelse(train, 'all', 'since')

valid_stems <- get_valid_stems()
cols_lst <- get_cols_lst(valid_stems[1])
tweets <- retrieve_tweets(method = method, token = token, export = FALSE)
data <- tweets %>% transform_tweets(train = train)
col_y <- 'favorite_count'
# col_y_sym <- col_y %>% sym()
col_id <- 'idx'
x <- data %>% dplyr::select(dplyr::one_of(cols_lst$cols_x))
y <- data %>% dplyr::select(dplyr::one_of(cols_lst$col_y))
id <- data[[col_id]]
strata <- data[[cols_lst$col_strata]]
wt <- data[[cols_lst$col_wt]]


grid_params <- x %>% generate_grid_params()
grid_params

res_fit <-
  xgbh::do_fit(
    overwrite = TRUE,
    grid_params = grid_params %>% dplyr::slice(1),
    x = x,
    y = y,
    id = id,
    strata = strata,
    wt = wt
  )
res_fit

res_fit <-
  xgbh::do_fit(
    overwrite = TRUE,
    grid_params = grid_params %>% dplyr::slice(1),
    x = x,
    y = y,
    id = id,
    strata = strata,
    wt = wt
  )
res_fit

res_fit <-
  xgbh::do_fit(
    overwrite = TRUE,
    grid_params = grid_params %>% dplyr::slice(1),
    data = data %>% dplyr::select(dplyr::one_of(c(col_id, cols_lst$col_y, cols_lst$col_wt, cols_lst$cols_x))),
    col_y = cols_lst$col_y,
    col_id = col_id,
    col_wt = cols_lst$col_wt
  )
res_fit

res_preds <-
  xgbh::do_predict(
    overwrite = TRUE,
    f_trans = xgbh::inverse_log,
    fit = res_fit$fit,
    x = x,
    y = y,
    id = id
  )
res_preds

res_preds <-
  xgbh::do_predict(
    overwrite = TRUE,
    f_trans = xgbh::inverse_log,
    fit = res_fit$fit,
    data = data,
    col_y = col_y,
    col_id = col_id
  )
res_preds

fit <- res_fit$fit
fit %>%
  xengagement::do_predict(
    data = data,
    stem = ..1,
    fit = ..2,
    .overwrite = list(preds = TRUE, shap = TRUE)
  )
