
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
tweets <- retrieve_tweets(method = method, token = token)
data <- tweets %>% transform_tweets(train = train)
col_y <- 'favorite_count'
# col_y_sym <- col_y %>% sym()
col_id <- 'idx'
x <-
  tweets_transformed %>%
  dplyr::select(dplyr::one_of(cols_lst$cols_x))
x
y <-
  tweets_transformed %>%
  dplyr::select(dplyr::one_of(cols_lst$col_y))
y

res_fit <-
  xgbh::do_fit(
    overwrite = TRUE,
    x = x,
    y = y,
    id = data[[col_id]],
    strata = data[[cols_lst$col_strata]],
    wt = data[[cols_lst$col_wt]]
  )
res_fit

res_fit <-
  xgbh::do_fit(
    overwrite = TRUE,
    data = data %>% dplyr::select(dplyr::one_of(c(col_id, cols_lst$col_y, cols_lst$col_wt, cols_lst$cols_x))),
    col_y = cols_lst$col_y,
    col_id = col_id,
    col_wt = cols_lst$col_wt
  )
res_fit

fit <- res_fit$fit
fit %>%
  xengagement::do_predict(
    tweets_transformed = tweets_transformed,
    stem = ..1,
    fit = ..2,
    .overwrite = list(preds = TRUE, shap = TRUE)
  )
