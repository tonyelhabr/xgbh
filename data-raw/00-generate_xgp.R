
library(xengagement)
library(dplyr)
token <- xengagement::get_twitter_token()
# options(xengagement.dir_data = 'data-raw')

train <- TRUE
method <- ifelse(train, 'all', 'since')

valid_stems <- xengagement::get_valid_stems()
cols_lst <- xengagement::get_cols_lst(valid_stems[1])
tweets <- xengagement::retrieve_tweets(method = method, token = token, export = FALSE)
xgp <- tweets %>% xengagement::transform_tweets(train = train)
xgp

col_y <- 'favorite_count_trans'
col_id <- 'idx'
xgp_fav <- xgp %>% dplyr::select(dplyr::one_of(c(col_y, col_id, cols_lst$cols_x)))

set.seed(42)
split <- xgp_fav %>% rsample::initial_split(strata = all_of(col_y))
xgp_fav_trn <- split %>% rsample::training()
xgp_fav_tst <- split %>% rsample::testing()

use_data(xgp, xgp_fav_trn, xgp_fav_tst, overwrite = TRUE)
