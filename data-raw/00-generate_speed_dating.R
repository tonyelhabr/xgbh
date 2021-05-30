
library(readr)
library(dplyr)
library(stringr)

f_import <- function(suffix) {
  file.path('data-raw', sprintf('sliced-s00e01-%s.csv', suffix)) %>%
    readr::read_csv(guess_max = 20000) %>%
    dplyr::select(-1) %>%
    dplyr::mutate(idx = dplyr::row_number())
}
speed_dating <- f_import('data')
speed_dating

speed_dating_holdout <- f_import('holdout')
speed_dating_holdout

col_y <- 'match'
col_id <- 'idx'
nms <- speed_dating %>% names()
cols_x <- nms %>% setdiff(col_y)

cols_x_paired <-
  cols_x %>%
  stringr::str_remove('_o$') %>%
  dplyr::tibble(col = .) %>%
  dplyr::count(col) %>%
  dplyr::filter(n > 1L) %>%
  dplyr::filter(col != 'dec')
cols_x_paired

f_select <- function(data) {
  res <-
    data %>%
    dplyr::select(
      dplyr::any_of(col_y),
      dplyr::all_of(col_id),
      dplyr::one_of(cols_x_paired %>% dplyr::pull(col)),
      dplyr::one_of(cols_x_paired %>% dplyr::pull(col) %>% paste0('_o'))
    )

  # for tidymodels
  # if(any('match' %in% colnames(res))) {
  #   res <-
  #     res %>%
  #     mutate(across(match, factor))
  # }
  res
}

speed_dating <- speed_dating %>% f_select()
speed_dating_holdout <- speed_dating_holdout %>% f_select()

use_data(speed_dating, speed_dating_holdout, overwrite = TRUE)

