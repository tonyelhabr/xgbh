
#' Fit model
#'
#' Tune and fit model.
#' @export
do_fit <-
  function(data = NULL,
           x = NULL,
           y = NULL,
           col_y = NULL,
           id = NULL,
           col_id = NULL,
           overwrite = TRUE,
           export = TRUE,
           minimal = FALSE,
           f_import = readr::read_rds,
           f_export = readr::write_rds,
           ...,
           drop = TRUE,
           dir = .get_dir_data(),
           file = NULL,
           ext = 'rds',
           sep = '_',
           suffix = NULL,
           seed = .get_seed(),
           .do = list(
             tune = NULL,
             fit = NULL
           ),
           .f_import = list(
             tune = NULL,
             fit = if(minimal) { xgboost::xgb.load } else { NULL }
           ),
           .f_export = list(
             tune = NULL,
             fit = if(minimal) { xgboost::xgb.save } else { NULL }
           ),
           .export = list(
             tune = NULL,
             fit = NULL
           ),
           .overwrite = list(
             tune = NULL,
             fit = NULL
           ),
           .dir = list(
             tune = NULL,
             fit = NULL
           ),
           .file = list(
             tune = 'tune',
             fit = 'fit'
           ),
           .ext = list(
             tune = NULL,
             fit = if(minimal) { 'model' } else { NULL }
           ),
           .path = list(
             tune = NULL,
             fit = NULL
           ),
           print_every_n = 10,
           fold_ids = NULL,
           n_fold = 10,
           strata = NULL,
           col_strata = NULL,
           wt = NULL,
           col_wt = NULL,
           cols_extra = NULL,
           grid_params = NULL,
           n_param = 10,
           nrounds = 2000,
           booster = 'gbtree',
           objective = 'reg:squarederror',
           eval_metrics = list('rmse'),
           .params_tune = list(NULL),
           .params = list(NULL),
           f_slice = dplyr::slice_min,
           early_stopping_rounds = 10) {

    .do_generate_path_partially <- partial(
      .do_generate_path,
      .path = .path,
      .dir = .dir,
      .file = .file,
      .ext = .ext,
      dir = dir,
      file = file,
      ext = ext,
      sep = sep,
      suffix = suffix,
      ... =
    )

    path_tune <- .do_generate_path_partially('tune')
    path_fit <- .do_generate_path_partially('fit')

    .f_import$tune <- .f_import$tune %||% f_import
    .f_import$fit <- .f_import$fit %||% f_import
    .f_export$tune <- .f_export$tune %||% f_export
    .f_export$fit <- .f_export$fit %||% f_export
    .do$tune <- .do$tune %||% do
    .do$fit <- .do$fit %||% do
    .export$tune <- .export$tune %||% export
    .export$fit <- .export$fit %||% export
    .overwrite$tune <- .overwrite$tune %||% overwrite
    .overwrite$fit <- .overwrite$fit %||% overwrite

    has_data <- !is.null(data)
    has_col_wt <- !is.null(col_wt)
    has_wt <- !is.null(wt)
    res_check <-
      .do_check(
        use_y = TRUE,
        is_prediction = FALSE,
        objective = objective,
        data = data,
        x = x,
        y = y,
        col_y = col_y,
        id = id,
        col_id = col_id,
        wt = wt,
        col_wt = col_wt,
        cols_extra = cols_extra,
        drop = drop
      )
    data <- res_check$data
    x <- res_check$x
    y <- res_check$y
    col_y <- res_check$col_y
    id <- res_check$id
    col_id <- res_check$col_id
    wt <- res_check$wt
    col_wt <- res_check$col_wt
    x_mat <- res_check$x_mat
    rm('res_check')

    if(has_col_wt | has_wt) {

      x_dmat <-
        xgboost::xgb.DMatrix(
          x_mat,
          weight = wt,
          label = y
        )
    } else {
      # browser()
      x_dmat <-
        xgboost::xgb.DMatrix(
          x_mat,
          label = y
        )
    }

    .f_tune <- function() {
      set.seed(seed)
      # browser()
      has_folds <- !is.null(fold_ids)
      has_col_strata <- !is.null(col_strata)
      has_strata <- !is.null(strata)

      if(!has_folds) {

        if(has_col_strata | has_strata) {
          if(has_col_strata & !has_strata) {
            assertthat::assert_that(has_col_strata, msg = '`col_strata` cannot be `NULL`.')
            assertthat::assert_that(is.character(col_strata), msg = glue::glue('`col_strata` must be a character, not a {class(col_strata)}.'))
            assertthat::assert_that(length(col_strata) == 1L, msg = '`col_strata` must have length 1, not {length(col_strata)}.')
            assertthat::assert_that(any(col_strata == nms), msg = glue::glue('`col_strata = {col_strata} is not in `names(data)`.'))
            strata <- data[[col_strata]]
          } else {
            strata_is_df <- any(class(strata) == 'data.frame')

            if(strata_is_df) {
              n_col_strata <- ncol(strata)
              assertthat::assert_that(n_col_strata == 1L | has_col_strata, msg = glue::glue('If `strata` is passed in as a data.frame, it should have only one column (not, {n_col_strata}, unless `col_strata` is also specified.'))
              if(n_col_strata == 1L) {
                strata <- strata %>% dply::pull(1)
              } else {
                assertthat::assert_that(is.character(col_strata), msg = glue::glue('`col_strata` must be a character, not a {class(col_strata)}.'))
                assertthat::assert_that(length(col_strata) == 1L, msg = '`col_strata` must have length 1, not {length(col_strata)}.')
                assertthat::assert_that(any(col_strata == nms), msg = glue::glue('`col_strata = {col_strata} is not in `names(data)`.'))
                strata <- strata[[col_strata]]
                data <- dplyr::bind_cols(data, tibble::tibble(strata) %>% rlang::set_names(col_strata))
              }

            } else {
              strata <- strata %>% as.vector()
            }
          }

          len_strata <- length(strata)
          len_y <- length(y)
          assertthat::assert_that(len_strata == len_y, msg = glue::glue('`strata` should have the same length ({len_strata}) length as `y` ({len_y}).'))

          folds <-
            dplyr::bind_cols(
              dplyr::tibble(fold = fold_ids),
              dplyr::tibble(id = id)
            ) %>%
            split(.$fold) %>%
            purrr::map(~dplyr::select(.x, id) %>% dplyr::pull(id))

          fold_ids <- create_folds(strata, k = n_fold)

        } else {
          # col_strata <- col_y
          # strata <- y
          .display_info('Using `xgboost`\'s `nfolds` ({n_fold}) since `strata` and `col_y` are NULL.')
        }

      } else {
        assertthat::assert_that(is.list(folds), msg = '`folds` should be a list')
        len_list <- folds %>% unlist() %>% length()
        len_y <- length(y)
        assertthat::assert_that((len_y %% len_list == 0), msg = glue::glue('Length of un-listed `folds` ({len_list}) should be a multiple of the length of `y` ({len_y}).'))
      }

      if(has_data) {
        x <- data %>% dplyr::select(-dplyr::any_of(c(col_id, col_y, col_wt, col_strata)))
      }

      # This is sort of redundant(?) since I didn't necessarily need to convert it to a matrix before
      x_tbl <- tibble::as_tibble(x)
      n_col <- ncol(x_tbl)

      if(is.null(grid_params)) {
        grid_params <- generate_grid_params(x = x_tbl, n_param = n_param, n_col = n_col)
      } else {
        cls_params <- class(grid_params)
        cls_is_df <- any(cls_params == 'data.frame')
        assertthat::assert_that(cls_is_df, msg = glue::glue('`grid_params` must be a data.frame, not a {cls_params}.'))
        nms_params <- names(grid_params)
        assertthat::assert_that(any('idx' == nms_params), msg = glue::glue('`idx` (unique identifier) must be a column in `grid_params`.'))
      }

      .tune_xgb_cv_partially <- function(...) {
        .tune_xgb_cv(
          nrounds = nrounds,
          grid_params = grid_params,
          x_dmat = x_dmat,
          booster = booster,
          objective = objective,
          eval_metrics = eval_metrics,
          sample_weight = wt,
          early_stopping_rounds = early_stopping_rounds,
          print_every_n = print_every_n,
          .params = .params_tune,
          ...
        )
      }

      res_tune_cv <- if(is.null(strata)) {
        .tune_xgb_cv_partially(nfold = n_fold, ...)
      } else {
        .tune_xgb_cv_partially(folds = folds, ...)
      }
      res_tune_cv
    }

    res_tune_cv <-
      do_get(
        f = .f_tune,
        path = path_tune,
        f_import = .f_import$tune, # readr::read_rds,
        f_export = .f_export$tune, # readr::write_rds,
        append = FALSE,
        export = .export$tune,
        overwrite = .overwrite$tune
      )

    .f_fit <- function() {
      eval_metric <- eval_metrics[1]
      eval_metric_tst <- sprintf('%s_tst', eval_metric)
      eval_metric_tst_sym <- eval_metric_tst %>% sym()
      res_cv_best <- res_tune_cv %>% f_slice(!!eval_metric_tst_sym)
      .pluck_param <- function(x) {
        res_cv_best %>% purrr::pluck(x)
      }

      params_best <-
        list(
          booster = booster,
          objective = objective,
          eval_metric = eval_metrics,
          eta = .params$eta %||% .pluck_param('eta'),
          gamma = .params$gamma %||% .pluck_param('gamma'),
          subsample = .params$subsample %||% .pluck_param('subsample'),
          colsample_bytree = .params$colsample_bytree %||% .pluck_param('colsample_bytree'),
          max_depth = .params$max_depth %||% .pluck_param('max_depth'),
          min_child_weight = .params$min_child_weight %||% .pluck_param('min_child_weight')
        )
      params_best

      nrounds_best <- round((.pluck_param('iter') / ((n_fold - 1) / (n_fold))), 0) + early_stopping_rounds

      fit <-
        xgboost::xgboost(
          params = params_best,
          data = x_dmat,
          label = y,
          sample_weight = wt,
          nrounds = nrounds_best,
          early_stopping_rounds = early_stopping_rounds,
          print_every_n = print_every_n,
          # verbose = verbose,
          ...
        )
    }

    fit <-
      do_get(
        f = .f_fit,
        path = path_fit,
        f_import = .f_import$fit, # xgboost::xgb.load,
        f_export = .f_export$fit, # xgboost::xgb.save,
        append = FALSE,
        export = .export$fit,
        overwrite = .overwrite$fit
      )
    list(res_tune_cv = res_tune_cv, fit = fit)
  }
