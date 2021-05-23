
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
           ...,
           drop = TRUE,
           dir = .get_dir_data(),
           sep = '_',
           suffix = NULL,
           seed = .get_seed(),
           .overwrite = list(
             tune = NULL,
             fit = NULL
           ),
           .export = list(
             tune = NULL,
             fit = NULL
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
           grid_params = NULL,
           n_param = 10,
           nrounds = 2000,
           booster = 'gbtree',
           objective = 'reg:squarederror',
           eval_metrics = list('rmse'),
           early_stopping_rounds = 10) {

    if(!is.null(suffix)) {
      suffix <- sprintf('%s%s', sep, suffix)
    } else {
      suffix <- ''
    }

    ..generate_path <- function(file, ext = NULL) {
      .generate_path(dir = dir, file = sprintf('%s%s', file, suffix), ext = ext)
    }
    .path_rds <- purrr::partial(..generate_path, ext = 'rds', ... = )

    path_res_tune_cv <- .path$res_tune_cv %||% ..generate_path('res_tune_cv', ext = 'rds')
    path_fit <- .path$fit %||% ..generate_path('fit', ext = 'model')

    .overwrite$tune <- .overwrite$tune %||% overwrite
    .overwrite$fit <- .overwrite$fit %||% overwrite
    .export$tune <- .export$tune %||% export
    .export$fit <- .export$fit %||% export

    has_data <- !is.null(data)
    has_col_wt <- !is.null(col_wt)
    has_wt <- !is.null(wt)

    res_check <-
      .do_check(
        data = data,
        x = x,
        y = y,
        col_y = col_y,
        id = id,
        col_id = col_id,
        wt = wt,
        col_wt = col_wt,
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
      x_dmat <-
        xgboost::xgb.DMatrix(
          x_mat,
          label = y
        )
    }

    .f_tune <- function() {
      set.seed(seed)

      has_folds <- !is.null(fold_ids)
      has_col_strata <- !is.null(col_strata)
      has_strata <- !is.null(strata)

      if(!has_folds) {

        if(!has_col_strata & !has_strata | has_data) {
          if(!has_col_strata & !has_strata) {
            .display_info('No `strata` or `col_strata`, so splitting based on `y`.')
            col_strata <- col_y
          } else if (has_data) {
            assertthat::assert_that(
              is.character(col_strata),
              msg = glue::glue('`col_strata` must be a character, not a {class(col_strata)}.')
            )
            assertthat::assert_that(
              length(col_strata) == 1L,
              msg = '`col_strata` must have length 1, not {length(col_strata)}.'
            )
            # browser()
            assertthat::assert_that(
              any(col_strata == nms),
              msg = glue::glue('`col_strata = {col_strata} is not in `names(data)`.')
            )
          }

          strata <- data[[col_strata]]

        }
      } else if(has_col_strata | has_strata) {

        assertthat::assert_that(has_strata, msg = '`col_strata` is specified but `data` is NULL and `strata` is NULL.')

        strata_is_df <- any(class(strata) == 'data.frame')

        if(strata_is_df) {
          n_col_strata <- ncol(strata)
          assertthat::assert_that(n_col_strata == 1L | has_col_strata, msg = glue::glue('If `strata` is passed in as a data.frame, it should have only one column (not, {n_col_strata}, unless `col_strata` is also specified.'))
          if(n_col_strata == 1L) {
            strata <- strata %>% dplyr::pull(1)
          } else {
            assertthat::assert_that(is.character(col_strata), msg = glue::glue('`col_strata` must be a character, not a {class(col_strata)}.'))
            assertthat::assert_that(length(col_strata) == 1L, msg = '`col_strata` must have length 1, not {length(col_strata)}.')
            assertthat::assert_that(any(col_strata == nms), msg = glue::glue('`col_strata = {col_strata} is not in `names(data)`.'))
            strata <- strata[[col_strata]]
          }
        } else {
          strata <- strata %>% as.vector()
        }

      }

      fold_ids <-
        .create_folds(
          strata,
          k = n_fold,
        )

      folds <-
        dplyr::bind_cols(
          dplyr::tibble(fold = fold_ids),
          dplyr::tibble(id = id)
        ) %>%
        split(.$fold) %>%
        purrr::map(~dplyr::select(.x, id) %>% dplyr::pull(id))

      if(has_data) {
        x <- data %>% dplyr::select(-dplyr::any_of(c(col_id, col_y, col_wt, col_strata)))
      }

      # This is sort of redundant(?) since I didn't necessarily need to convert it to a matrix before
      x_tbl <- tibble::as_tibble(x)
      # n_row <- x_tbl %>% nrow()
      n_col <- x_tbl %>% ncol()

      if(is.null(grid_params)) {
        grid_params <- generate_grid_params(x = x_tbl, n_param = n_param, n_col = n_col)
      } else {
        cls_params <- class(grid_params)
        cls_is_df <- any(cls_params == 'data.frame')
        assertthat::assert_that(cls_is_df, msg = glue::glue('`grid_params` must be a data.frame, not a {cls_params}.'))
        nms_params <- names(grid_params)
        assertthat::assert_that(any('idx' == nms_params), msg = glue::glue('`idx` (unique identifier) must be a column in `grid_params`.'))
      }

      res_tune_cv <-
        .tune_xgb_cv(
          nrounds = nrounds,
          grid_params = grid_params,
          folds = folds,
          x_dmat = x_dmat,
          booster = booster,
          objective = objective,
          eval_metrics = eval_metrics,
          sample_weight = wt,
          early_stopping_rounds = early_stopping_rounds,
          print_every_n = print_every_n
        )
      res_tune_cv
    }

    # browser()
    res_tune_cv <-
      do_get(
        f = .f_tune,
        path = path_res_tune_cv,
        f_import = readr::read_rds,
        f_export = readr::write_rds,
        append = FALSE,
        export = .export$tune,
        overwrite = .overwrite$tune
      )

    .f_fit <- function() {
      eval_metric <- eval_metrics[1]
      eval_metric_tst <- sprintf('%s_tst', eval_metric)
      eval_metric_tst_sym <- eval_metric_tst %>% sym()
      res_cv_best <- res_tune_cv %>% dplyr::slice_min(!!eval_metric_tst_sym)
      res_cv_best

      .pluck_param <- function(x) {
        res_cv_best %>% purrr::pluck(x)
      }

      params_best <-
        list(
          booster = booster,
          objective = objective,
          eval_metric = eval_metrics,
          eta = .pluck_param('eta'),
          gamma = .pluck_param('gamma'),
          subsample = .pluck_param('subsample'),
          colsample_bytree = .pluck_param('colsample_bytree'),
          max_depth = .pluck_param('max_depth'),
          min_child_weight = .pluck_param('min_child_weight')
        )
      params_best

      nrounds_best <- round((.pluck_param('iter') / ((n_fold - 1) / (n_fold))), 0) + early_stopping_rounds

      fit <-
        xgboost::xgboost(
          params = params_best,
          data = x_dmat,
          # data = x_mat,
          # label = y,
          # sample_weight = wt,
          nrounds = nrounds_best,
          early_stopping_rounds = early_stopping_rounds,
          print_every_n = print_every_n,
          verbose = 1
        )
    }

    fit <-
      do_get(
        f = .f_fit,
        path = path_fit,
        f_import = xgboost::xgb.load,
        f_export = xgboost::xgb.save,
        append = FALSE,
        export = .export$fit,
        overwrite = .overwrite$fit
      )
    list(res_tune_cv = res_tune_cv, fit = fit)
  }
