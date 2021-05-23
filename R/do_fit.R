
#' Fit model
#'
#' Tune and fit model.
#' @export
do_fit <-
  function(data = NULL,
           x = NULL,
           y = NULL,
           overwrite = TRUE,
           export = TRUE,
           ...,
           col_id = NULL,
           id = NULL,
           col_y = NULL,
           drop = TRUE,
           dir = .get_dir_data(),
           sep = '_',
           suffix = NULL,
           seed = 42,
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
           col_strata = NULL,
           strata = NULL,
           col_wt = NULL,
           wt = NULL,
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
    has_col_id <- !is.null(col_id)
    has_id <- !is.null(id)
    has_x <- !is.null(x)
    has_col_y <- !is.null(col_y)
    has_y <- !is.null(y)
    has_col_wt <- !is.null(col_wt)
    has_wt <- !is.null(wt)

    if(drop & !has_col_y) {
      .display_warning('Can\'t drop if `col_y` and `data` aren\'t provided.')
    }

    if(has_data) {

      nms <- data %>% names()

      assertthat::assert_that(has_col_y | has_y, msg = glue::glue('Must provide `col_y` or `y`.'))
      if(has_col_y & !has_y) {

        assertthat::assert_that(has_col_y, msg = '`col_y` cannot be `NULL`.')
        assertthat::assert_that(is.character(col_y), msg = glue::glue('`col_y` must be a character, not a {class(col_y)}.'))
        assertthat::assert_that(length(col_y) == 1L, msg = '`col_y` must have length 1, not {length(col_y)}.')
        assertthat::assert_that(any(col_y == nms), msg = glue::glue('`col_y = {col_y} is not in `names(data)`.'))
        col_y_sym <- col_y %>% sym()
        y <- data[[col_y]]
      } else {
        y_is_df <- any(class(y) == 'data.frame')

        if(y_is_df) {
          n_col_y <- ncol(y)
          assertthat::assert_that(n_col_y == 1L | has_col_y, msg = glue::glue('If `y` is passed in as a data.frame, it should have only one column (not, {n_col_y}, unless `col_y` is also specified.'))
          if(n_col_y == 1L) {
            y <- y %>% dplyr::pull(1)
          } else {
            assertthat::assert_that(is.character(col_y), msg = glue::glue('`col_y` must be a character, not a {class(col_y)}.'))
            assertthat::assert_that(length(col_y) == 1L, msg = '`col_y` must have length 1, not {length(col_y)}.')
            assertthat::assert_that(any(col_y == nms), msg = glue::glue('`col_y = {col_y} is not in `names(data)`.'))
            y <- y[[col_y]]
            data <- dplyr::bind_cols(data, tibble::tibble(y) %>% rlang::set_names(col_y))
          }

        } else {
          y <- y %>% as.vector()
        }
      }

      len_y <- length(y)
      n_row <- nrow(data)
      assertthat::assert_that(len_y == n_row, msg = glue::glue('`y` should have the same length ({len_y}) as the number of rows in `data` ({n_row}).'))

      if(drop & !has_col_y) {
        n_before <- nrow(data)
        data <- data %>% dplyr::filter(!is.na(!!col_y_sym))
        n_after <- nrow(data)
        y <- y[!is.na(y)]
        if(n_before != n_after) {
          .display_warning('# of rows before dropping: {n_before}
                           # of rows after dropping: {n_after}')
        }
      }

      assertthat::assert_that(has_col_id | has_id, msg = glue::glue('Must provide `col_id` or `id`.'))
      if(has_col_id & !has_id) {
        assertthat::assert_that(has_col_id, msg = '`col_id` cannot be `NULL`.')
        assertthat::assert_that(is.character(col_id), msg = glue::glue('`col_id` must be a character, not a {class(col_id)}.'))
        assertthat::assert_that(length(col_id) == 1L, msg = '`col_id` must have length 1, not {length(col_id)}.')
        assertthat::assert_that(any(col_id == nms), msg = glue::glue('`col_id = {col_id} is not in `names(data)`.'))
        col_id_sym <- col_id %>% sym()
        id <- data[[col_id]]
      } else {
        id_is_df <- any(class(id) == 'data.frame')

        if(id_is_df) {
          n_col_id <- ncol(id)
          assertthat::assert_that(n_col_id == 1L | has_col_id, msg = glue::glue('If `id` is passed in as a data.frame, it should have onlid one column (not, {n_col_id}, unless `col_id` is also specified.'))
          if(n_col_id == 1L) {
            id <- id %>% dply::pull(1)
          } else {
            assertthat::assert_that(is.character(col_id), msg = glue::glue('`col_id` must be a character, not a {class(col_id)}.'))
            assertthat::assert_that(length(col_id) == 1L, msg = '`col_id` must have length 1, not {length(col_id)}.')
            assertthat::assert_that(any(col_id == nms), msg = glue::glue('`col_id = {col_id} is not in `names(data)`.'))
            id <- id[[col_id]]
          }
          data <- dplyr::bind_cols(data, tibble::tibble(id) %>% rlang::set_names(col_id))
        } else {
          id <- id %>% as.vector()
        }
      }

      len_id <- length(id)
      n_row <- nrow(data)
      assertthat::assert_that(len_id == n_row, msg = glue::glue('`id` should have the same length ({len_id}) as the number of rows in `data` ({n_row}).'))

      if(has_col_wt | has_wt) {
        if(has_col_wt & !has_wt) {
          assertthat::assert_that(has_col_wt, msg = '`col_wt` cannot be `NULL`.')
          assertthat::assert_that(is.character(col_wt), msg = glue::glue('`col_wt` must be a character, not a {class(col_wt)}.'))
          assertthat::assert_that(length(col_wt) == 1L, msg = '`col_wt` must have length 1, not {length(col_wt)}.')
          assertthat::assert_that(any(col_wt == nms), msg = glue::glue('`col_wt = {col_wt} is not in `names(data)`.'))
          col_wt_sym <- col_wt %>% sym()
          wt <- data[[col_wt]]
        } else {
          wt_is_df <- anwt(class(wt) == 'data.frame')

          if(wt_is_df) {
            n_col_wt <- ncol(wt)
            assertthat::assert_that(n_col_wt == 1L | has_col_wt, msg = glue::glue('If `wt` is passed in as a data.frame, it should have onlwt one column (not, {n_col_wt}, unless `col_wt` is also specified.'))
            if(n_col_wt == 1L) {
              wt <- wt %>% dply::pull(1)
            } else {
              assertthat::assert_that(is.character(col_wt), msg = glue::glue('`col_wt` must be a character, not a {class(col_wt)}.'))
              assertthat::assert_that(length(col_wt) == 1L, msg = '`col_wt` must have length 1, not {length(col_wt)}.')
              assertthat::assert_that(anwt(col_wt == nms), msg = glue::glue('`col_wt = {col_wt} is not in `names(data)`.'))
              wt <- wt[[col_wt]]
              data <- dplyr::bind_cols(data, tibble::tibble(wt) %>% rlang::set_names(col_wt))
            }

          } else {
            wt <- wt %>% as.vector()
          }
        }
        len_wt <- length(wt)
        assertthat::assert_that(len_wt == n_row, msg = glue::glue('`wt` should have the same length ({len_wt}) as the number of rows in `data` ({n_row}).'))
      }

      x <- data %>% dplyr::select(-dplyr::any_of(c(col_id, col_y, col_wt)))

    } else {

      assertthat::assert_that(has_x, msg = '`x` cannot be NULL if `data` is NULL.')

      assertthat::assert_that(has_col_y | has_y, msg = '`col_y` or `y` cannot both be NULL.')
      assertthat::assert_that(has_y, msg = '`col_y` is specified but `y` is NULL.')
      y_is_df <- any(class(y) == 'data.frame')

      if(y_is_df) {
        n_col_y <- ncol(y)
        assertthat::assert_that(n_col_y == 1L | has_col_y, msg = glue::glue('If `y` is passed in as a data.frame, it should have only one column (not, {n_col_y}, unless `col_y` is also specified.'))
        if(n_col_y == 1L) {
          y <- y %>% dplyr::pull(1)
        } else {
          assertthat::assert_that(is.character(col_y), msg = glue::glue('`col_y` must be a character, not a {class(col_y)}.'))
          assertthat::assert_that(length(col_y) == 1L, msg = '`col_y` must have length 1, not {length(col_y)}.')
          assertthat::assert_that(any(col_y == nms), msg = glue::glue('`col_y = {col_y} is not in `names(data)`.'))
          y <- y[[col_y]]
        }

      } else {
        y <- y %>% as.vector()
      }

      len_y <- length(y)
      n_row <- nrow(x)
      assertthat::assert_that(len_y == n_row, msg = glue::glue('`y` should have the same length ({len_y}) as the number of rows in `x` ({n_row}).'))

      assertthat::assert_that(has_col_id | has_id, msg = '`col_id` or `id` cannot both be NULL.')
      assertthat::assert_that(has_id, msg = '`col_id` is specified but `id` is NULL.')
      id_is_df <- any(class(id) == 'data.frame')

      if(id_is_df) {
        n_col_id <- ncol(id)
        assertthat::assert_that(n_col_id == 1L | has_col_id, msg = glue::glue('If `id` is passed in as a data.frame, it should have onlid one column (not, {n_col_id}, unless `col_id` is also specified.'))
        if(n_col_id == 1L) {
          id <- id %>% dplidr::pull(1)
        } else {
          assertthat::assert_that(is.character(col_id), msg = glue::glue('`col_id` must be a character, not a {class(col_id)}.'))
          assertthat::assert_that(length(col_id) == 1L, msg = '`col_id` must have length 1, not {length(col_id)}.')
          assertthat::assert_that(any(col_id == nms), msg = glue::glue('`col_id = {col_id} is not in `names(data)`.'))
          id <- id[[col_id]]
        }

      } else {
        id <- id %>% as.vector()
      }

      len_id <- length(id)
      assertthat::assert_that(len_id == n_row, msg = glue::glue('`id` should have the same length ({len_id}) as the number of rows in `x` ({n_row}).'))

      if(has_col_wt | has_wt) {

        assertthat::assert_that(has_wt, msg = '`col_wt` is specified but `data` is NULL and `wt` is NULL.')

        wt_is_df <- any(class(wt) == 'data.frame')
        if(wt_is_df) {
          n_col_wt <- ncol(wt)
          assertthat::assert_that(n_col_wt == 1L | has_col_wt, msg = glue::glue('If `wt` is passed in as a data.frame, it should have only one column (not, {n_col_wt}, unless `col_wt` is also specified.'))
          if(n_col_wt == 1L) {
            wt <- wt %>% dplyr::pull(1)
          } else {
            assertthat::assert_that(is.character(col_wt), msg = glue::glue('`col_wt` must be a character, not a {class(col_wt)}.'))
            assertthat::assert_that(length(col_wt) == 1L, msg = '`col_wt` must have length 1, not {length(col_wt)}.')
            assertthat::assert_that(any(col_wt == nms), msg = glue::glue('`col_wt = {col_wt} is not in `names(data)`.'))
            wt <- wt[[col_wt]]
          }
        } else {
          wt <- wt %>% as.vector()
        }
        len_wt <- length(wt)
        assertthat::assert_that(len_wt == n_row, msg = glue::glue('`wt` should have the same length ({len_wt}) as the number of rows in `x` ({n_row}).'))
      }

    }

    x_mat <- x %>% .df2mat()
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
          # col_strata_sym <- col_strata %>% sym()
          # fold_ids <-
          #   .create_folds(
          #     strata,
          #     k = n_fold
          #   )
          # browser()
          # folds <-
          #   data %>%
          #   dplyr::bind_cols(dplyr::tibble(fold = fold_ids)) %>%
          #   dplyr::left_join(
          #     data %>% dplyr::select(!!col_strata_sym, !!col_id_sym)
          #   ) %>%
          #   dplyr::select(fold, !!col_id_sym) %>%
          #   split(.$fold) %>%
          #   purrr::map(~dplyr::select(.x, !!col_id_sym) %>% dplyr::pull(!!col_id_sym))

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

        # fold_ids <-
        #     strata,
        #     k = n_fold,
        #   )
        #
        # folds <-
        #   dplyr::bind_cols(
        #     dplyr::tibble(fold = fold_ids),
        #     dplyr::tibble(id = id)
        #   ) %>%
        #   split(.$fold) %>%
        #   purrr::map(~dplyr::select(.x, id) %>% dplyr::pull(id))

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
        grid_params <- .generate_grid_params(x = x_tbl, n_param = n_param, n_col = n_col)
      } else {
        cls_params <- class(grid_params)
        cls_is_df <- any(cls_grid == 'data.frame')
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
