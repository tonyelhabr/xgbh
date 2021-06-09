

#' Fit model
#'
#' Fit model.
#' @export
fit <-
  function(data = NULL,
           x = NULL,
           y = NULL,
           col_y = NULL,
           id = NULL,
           col_id = NULL,
           overwrite = TRUE,
           export = TRUE,
           f_import = readr::read_rds,
           f_export = readr::write_rds,
           ...,
           drop = TRUE,
           dir = .get_dir_data(),
           file = 'fit',
           ext = 'rds',
           path = NULL,
           sep = '_',
           suffix = NULL,
           print_every_n = 10,
           cols_extra = NULL,
           n_param = 10,
           nrounds = 2000,
           booster = 'gbtree',
           objective = 'reg:squarederror',
           eval_metrics = list('rmse'),
           .params = list(NULL),
           early_stopping_rounds = 10) {

    .do_generate_path_partially <- partial(
      .do_generate_path,
      .path = list(fit = path),
      dir = dir,
      file = file,
      ext = ext,
      sep = sep,
      suffix = suffix,
      ... =
    )

    path_fit <- .do_generate_path_partially('fit')

    has_data <- !is.null(data)
    has_col_wt <- !is.null(col_wt)
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
        cols_extra = cols_extra,
        drop = drop
      )
    y <- res_check$y
    # col_y <- res_check$col_y
    x_mat <- res_check$x_mat
    rm('res_check')
    x_dmat <-
      xgboost::xgb.DMatrix(
        x_mat,
        label = y
      )

    .f_fit <- function() {
      eval_metric <- eval_metrics[1]

      params <-
        list(
          booster = booster,
          objective = objective,
          eval_metric = eval_metrics
        )
      params
      params <- purrr::compact(c(params, .params))

      fit <-
        xgboost::xgboost(
          params = params,
          data = x_dmat,
          label = y,
          nrounds = nrounds,
          early_stopping_rounds = early_stopping_rounds,
          print_every_n = print_every_n,
          ...
        )
    }

    fit <-
      do_get(
        f = .f_fit,
        path = path_fit,
        f_import = f_import,
        f_export = f_export,
        append = FALSE,
        export = export,
        overwrite = overwrite
      )
    fit
  }
