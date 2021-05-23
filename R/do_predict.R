
#' Make predictions
#'
#' Make predictions for new data.
#' @export
do_predict <-
  function(data = NULL,
           fit = NULL,
           path_fit = NULL,
           x = NULL,
           y = NULL,
           col_y = NULL,
           id = NULL,
           col_id = NULL,
           overwrite = TRUE,
           export = TRUE,
           ...,
           dir = .get_dir_data(),
           sep = '_',
           suffix = NULL,
           .overwrite = list(
             preds = NULL,
             shap = NULL
           ),
           .export = list(
             preds = NULL,
             shap = NULL
           ),
           .path = list(
             preds = NULL,
             shap = NULL
           ),
           cols_extra = NULL,
           f_trans = NULL) {

    has_fit <- !is.null(fit)
    has_path_fit <- !is.null(path_fit)

    if(!is.null(suffix)) {
      suffix <- sprintf('%s%s', sep, suffix)
    } else {
      suffix <- ''
    }

    ..generate_path <- function(file, ext = NULL) {
      .generate_path(dir = dir, file = sprintf('%s%s', file, suffix), ext = ext)
    }
    .path_rds <- purrr::partial(..generate_path, ext = 'rds', ... = )

    path_preds <- .path$preds %||% ..generate_path('preds', ext = 'rds')
    path_shap <- .path$shap %||% ..generate_path('shap', ext = 'rds')

    .overwrite$preds <- .overwrite$preds %||% overwrite
    .overwrite$shap <- .overwrite$shap %||% overwrite
    .export$preds <- .export$preds %||% export
    .export$shap <- .export$shap %||% export

    assertthat::assert_that(has_fit | has_path_fit, msg = '`fit` and `path_fit.` cannot both be NULL')

    if(!has_fit) {
      .import_safely <- purrr::safely(xgboost::xgb.load, otherwise = NULL)
      fit <- .import_safely(path_fit)
      if(is.null(fit)) {
        .display_error('The `fit` loaded from `path_fit = "{path_fit}"` is not an `{xgboost}` model.')
      }
    }
    cls_fit <- class(fit)
    is_xgb <- any('xgb.Booster' == cls_fit)

    assertthat::assert_that(is_xgb, msg = '`fit` must be of class `xgb.Booster`, which is not in {cls_fit}.')
    cols_fit <- fit$feature_names

    res_check <-
      .do_check(
        data = data[, c(col_y, col_id, cols_fit)],
        x = x,
        y = y,
        col_y = col_y,
        id = id,
        col_id = col_id
      )
    data <- res_check$data
    x <- res_check$x
    y <- res_check$y
    col_y <- res_check$col_y
    id <- res_check$id
    col_id <- res_check$col_id
    x_mat <- res_check$x_mat
    rm('res_check')

    # I think I should technically still do this, even with `x_mat` returned in `res_check`.
    x_mat <- x %>% dplyr::select(dplyr::all_of(cols_fit)) %>% .df2mat()

    .f_predict <- function() {

      preds <-
        fit %>%
        stats::predict(x_mat) %>%
        .augment_preds(
          x = x,
          f_trans = f_trans
        ) %>%
        dplyr::bind_cols(
          tibble::tibble(.y = y) %>% rlang::set_names(col_y),
          tibble::tibble(.id = id) %>% rlang::set_names(col_id),
          data %>% dplyr::select(dplyr::any_of(cols_extra))
        )
      preds
    }

    preds <-
      do_get(
        f = .f_predict,
        path = path_preds,
        f_import = readr::read_rds,
        f_export = readr::write_rds,
        append = FALSE,
        export = .export$preds,
        overwrite = .overwrite$preds
      )

    .f_shap <- function() {
      shap <-
        .shap_xgb(
          x_mat = x_mat,
          fit = fit,
          preds = preds,
          col_id = col_id,
          col_y = col_y
        )
    }

    shap <-
      do_get(
        f = .f_shap,
        path = path_shap,
        f_import = readr::read_rds,
        f_export = readr::write_rds,
        append = FALSE,
        export = .export$shap,
        overwrite = .overwrite$shap
      )
    list(preds = preds, shap = shap)
  }
