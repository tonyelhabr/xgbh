
#' Make predictions
#'
#' Make predictions and compute SHAP values for new data.
#' @export
do_predict <-
  function(data = NULL,
           fit = NULL,
           path_fit = NULL,
           x = NULL,
           cols_x = NULL,
           y = NULL,
           col_y = NULL,
           id = NULL,
           col_id = NULL,
           f_import = arrow::read_parquet,
           f_export = arrow::write_parquet,
           augment = TRUE,
           do = TRUE,
           overwrite = TRUE,
           export = TRUE,
           ...,
           dir = .get_dir_data(),
           file = NULL,
           ext = 'parquet',
           sep = '_',
           suffix = NULL,
           use_y = FALSE,
           .do = list(
             preds = NULL,
             shap = NULL
           ),
           .f_import = list(
             preds = NULL,
             shap = NULL
           ),
           .f_export = list(
             preds = NULL,
             shap = NULL
           ),
           .export = list(
             preds = NULL,
             shap = NULL
           ),
           .overwrite = list(
             preds = NULL,
             shap = NULL
           ),
           .dir = list(
             preds = NULL,
             shap = NULL
           ),
           .file = list(
             preds = 'preds',
             shap = 'shap'
           ),
           .ext = list(
             preds = NULL,
             shap = NULL
           ),
           .path = list(
             preds = NULL,
             shap = NULL
           ),
           objective = NULL,
           cols_extra = NULL,
           f_trans = NULL) {

    has_fit <- !is.null(fit)
    has_path_fit <- !is.null(path_fit)

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

    path_preds <- .do_generate_path_partially('preds')
    path_shap <- .do_generate_path_partially('shap')

    .f_import$preds <- .f_import$preds %||% f_import
    .f_import$shap <- .f_import$shap %||% f_import
    .f_export$preds <- .f_export$preds %||% f_export
    .f_export$shap <- .f_export$shap %||% f_export
    .do$preds <- .do$preds %||% do
    .do$shap <- .do$shap %||% do
    .export$preds <- .export$preds %||% export
    .export$shap <- .export$shap %||% export
    .overwrite$preds <- .overwrite$preds %||% overwrite
    .overwrite$shap <- .overwrite$shap %||% overwrite


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


    has_cols_x <- !is.null(cols_x)
    if(is.null(cols_x)) {
      cols_x <- fit$feature_names
      if(is.null(cols_x)) {
        .display_warning('Unable to check names in data for `fit`. (This happens when you load in a model saved in a different session.)')
      } else {
        has_cols_x <- TRUE
      }
    }

    objective <- fit$params$objective %||% objective
    res_check <-
      .do_check(
        use_y = use_y,
        is_prediction = TRUE,
        objective = objective,
        data = data, # [, c(col_y, col_id, cols_x)],
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
    type_y <- res_check$type_y
    rm('res_check')

    if(has_cols_x) {
      nms_x <- colnames(x_mat)
      nms_diff1 <- setdiff(nms_x, cols_x)
      len_diff1 <- length(nms_diff1)
      len_diff_gt1 <- ifelse(len_diff1 > 1L, TRUE, FALSE)
      assertthat::assert_that(len_diff1 == 0L, msg = glue::glue('There {ifelse(len_diff_gt1, "are", "is")} {len_diff1} name{ifelse(len_diff_gt1, "s", "")} in  `x`/`data` that are not in `fit` .\nDifference{ifelse(len_diff_gt1, "s", "")}: {paste(nms_diff1, collapse = "`, `", sep = "")}'))
      # I think I should technically still do this, even with `x_mat` returned in `res_check`.

      nms_diff2 <- setdiff(cols_x, nms_x)
      len_diff2 <- length(nms_diff2)
      len_diff_gt2 <- ifelse(len_diff2 > 1L, TRUE, FALSE)
      assertthat::assert_that(len_diff2 == 0L, msg = glue::glue('There {ifelse(len_diff_gt2, "are", "is")} {len_diff1} name{ifelse(len_diff_gt2, "s", "")} in `fit` that are not in `x`/`data`.\nDifference{ifelse(len_diff_gt2, "s", "")}: {paste(nms_diff2, collapse = "`, `", sep = "")}'))
      # x_mat <- x %>% dplyr::select(dplyr::all_of(cols_x)) %>% .df2mat()
    }


    .f_predict <- function() {

      if(type_y == 'multiclass') {
        preds_v <- fit %>% stats::predict(x_mat, ...)
        n_class <- fit$params$num_class
        preds <-
          matrix(preds_v, ncol = n_class, byrow = TRUE) %>%
          as.data.frame() %>%
          as_tibble() %>%
          set_names(sprintf('.prob_%d', 1:n_class))
      } else if (isTRUE(objective == 'binary:logistic')) {
        preds_v <- fit %>% stats::predict(x_mat, ...)
        preds <- tibble::tibble(.prob = preds_v)
      } else if (type_y == 'continuous') {
        preds_v <- fit %>% stats::predict(x_mat, ...)
        preds <- tibble::tibble(.pred = preds_v)
      } else {
        .display_error('INTERNAL: `type_y` should be one of "multiclass", "binary", or "continuous" (not "{type_y}").')
      }

      if(!augment & .do$shap) {
        .display_warning('`augment` must be TRUE in order for SHAP to be output. Skipping SHAP.')
        .do$shap <- FALSE
      }

      has_f_trans <- !is.null(f_trans) & is.function(f_trans)
      if(has_f_trans & type_y != 'continuous') {
        .display_warning('Shouldn\'t transform a classification probability output. Ignoring `f_trans`.')
      } else if (has_f_trans) {
        preds <-
          preds %>%
          dplyr::mutate(dplyr::across(.pred, f_trans))
      }

      preds <-
        preds %>%
        dplyr::bind_cols(
          tibble::tibble(.id = id) %>% rlang::set_names(col_id)
        )

      # Only need to check `has_y` at this point, because `col_y` will have been assigned `.y` in `.do_check()` if it was previously NULL.
      has_y <- !is.null(y)
      if(has_y) {
        preds <-
          preds %>%
          dplyr::bind_cols(
            tibble::tibble(.y = y) %>% rlang::set_names(col_y)
          )
      }

      # if(augment & !is.null(cols_extra)) {
      #   .display_warning('Non-null `cols_extra` is ignored since `augment = FALSE`.')
      # }

      # These columns will be added twice if `augment = TRUE`
      has_cols_extra <- !is.null(cols_extra)
      if(has_cols_extra & !augment) {
        preds <-
          preds %>%
          dplyr::bind_cols(
            data %>% dplyr::select(dplyr::any_of(cols_extra))
          )
      }

      if(augment) {
        preds <- preds %>% dplyr::bind_cols(x)
      }

      preds

    }

    preds <-
      do_get(
        do = .do$preds,
        f = .f_predict,
        path = path_preds,
        f_import = .f_import$preds,
        f_export = .f_export$preds,
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
        do = .do$shap,
        f = .f_shap,
        path = path_shap,
        f_import = .f_import$shap,
        f_export = .f_export$shap,
        append = FALSE,
        export = .export$shap,
        overwrite = .overwrite$shap
      )

    list(preds = preds, shap = shap)
  }
