#' @noRd
.df2mat <- function(data) {
  stats::model.matrix(
    ~.+0,
    data =
      stats::model.frame(
        ~.+0,
        data,
        na.action = na.pass
      )
  )
}

#' @noRd
.postprocess_xgb_cv_res <- function(res, fit_cv, eval_metrics) {
  # NOTE: This doesn't generalize when there are more than one eval metrics.

  if(length(eval_metrics) > 1L) {
    .display_warning('Only know how to deal with `length(eval_metrics) == 1L`. Only using first element.')
  }
  .eval_metric <- eval_metrics[1]
  col_trn <- sprintf('train_%s_mean', .eval_metric)
  col_trn_res <- sprintf('%s_trn', .eval_metric)
  col_trn_sym <- col_trn %>% sym()
  col_trn_res_sym <- col_trn_res %>% sym()
  col_tst <- sprintf('test_%s_mean', .eval_metric)
  col_tst_res <- sprintf('%s_tst', .eval_metric)
  col_tst_sym <- col_tst %>% sym()
  col_tst_res_sym <- col_tst_res %>% sym()

  res$iter <- fit_cv$best_iteration
  res[[col_trn_res]] = fit_cv$evaluation_log[res$iter, ][[col_trn]]
  res[[col_tst_res]] = fit_cv$evaluation_log[res$iter, ][[col_tst]]
  res[['eval_metric']] <- NULL
  # browser()
  dplyr::bind_rows(res)
}

#' @noRd
.tune_xgb_cv <-
  function(grid_params,
           x_dmat,
           booster,
           objective,
           eval_metrics,
           ...) {

    .get_metrics <- function(params, idx = 1) {

      .display_info('Row {cli::bg_black(idx)}')

      res <-
        list(
          booster = booster,
          objective = objective,
          eval_metric = eval_metrics,
          eta = params$learn_rate,
          gamma = params$loss_reduction,
          subsample = params$sample_size,
          colsample_bytree = params$mtry,
          max_depth = params$tree_depth,
          min_child_weight = params$min_n
        )


      fit_cv <-
        xgboost::xgb.cv(
          data = x_dmat,
          params = res,
          metrics = eval_metrics,
          ...
        )

      res <- .postprocess_xgb_cv_res(res, fit_cv, eval_metrics)
      res
    }

    res <-
      grid_params %>%
      tidyr::nest(params = -idx) %>%
      dplyr::mutate(metrics = purrr::map2(params, idx, ~.get_metrics(params = ..1, idx = ..2))) %>%
      dplyr::select(-params) %>%
      tidyr::unnest(metrics)
    res
  }

#' Inverse log
#'
#' Perform an inverse log.
#' @export
inverse_log <- function(x) {
  exp(x) - 1
}


#' @noRd
.shap_xgb <- function(fit, x_mat, preds, col_y, col_id) {

  is_multi_soft <- fit$params$objective %>% str_detect('^multi[:].*prob$')
  is_binary_soft <- fit$params$objective == '^binary[:]logistic'
  # if(is_multi_soft | is_binary_soft) {
  #   preds_v <- fit %>% stats::predict(x_mat, ...)
  #   n_class <- fit$params$num_class
  #   preds <-
  #     matrix(preds_v, ncol = n_class, byrow = TRUE) %>%
  #     as.data.frame() %>%
  #     as_tibble() %>%
  #     set_names(sprintf('.prob_%d', 1:n_class))
  # } else {
  #   preds_v <- fit %>% stats::predict(x_mat, ...)
  #   preds <- tibble::tibble(.pred = preds_v)
  # }

  suppressWarnings(
    feature_values_init <-
      x_mat %>%
      as.data.frame() %>%
      dplyr::mutate_all(scale) %>%
      tidyr::gather('feature', 'feature_value') %>%
      dplyr::as_tibble()
  )
  feature_values_init

  feature_values <-
    feature_values_init %>%
    dplyr::pull(feature_value)
  feature_values

  shap_init <-
    fit %>%
    stats::predict(newdata = x_mat, predcontrib = TRUE) %>%
    as.data.frame() %>%
    dplyr::as_tibble() %>%
    dplyr::rename(baseline = BIAS)
  shap_init

  has_y <- any(names(preds) == col_y)
  rgx <- '^[.](pred|prob)'
  preds_join <- if(has_y) {
    preds %>%
      dplyr::select(!!sym(col_id), matches(rgx), .actual = !!sym(col_y))
  } else {
    preds %>%
      dplyr::select(!!sym(col_id), matches(rgx))
  }

  shap <-
    shap_init %>%
    dplyr::bind_cols(preds_join)
  shap
}

