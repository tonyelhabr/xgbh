
#' Save a plot
#'
#' Save params
#' @param height,width Height and width to use for saved image, in inches.
#' @importFrom ggplot2 ggsave
#' @export
save_plot <-
  function(object,
           path = NULL,
           dir = .get_dir_data(),
           file = deparse(substitute(object)),
           ext = 'png',
           height = 8,
           width = height,
           ...) {
    path <- .generate_path(path = path, dir = dir, file = file, ext = ext)
    ggplot2::ggsave(
      plot = object,
      filename = path,
      width = width,
      height = height,
      type = 'cairo',
      ...
    )
  }

#' Make SHAP plots
#'
#' Make SHAP plots
#' @importFrom purrr partial
#' @importFrom ggplot2 ggsave ggplot geom_col labs aes theme element_blank scale_size_area scale_color_distiller
#' @importFrom magick image_read
#' @importFrom dplyr summarize mutate group_by ungroup across row_number desc matches any_of arrange
#' @importFrom forcats fct_reorder
#' @importFrom tidyr pivot_longer
#' @importFrom ggbeeswarm geom_quasirandom
#' @export
do_plot_shap <-
  function(shap,
           col_y = NULL,
           col_id = NULL,
           cols_extra = NULL,
           f_import = magick::image_read,
           f_export = save_plot,
           do = TRUE,
           overwrite = TRUE,
           export = TRUE,
           ...,
           dir = .get_dir_data(),
           file = NULL,
           ext = 'png',
           sep = '_',
           prefix = 'viz',
           suffix = NULL,
           .do = list(
             agg = NULL,
             swarm = NULL
           ),
           .f_import = list(
             agg = NULL,
             swarm = NULL
           ),
           .f_export = list(
             agg = purrr::partial(save_plot, width = 8, height = 8),
             swarm = purrr::partial(save_plot, width = 12, height = 8)
           ),
           .export = list(
             agg = NULL,
             swarm = NULL
           ),
           .overwrite = list(
             agg = NULL,
             swarm = NULL
           ),
           .dir = list(
             agg = NULL,
             swarm = NULL
           ),
           .file = list(
             agg = 'agg',
             swarm = 'swarm'
           ),
           .ext = list(
             agg = NULL,
             swarm = NULL
           ),
           .path = list(
             agg = NULL,
             swarm = NULL
           )) {

    .do_generate_path_partially <- purrr::partial(
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
      prefix = prefix,
      ... =
    )

  path_agg <- .do_generate_path_partially('agg')
  path_swarm <- .do_generate_path_partially('swarm')
  prnk <-
    shap %>%
    dplyr::select(all_of(col_id), .actual) %>%
    dplyr::mutate(prnk = dplyr::percent_rank(.actual))

  .f_import$agg <- .f_import$agg %||% f_import
  .f_import$swarm <- .f_import$swarm %||% f_import
  .f_export$agg <- .f_export$agg %||% f_export
  .f_export$swarm <- .f_export$swarm %||% f_export
  .do$agg <- .do$agg %||% do
  .do$swarm <- .do$swarm %||% do
  .export$agg <- .export$agg %||% export
  .export$swarm <- .export$swarm %||% export
  .overwrite$agg <- .overwrite$agg %||% overwrite
  .overwrite$swarm <- .overwrite$swarm %||% overwrite

  shap_tidy <-
    shap %>%
    tidyr::pivot_longer(
      -c('baseline', dplyr::any_of(c(col_y, col_id, cols_extra)), dplyr::matches('^[.](prob|pred|actual)')),
      names_to = 'feature',
      values_to = 'shap_value'
    )

  shap_agg_by_feature <-
    shap_tidy %>%
    dplyr::group_by(feature) %>%
    dplyr::summarize(
      dplyr::across(shap_value, ~mean(abs(.x))),
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      dplyr::across(shap_value, list(rnk = ~dplyr::row_number(dplyr::desc(.x)))),
      dplyr::across(feature, ~forcats::fct_reorder(feature, -shap_value_rnk))
    ) %>%
    dplyr::arrange(shap_value_rnk)
  shap_agg_by_feature

  .f_agg <- function() {
    viz_shap_agg <-
      shap_agg_by_feature %>%
      ggplot2::ggplot() +
      ggplot2::aes(y = feature, x = shap_value) +
      ggplot2::geom_col() +
      ggplot2::theme(
        panel.grid.major.y = ggplot2::element_blank(),
        axis.ticks.y = ggplot2::element_blank()
      ) +
      ggplot2::labs(
        x = 'mean(|SHAP|)',
        y = NULL
      )
    viz_shap_agg
  }

  agg <-
    do_get(
      do = .do$agg,
      f = .f_agg,
      path = path_agg,
      f_import = .f_import$agg,
      f_export = .f_export$agg,
      append = FALSE,
      export = .export$agg,
      overwrite = .overwrite$agg
    )

  .f_swarm <- function() {
    viz_shap_swarm <-
      shap_tidy %>%
      dplyr::left_join(shap_agg_by_feature %>% dplyr::select(-shap_value)) %>%
      dplyr::left_join(prnk) %>%
      dplyr::mutate(
        dplyr::across(feature, ~forcats::fct_reorder(feature, -shap_value_rnk)),
      ) %>%
      dplyr::mutate(scale = dplyr::if_else(shap_value > 0.5, shap_value, 1 - shap_value)) %>%
      ggplot2::ggplot() +
      ggplot2::aes(x = shap_value, y = feature) +
      ggbeeswarm::geom_quasirandom(
        aes(color = prnk, size = scale, alpha = scale),
        show.legend = FALSE,
        groupOnX = FALSE,
        varwidth = TRUE
      ) +
      ggplot2::scale_color_distiller(
        palette = 'RdBu'
      ) +
      ggplot2::scale_size_area(max_size = 3)
    viz_shap_swarm
  }

  swarm <-
    do_get(
      do = .do$swarm,
      f = .f_swarm,
      path = path_swarm,
      f_import = .f_import$swarm,
      f_export = .f_export$swarm,
      append = FALSE,
      export = .export$swarm,
      overwrite = .overwrite$swarm
    )

  list(agg = agg, swarm = swarm)
}
