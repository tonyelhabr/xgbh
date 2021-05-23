
.generate_grid_params <- function(x, n_param = 10, n_col = ncol(x)) {

  x <- tibble::as_tibble(x)
  res <-
    dials::grid_latin_hypercube(
      dials::finalize(dials::mtry(), x),
      dials::min_n(),
      dials::tree_depth(),
      dials::learn_rate(),
      dials::loss_reduction(),
      sample_size = dials::sample_prop(),
      size = n_param
    ) %>%
    dplyr::mutate(
      learn_rate = 0.1 * ((1:n_param) / n_param),
      mtry = mtry / n_col,
      idx = dplyr::row_number()
    ) %>%
    dplyr::relocate(idx)
  res
}

.create_group_folds <- function(data, col_grp, v = 1, ...) {
  split_objs <- rsample::group_vfold_splits(data = data, group = col_grp)
}
