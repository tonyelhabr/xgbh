
#' @noRd
.get_dir_data <- function() {
  getOption('xgbh.dir_data')
}

#' Directory for package-stored data
#'
#' Where to retrieve package-stored data
#' @export
get_dir_data <- .get_dir_data

#' @noRd
.get_seed <- function() {
  getOption('xgbh.seed')
}

#' @noRd
.get_verbose <- function() {
  getOption('xgbh.verbose')
}

#' @noRd
`%||%` <- function (x, y) {
  if(is.null(x)) {
    y
  } else {
    x
  }
}
