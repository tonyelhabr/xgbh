#' @noRd
#' @export
.path_x <- function(dir, file = tempfile(), ext = NULL) {
  if(!is.null(ext)) {
    ext <- sprintf('.%s', ext)
  } else {
    ext <- ''
  }
  file.path(dir, sprintf('%s%s', file, ext))
}

#' @noRd
#' @export
.generate_path <- function(path = NULL, dir, file, ext) {
  if(!is.null(path)) {
    return(path)
  }
  .path_x(dir = dir, file = file, ext = ext)
}

#' @noRd
#' @export
.generate_path_data <- function(dir = getwd(), ...) {
  .generate_path(dir = dir, ...)
}
