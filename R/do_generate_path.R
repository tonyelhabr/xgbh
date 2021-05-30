
#' @noRd
.do_generate_path <- function(name, .path, .dir, .file, .ext, dir, file, ext, sep, suffix = NULL) {

  nms <- names(.path)
  assertthat::assert_that(any(name == nms), msg = glue::glue('INTERNAL: `name` not in `names(.path)` ({nms}).'))
  if(!is.null(suffix)) {
    suffix <- sprintf('%s%s', sep, suffix)
  } else {
    suffix <- ''
  }

  .path[[name]] %||%
    .generate_path(
      dir = .dir[[name]] %||% dir,
      file = sprintf('%s%s', .file[[name]] %||% file, suffix),
      ext = .ext[[name]] %||% ext
    )
}
