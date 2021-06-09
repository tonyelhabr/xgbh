

#' @noRd
.do_generate_path <-
  function(name,
           .path = NULL,
           .dir = NULL,
           .file = NULL,
           .ext = NULL,
           dir = NULL,
           file = NULL,
           ext = NULL,
           sep = '_',
           suffix = NULL,
           prefix = NULL) {
    # browser()
    nms <- names(.path)
    assertthat::assert_that(any(name == nms), msg = glue::glue('INTERNAL: `name` not in `names(.path)` ({nms}).'))

    if (!is.null(prefix)) {
      prefix <- sprintf('%s%s', prefix, sep)
    } else {
      prefix <- ''
    }


    if (!is.null(suffix)) {
      suffix <- sprintf('%s%s', sep, suffix)
    } else {
      suffix <- ''
    }

    .path[[name]] %||%
      .generate_path(
        dir = .dir[[name]] %||% dir,
        file = sprintf('%s%s%s', prefix, .file[[name]] %||% file, suffix),
        ext = .ext[[name]] %||% ext
      )
  }
