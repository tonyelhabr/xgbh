
#' @seealso \url{https://r-pkgs.org/r.html?q=onLoad#when-you-do-need-side-effects}
.onLoad <- function(libname, pkgname) {
  op <- options()
  op.xgbh <- list(
    xgbh.dir_data = file.path('inst', 'extdata'),
    engagement.seed = 42L,
    xgbh.verbose = TRUE
  )
  toset <- !(names(op.xgbh) %in% names(op))
  if(any(toset)) options(op.xgbh[toset])

  invisible()
}
