#' A nicer deparse
#'
#' @description
#' \code{dp} is a reimplementation of \code{\link[base]{dput}} and related
#' functions. It tries its best to produce output that is easy to read
#' (for humans), yet produces (almost) identical results to the input
#' (for machines).
#'
#' Unlike \code{\link[base]{dput}} and \code{\link[base]{deparse}}, it creates
#' a \code{call} object which can be evaluated.  Because the formatting may
#' be important, it is retained as an attribute.
#'
#' @export
dp <- function(x, ...) UseMethod("dp")

#' @export
dp.default <- function(x, ...) {
  paste(deparse(x, 500L), collapse = "")
}
