#' A nicer deparse
#'
#' @description
#' \code{dp} is a reimplementation of \code{\link[base]{dput}} and related
#' functions. It tries its best to produce output that is easy to read
#' (for humans), yet produces (almost) identical results to the input
#' (for machines). This function is a generic, so other packages can easily
#' provide implementations for the objects they define.
#'
#' Unlike \code{\link[base]{dput}} and \code{\link[base]{deparse}}, it creates
#' a \code{call} object which can be evaluated.  Because the formatting may
#' be important, it is retained as an attribute.
#'
#' @export
dp <- function(x, ...) UseMethod("dp")

#' @export
dp.default <- function(x, ...) {
  paste(deparse(x, 500L, backtick = TRUE), collapse = "")
}

#' @export
dp.Date <- function(x, ...) {
  dp_call("as.Date", format(x))
}

#' @export
dp.POSIXct <- function(x, ...) {
  dp_call("as.POSIXct", format(x, usetz = TRUE))
}

#' @export
dp.POSIXlt <- function(x, ...) {
  dp_call("as.POSIXlt", format(x, usetz = TRUE))
}

dp_call <- function(call, argument) {
  paste0(call, "(\"", dp(argument), "\")")
}
