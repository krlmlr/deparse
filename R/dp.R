#' A nicer deparse
#'
#' \code{dp} is a reimplementation of \code{\link[base]{dput}} and related
#' functions. It tries its best to produce output that is easy to read
#' (for humans), yet produces (almost) identical results to the input
#' (for machines). This function is a generic, so other packages can easily
#' provide implementations for the objects they define.
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
  paste0(call, "(", dp(argument), ")")
}

#' @rdname dp
#'
#' @description
#' The \code{dpc} function leverages \code{dp} by creating
#' a \code{call} object which can be evaluated but retains formatting
#' (in the form of a \code{\link[base]{srcref}} attribute).
#' @export
dpc <- function(x, ...) {
  text <- dp(x, ...)
  as.srcref_call(srcfilecopy("<dpc>", text))
}
