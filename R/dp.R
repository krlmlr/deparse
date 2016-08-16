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

#' @export
dp.function <- function(x, ...) {
  fun_in_namespace <- find_function_in_namespace(x)
  if (is.null(fun_in_namespace))
    NextMethod()
  else {
    paste0(dp(as.name(fun_in_namespace$ns)), "::", dp(as.name(fun_in_namespace$fun)))
  }
}

find_function_in_namespace <- function(fun) {
  env <- environment(fun)
  if (!isNamespace(env))
    return(NULL)

  namespace_funs <- as.list(env)
  namespace_funs <- namespace_funs[order(names(namespace_funs))]

  same <- vapply(namespace_funs, identical, fun, FUN.VALUE = logical(1L))
  same_name <- names(which(same))
  if (length(same_name) == 0L)
    return(NULL)

  list(ns = getNamespaceName(env), fun = same_name[[1L]])
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
