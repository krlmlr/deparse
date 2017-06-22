#' A nicer deparse
#'
#' \code{deparse} is a reimplementation of \code{\link[base]{dput}} and related
#' functions. It tries its best to produce output that is easy to read
#' (for humans), yet produces (almost) identical results to the input
#' (for machines). This function is a generic, so other packages can easily
#' provide implementations for the objects they define.
#'
#' @param x object to deparse
#' @param ... passed to other methods
#'
#' @import rlang
#' @export
deparse <- function(x, ...) {
  UseMethod("deparse")
}

#' @export
deparse.default <- function(x, ...) {
  if (is.list(x)) {
    deparse.list(x, ...)
  } else {
    paste(base::deparse(x, 500L, backtick = TRUE), collapse = "")
  }
}

#' @export
deparse.Date <- function(x, ...) {
  deparse_call("as.Date", format(x))
}

#' @export
deparse.POSIXct <- function(x, ...) {
  deparse_call("as.POSIXct", format(x, usetz = TRUE))
}

#' @export
deparse.POSIXlt <- function(x, ...) {
  deparse_call("as.POSIXlt", format(x, usetz = TRUE))
}

deparse_call <- function(call, argument) {
  paste0(call, "(", deparse(argument), ")")
}

#' @export
deparse.function <- function(x, ...) {
  fun_in_namespace <- find_function_in_namespace(x)
  if (is.null(fun_in_namespace))
    NextMethod()
  else {
    paste0(deparse(as.name(fun_in_namespace$ns)), "::", deparse(as.name(fun_in_namespace$fun)))
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

#' @rdname deparse
#'
#' @description
#' The \code{deparsec} function leverages \code{deparse} by creating
#' a \code{call} object which can be evaluated but retains formatting
#' (in the form of a \code{\link[base]{srcref}} attribute).
#' @export
deparsec <- function(x, ...) {
  text <- deparse(x, ...)
  as.srcref_call(srcfilecopy("<deparsec>", text))
}
