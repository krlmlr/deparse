#' A nicer deparse
#'
#' \code{lave} is a reimplementation of \code{\link[base]{dput}} and related
#' functions. It tries its best to produce output that is easy to read
#' (for humans), yet produces (almost) identical results to the input
#' (for machines). This function is a generic, so other packages can easily
#' provide implementations for the objects they define.
#'
#' @export
lave <- function(x, ...) UseMethod("lave")

#' @export
lave.default <- function(x, ...) {
  paste(deparse(x, 500L, backtick = TRUE), collapse = "")
}

#' @export
lave.Date <- function(x, ...) {
  lave_call("as.Date", format(x))
}

#' @export
lave.POSIXct <- function(x, ...) {
  lave_call("as.POSIXct", format(x, usetz = TRUE))
}

#' @export
lave.POSIXlt <- function(x, ...) {
  lave_call("as.POSIXlt", format(x, usetz = TRUE))
}

lave_call <- function(call, argument) {
  paste0(call, "(", lave(argument), ")")
}

#' @export
lave.function <- function(x, ...) {
  fun_in_namespace <- find_function_in_namespace(x)
  if (is.null(fun_in_namespace))
    NextMethod()
  else {
    paste0(lave(as.name(fun_in_namespace$ns)), "::", lave(as.name(fun_in_namespace$fun)))
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

#' @rdname lave
#'
#' @description
#' The \code{lavec} function leverages \code{lave} by creating
#' a \code{call} object which can be evaluated but retains formatting
#' (in the form of a \code{\link[base]{srcref}} attribute).
#' @export
lavec <- function(x, ...) {
  text <- lave(x, ...)
  as.srcref_call(srcfilecopy("<lavec>", text))
}
