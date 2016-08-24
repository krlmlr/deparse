#' Calls with attached srcrefs
#'
#' As of R 2.10.0, \code{\link[base]{call}} objects don't use a
#' \code{\link[base]{srcref}} attribute
#' even if it's attached, see \code{vignette("calls", package = "lave")} for
#' details. This class provides call objects that use their \code{srcref}
#' attribute for printing.
srcref_call <- function(call, srcref) {
  stop("NYI")
}

#' @rdname srcref_call
#' @export
as.srcref_call <- function(x, ...) UseMethod("as.srcref_call")

#' @export
as.srcref_call.srcfile <- function(x, ...) {
  ex <- parse_srcfile(x)
  cl <- ex[[1L]]
  class(cl) <- "srcref_call"
  attr(cl, "srcref") <- attr(ex, "srcref")[[1]]
  cl
}

#' @export
print.srcref_call <- function(x, ..., useSource = TRUE) {
  if (!useSource || is.null(srcref <- getSrcref(x)))
    NextMethod()
  else
    print(srcref, ...)
  invisible(x)
}

parse_srcfile <- function(x) {
  parse(text = read_srcfile(x), keep.source = TRUE, srcfile = x)
}

read_srcfile <- function(x) {
  # Can't pass conn to parse() for some reason, results are garbled
  conn <- open(x, 1L)
  on.exit(close(conn), add = TRUE)
  readLines(conn)
}
