#' Calls with attached srcrefs
#'
#' As of R 2.10.0, \code{\link[base]{call}} objects don't use a
#' \code{\link[base]{srcref}} attribute
#' even if it's attached, see \code{vignette("calls", package = "dp")} for
#' details. This class provides call objects that use their \code{srcref}
#' attribute for printing.
srcref_call <- function(call, srcref) {
  stop("NYI")
}
