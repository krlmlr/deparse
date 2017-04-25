#' @export
deparse.factor <- function(x, ...) {
  fac_items <- as.character(x)
  fac_items_dp <- deparse(fac_items)
  if (!identical(levels(x), levels(factor(fac_items)))) {
    fac_items_dp <- sprintf("%s, levels = %s", fac_items_dp, deparse(levels(x)))
  }
  if (is.ordered(x)) {
    func_name <- "ordered"
  } else {
    func_name <- "factor"
  }
  sprintf("%s(%s)", func_name, fac_items_dp)
}
