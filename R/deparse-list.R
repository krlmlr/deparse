#' @export
deparse.list <- function(x, ...) {
  my_attrs <- attributes(x)
  if (any(!(names(my_attrs) %in% c("names", "class"))) ||
      !identical(class(x), "list")) {
    base::deparse(x, ...)
  } else {
    create_list_item <- function(name, item) {
      if (name != "") {
        name <- paste0(name, " = ")
      }
      paste0(name, deparse(item))
    }
    if (is.null(names(x))) {
      list_names <- character(length(x))
    } else {
      list_names <- names(x)
    }
    output <- map2_chr(list_names, x, create_list_item)
    paste0(
      "list(",
      paste(output, collapse = ", "),
      ")"
    )
  }
}
