#' @export
deparse.list <- function(x, ...) {
  my_attrs <- attributes(x)
  create_list_item <- function(name, item) {
    if (name != "") {
      name <- paste0(name, " = ")
    }
    paste0(name, deparse(item, ...))
  }
  if (is.null(names(x))) {
    list_names <- character(length(x))
  } else {
    list_names <- names(x)
  }
  output <- map2_chr(list_names, x, create_list_item)
  output <- paste0(
    "list(",
    paste(output, collapse = ", "),
    ")"
  )
  wrap_structure(x, output, "list")
}

wrap_structure <- function(x, deparsed, current_class, exclude_attrs = NULL) {
  x_attrs <- attributes(x)
  exclude_attrs <- union(c("names", "class"), exclude_attrs)
  if (!identical(class(x), current_class)) {
    add_class <- sprintf(", class = %s", deparse(class(x)))
  } else {
    add_class <- ""
  }
  add_attr_names <- setdiff(names(x_attrs), exclude_attrs)
  add_attr_labels <- stats::setNames(add_attr_names, add_attr_names)
  to_replace <- match(c("dim", "dimnames", "tsp", "levels"), add_attr_names)
  if (any(!is.na(to_replace))) {
    add_attr_labels[na.omit(to_replace)] <-
      c(".Dim", ".Dimnames", ".Tsp", ".Label")[!is.na(to_replace)]
  }
  make_attr_text <- function(attrib) {
    sprintf("%s = %s", add_attr_labels[[attrib]], deparse(x_attrs[[attrib]]))
  }
  add_attrs <- paste(map_chr(add_attr_names, make_attr_text),
                     collapse = ", ")
  if (add_attrs != "") {
    add_attrs <- paste0(", ", add_attrs)
  }
  if (add_attrs != "" || add_class != "") {
    sprintf("structure(%s%s%s)", deparsed, add_class, add_attrs)
  } else {
    deparsed
  }
}
