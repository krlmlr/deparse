deparse.data.frame <- function(x, as_tibble = FALSE, as_tribble = FALSE, ...) {
  if (as_tribble) {
    return(deparse_tribble(x, ...))
  }

  col_names <- vapply(names(x), function(nm) deparse(as.name(nm)), character(1))
  columns <- sprintf("%s = %s", col_names, vapply(x, deparse, character(1)))

  if (any(row.names(x) != as.character(seq_len(nrow(x))))) {
    columns <- c(columns, sprintf("row.names = c(%s)",
                                  paste(sprintf("\"%s\"", row.names(x)), collapse = ", ")))
  }

  if (as_tibble) {
    sprintf("tibble(%s)",
            paste(columns, collapse = ", "))
  } else {
    sprintf("data.frame(%s, stringsAsFactors = FALSE, check.names = FALSE)",
            paste(columns, collapse = ", "))
  }
}

deparse.tbl_df <- function(x, as_tibble = TRUE, as_tribble = FALSE, ...) {
  deparse.data.frame(x = x, as_tibble = as_tibble, ...)
}

deparse_tribble <- function(x, ...) {
  col_names <- names(x)

}
