#' @export
deparse.data.frame <-
  function(x, as_tibble = FALSE, as_tribble = FALSE, generate_mutate = TRUE, ...) {

  if (as_tribble) {
    return(deparse_tribble(x, generate_mutate, ...))
  }

  col_names <- vapply(names(x), function(nm) deparse(as.name(nm)), character(1))
  columns <- sprintf("%s = %s", col_names, vapply(x, deparse, character(1)))

  if (any(row.names(x) != as.character(seq_len(nrow(x))))) {
    if (as_tibble) {
      warn("row.names are not supported by `tibble`")
    } else {
      columns <- c(columns, sprintf("row.names = %s",
                                    deparse(attr(x, "row.names"))))
    }
  }

  if (as_tibble) {
    sprintf("tibble(%s)",
            paste(columns, collapse = ", "))
  } else {
    sprintf("data.frame(%s, stringsAsFactors = FALSE, check.names = FALSE)",
            paste(columns, collapse = ", "))
  }
}

#' @export
deparse.tbl_df <- function(x, as_tibble = TRUE, as_tribble = FALSE, ...) {
  deparse.data.frame(x = x, as_tibble = as_tibble, as_tribble = as_tribble, ...)
}

deparse_tribble <- function(x, generate_mutate, ...) {
  col_names <- names(x)

  row_item_calls <- list(quote(`:`), quote(c), quote(list))

  # Finds an appropriate vector wrapped in function calls and replaces the
  # vector with the column name
  # Returns NULL if there is no matching vector
  find_and_replace_c <- function(cur_call, col_name, n_rows) {
    if ((!is.call(cur_call) && n_rows == 1) ||
        (is.call(cur_call) && some(row_item_calls, identical, cur_call[[1L]]) &&
        length(eval(cur_call)) == n_rows)) {
      return(list(col_data = cur_call, call = as.symbol(col_name)))
    }
    if (is.call(cur_call) && length(cur_call) > 1L) {
      res <- find_and_replace_c(cur_call[[2L]], col_name, n_rows)
      if (!is.null(res)) {
        cur_call[[2L]] <- res$call
        return(list(col_data = res$col_data, call = cur_call))
      } else {
        return(NULL)
      }
    }
    return(NULL)
  }

  generate_column_calls <- function(column, col_name) {
    col_dp <- deparsec(column)
    col_call <- NULL
    if (is.call(col_dp)) {
      if (!some(row_item_calls, identical, col_dp[[1L]]) &&
          length(col_dp) > 1L && !identical(col_dp[[1L]], quote(list))) {
        res <- find_and_replace_c(col_dp[[2L]], col_name, nrow(x))
        if (!is.null(res)) {
          col_call <- col_dp
          col_call[[2L]] <- res$call
          column <- eval(res$col_data)
        }
      }
    }
    return(list(col_data = column, col_call = col_call))
  }

  col_calls <- list()

  output_data <- character(nrow(x) * ncol(x))
  dim(output_data) <- dim(x)

  for (i in seq_along(x)) {
      res <- generate_column_calls(x[[i]], col_names[i])
      if (generate_mutate) {
        output_data[, i] <- map_chr(res$col_data, deparse, ...)
      } else {
        output_data[, i] <- map_chr(x[[i]], deparse, ...)
      }
      if (!is.null(res$col_call)) {
        col_calls <- c(
          col_calls,
          stats::setNames(list(deparse(res$col_call)), col_names[i])
        )
      }
  }

  syntactic_name <- function(x) {
    base::deparse(as.symbol(x), backtick = TRUE)
  }
  output_col_names <- paste0(
    "~",
    map_chr(col_names, syntactic_name)
  )

  output_collapsed <- map_chr(
    seq_len(nrow(x)),
    function(i) paste(output_data[i, ], collapse = ", ")
  )

  output_final <- paste0(
    "tribble(\n  ",
    paste(
      c(paste(output_col_names, collapse = ", "), output_collapsed),
      collapse = ",\n  "
      ),
    "\n  )"
    )

  if (length(col_calls) > 0L) {
    if (generate_mutate) {
      output_final <- paste0(
        output_final,
        " %>% \n",
        "  mutate(\n",
        paste(
          sprintf("    %s = %s", names(col_calls), col_calls),
          collapse = ",\n"
          ),
        "\n  )"
      )
    } else {
      warning("Without `generate_mutate`, deparsed code may not function correctly on types such as factors")
    }
  }
  output_final
}
