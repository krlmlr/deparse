check_deparse_identical <- function(x, ...) {
  tibble <- tibble::tibble
  tribble <- tibble::tribble
  mutate <- dplyr::mutate
  expect_identical(x, eval(deparsec(x, ...)))
}

check_deparse_equal <- function(x, ...) {
  tibble <- tibble::tibble
  tribble <- tibble::tribble
  mutate <- dplyr::mutate
  expect_equal(x, eval(deparsec(x, ...)))
}
