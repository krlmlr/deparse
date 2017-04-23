check_deparse_identical <- function(x, ...) {
  expect_identical(x, eval(deparsec(x, ...)))
}

check_deparse_equal <- function(x, ...) {
  expect_equal(x, eval(deparsec(x, ...)))
}
