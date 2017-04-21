check_deparse_identical <- function(x, ...) {
  expect_identical(x, eval(deparsec(x, ...)))
}
