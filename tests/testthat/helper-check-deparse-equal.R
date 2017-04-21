check_deparse_equal <- function(x, ...) {
  expect_equal(x, eval(deparsec(x, ...)))
}
