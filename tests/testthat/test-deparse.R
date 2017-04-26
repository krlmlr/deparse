context("deparse()")

test_that("deparse handles lists appropriately", {
  check_deparse_equal(list(LETTERS))
  check_deparse_equal(list(x = 1:4))
  check_deparse_equal(list(list(list(1:4), y = 1:5)))
})

test_that("deparse handles basic types", {
  check_deparse_equal(1:5)
  check_deparse_equal(LETTERS)
  check_deparse_equal(c(TRUE, FALSE))
  check_deparse_equal(NULL)
})
