context("deparse()")

library(tibble)

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

test_that("deparse handles factors", {
  check_deparse_equal(factor(1:5))
  check_deparse_equal(factor(1:5, levels = c(3:1)))
})

test_that("deparse handles data.frames", {
  check_deparse_equal(data.frame(x = 1:5, y = 4, z = LETTERS[1:5]))
  check_deparse_equal(tibble(x = 1:5, y = 4, z = LETTERS[1:5]))

  # Check as_tribble works ok
  check_deparse_equal(tibble(x = 1:5, y = 4, z = LETTERS[1:5]), as_tribble = TRUE)
})
