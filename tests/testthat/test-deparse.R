context("deparse()")

library(tibble)
library(dplyr)

test_that("deparse handles lists appropriately", {
  check_deparse_identical(list(LETTERS))
  check_deparse_identical(list(x = 1:4))
  check_deparse_identical(list(list(list(1:4), y = 1:5)))

  my_list <- list(x = 1:4)
  attr(my_list, "my_attr") <- "test"

  check_deparse_identical(my_list)

  my_classed_list <- my_list
  class(my_classed_list) <- "test_class"

  check_deparse_identical(my_classed_list)
})

test_that("deparse handles basic types", {
  check_deparse_identical(1:5)
  check_deparse_identical(LETTERS)
  check_deparse_identical(c(TRUE, FALSE))
  check_deparse_identical(NULL)
})

test_that("deparse handles dates", {
  check_deparse_identical(Sys.Date())
  check_deparse_identical(as.POSIXct("2003-04-05 06:07:08 UTC"))
  check_deparse_identical(as.POSIXlt("2003-04-05 06:07:08 UTC"))
})

test_that("deparse handles functions", {
  check_deparse_equal(function(x) x + 1)
  check_deparse_equal(mean)
  expect_output(print(deparsec(function(x) x + 1)), "function \\(x\\) x \\+ 1")
  expect_output(print(deparsec(function(x) x + 1), useSource = FALSE), "srcref_call")
})

test_that("deparse handles factors", {
  check_deparse_identical(factor(1:5))
  check_deparse_identical(factor(1:5, levels = c(3:1)))
})

test_that("deparse handles data.frames", {
  check_deparse_identical(data.frame(x = 1:5, y = 4, z = LETTERS[1:5]))
  check_deparse_identical(
    data.frame(
      x = 1:5, y = 4, z = LETTERS[1:5], row.names = 6:10
      )
  )
  check_deparse_identical(tibble(x = 1:5, y = 4, z = LETTERS[1:5]))

  # Check as_tribble works ok
  check_deparse_identical(tibble(x = 1:5, y = 4, z = LETTERS[1:5]), as_tribble = TRUE)
  check_deparse_identical(tibble(x = 1:3, y = list(4:6, 7:9, 10:15)))

  # Check as_tribble works ok for more complex types
  check_deparse_identical(tibble(
    x = as.Date(c("2013-01-02", "2014-02-03")),
    y = factor(c("A", "B"), levels = c("B", "A"))
  ), as_tribble = TRUE)

  # Check as_tibble warns appropriately for row.names
  expect_warning(
    deparse(data.frame(x = 1, row.names = "A"), as_tibble = TRUE),
    "row\\.names are not supported by `tibble`")
})
