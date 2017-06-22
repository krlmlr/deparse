context("output")

test_that("known output", {
  expect_output_file(cat(deparse(iris, as_tibble = TRUE), sep = "\n"), "out/iris-tibble.txt", update = TRUE)
  expect_output_file(cat(deparse(iris, as_tribble = TRUE), sep = "\n"), "out/iris-tribble.txt", update = TRUE)
})
