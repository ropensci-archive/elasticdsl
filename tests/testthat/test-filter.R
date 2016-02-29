context("filter.R")


test_that("functions return the correct classes", {
  expect_true("filtered" %in% class(filter(NULL)))
})

