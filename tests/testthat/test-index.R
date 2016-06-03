context("index.R")

invisible(elastic::connect())

first_available_index <- indices()[[1]]


test_that("index() returns the right class", {
  expect_equal(class(index(first_available_index)), "index")
})

test_that("indices returns at least one index", {
  expect_gt(length(indices()), 0)
})

test_that("get_map", {
  expect_is(names(elasticdsl:::get_map(first_available_index)), "character")
})
