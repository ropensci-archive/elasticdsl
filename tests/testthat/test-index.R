context("index.R")


test_that("index() returns the right class", {
  # Using the ES_IP env var to pass in the
  # cluster's ip.
  expect_equal(class(index("gbif")), "index")
})

test_that("indices returns at least one index", {
  expect_gt(length(indices()), 0)
})
