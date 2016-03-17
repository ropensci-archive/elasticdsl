context("filter.R")

first_available_index <- indices()[[1]]

test_that("filter", {
  expect_true("filtered" %in% class(filter(NULL)))
})

test_that("ids", {
  expect_true("comb" %in% class(ids_(c(1, 2))))
  expect_true("ids" %in% class(ids_(c(1, 2))[[1]]))
})

test_that("ids_", {
  expect_true("comb" %in% class(ids_(c(1, 2))))
  expect_true("ids" %in% class(ids_(c(1, 2))[[1]]))
})

test_that("ids", {
  expect_equal(
    index("shakespeare") %>%
      filter() %>%
      ids(c(1, 2)) %>%
      exec() %>%
      hits() %>%
      sapply(., FUN = function(i) { i$`_id` }) %>%
      as.integer() %>%
      sort.default(),
    c(1, 2))
})

test_that("operands", {
  expect_equal(index("shakespeare") %>% and() %>% attr("operand"), "and")
  expect_equal(index("shakespeare") %>% or() %>% attr("operand"), "or")
  expect_equal(index("shakespeare") %>% not() %>% attr("operand"), "not")
})

test_that("exec", {
  expect_true(
    index("shakespeare") %>%
      filter() %>%
      prefix(speaker = "KING H") %>%
      exec() %>%
      hits() %>%
      sapply(., FUN = function(i) { i$`_source`$speaker }) %>%
      grepl("KING HENRY IV", .) %>%
      all()
  )
})

