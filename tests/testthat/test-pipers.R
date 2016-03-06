context("pipers.R")

test_that("pipline_info()", {
  expect_named(elasticdsl:::pipeline_info(), c("is_piped", "env"))
  expect_is(index("shakespeare") %>% { elasticdsl:::pipeline_info() }, "list")
  expect_true(index("shakespeare") %>% { elasticdsl:::pipeline_info() } %>% .$is_piped)
  expect_false((elasticdsl:::pipeline_info())$is_piped)
})
