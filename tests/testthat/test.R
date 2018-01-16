
library(boulder)
context("API")

test_that("API produces correct structure", {
  expect_equal(class(get_databases()), "tai_api")
  expect_equal(names(get_databases()), c("content", "path", "response"))
  expect_equal(names(get_databases()$content), c("dbid", "text"))
  expect_equal(class(get_nodes("01Rahvastik")), "tai_api")
  expect_equal(names(get_nodes("01Rahvastik")), c("content", "path", "response"))
  expect_error(get_nodes(""), "Empty string")
})



