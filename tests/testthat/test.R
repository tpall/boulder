
library(boulder)
context("API")

test_that("API produces correct structure", {
  expect_is(get_databases(), "tai_api")
  expect_equal(names(get_databases()), c("content", "path", "response"))
  expect_equal(names(get_databases()$content), c("dbid", "text"))
  expect_is(get_databases()$content, "data.frame")
  expect_is(get_nodes("01Rahvastik"), "tai_api")
  expect_equal(names(get_nodes("01Rahvastik")), c("content", "path", "response"))
  expect_equal(names(get_nodes("01Rahvastik")$content), c("id", "type", "text"))
  expect_is(get_nodes("01Rahvastik")$content, "data.frame")
  expect_error(get_nodes(""), "Empty string")
  expect_error(get_tables("01Rahvastik", "", ""), "Empty string")
  expect_error(get_tables("01Rahvastik", "02Synnid", ""), "Empty string")
  expect_error(get_tables("01Rahvastik", "", "SR01"), "Empty string")
  expect_equal(names(get_tables("01Rahvastik", "02Synnid", "SR01")), c("content", "path", "response"))
  expect_is(get_tables("01Rahvastik", "02Synnid", "SR01"), "tai_api")
  expect_equal(names(get_tables("01Rahvastik", "02Synnid", "SR01")$content), c("title", "variables"))
  expect_is(get_tables("01Rahvastik", "02Synnid", "SR01")$content$variables, "data.frame")
})


context("Bundled data")

testthat::test_that("Table with data table names has correct format", {
  expect_is(vars_et, "data.frame")
  expect_is(vars_en, "data.frame")
})


