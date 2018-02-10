
library(boulder)
context("API get functions")

context("get_all_tables")

testthat::test_that("Table with data table names has correct format", {
  expect_is(vars_et, "data.frame")
  expect_is(vars_en, "data.frame")
  expect_is(get_all_tables(), "data.frame")
  expect_is(get_all_tables(), "tai_tables")
  expect_is(get_all_tables(local = FALSE), "data.frame")
})


