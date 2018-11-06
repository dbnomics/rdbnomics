context("Right combination of arguments for rdb")
library(rdbnomics)

test_that("Fetch by dimensions", {    
  expect_error(rdb(dimensions = "x", provider_code = NULL, dataset_code = "y"))
  expect_error(rdb(dimensions = "x", provider_code = "y", dataset_code = NULL))
  expect_error(rdb(dimensions = "x", provider_code = NULL, dataset_code = NULL))
  expect_error(rdb(dimensions = c("x1", "x2"), provider_code = "y", dataset_code = "z"))
})

test_that("Fetch by mask", {
  expect_error(rdb(mask = "IMF", provider_code = NULL, dataset_code = "y"))
  expect_error(rdb(mask = "IMF", provider_code = "y", dataset_code = NULL))
  expect_error(rdb(mask = "IMF", provider_code = NULL, dataset_code = NULL))
  expect_error(rdb(mask = c("x1", "x2"), provider_code = "y", dataset_code = "z"))
})

test_that("Fetch by mask with wrong provider_code", {
  expect_error(rdb(mask = "x", provider_code = "y", dataset_code = "z"))
})

test_that("Fetch by ids", {
  expect_error(rdb(ids = character()))
})

test_that("Nothing provided", {
  expect_error(rdb())
})
