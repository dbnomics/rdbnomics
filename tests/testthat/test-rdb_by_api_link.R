context("Right api_link and use_readLines")
library(rdbnomics)

test_that("api_link is NULL", {
  expect_identical(rdb_by_api_link(NULL), NULL)
})

test_that("api_link length is greater than one or equal to zero", {
  expect_error(rdb_by_api_link(c("url1", "url2")))
  expect_error(rdb_by_api_link(character()))
})

test_that("api_link is not a character string", {
  expect_error(rdb_by_api_link(0))
})

test_that("use_readLines is NULL", {
  expect_error(rdb_by_api_link("url", NULL))
})

test_that("use_readLines length is greater than one or equal to zero", {
  expect_error(rdb_by_api_link("url", c(TRUE, FALSE)))
  expect_error(rdb_by_api_link("url", logical()))
})

test_that("use_readLines is not a logical", {
  expect_error(rdb_by_api_link("url", "TRUE"))
})
