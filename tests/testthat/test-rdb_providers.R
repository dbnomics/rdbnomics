context("Right arguments for rdb_providers")
library(rdbnomics)

test_that("code is logical and of length one", {
  expect_error(rdb_providers(code = logical()))
  expect_error(rdb_providers(code = "TRUE"))
})
