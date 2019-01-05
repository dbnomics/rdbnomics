context("Right arguments for rdb_last_updates")
library(rdbnomics)

test_that("all is logical and of length one", {    
  expect_error(rdb_last_updates(all = logical()))
  expect_error(rdb_last_updates(all = "TRUE"))
})
