

library(testthat)
library(opm)
context("Testing the constants of the opmDB package")


## MEASUREMENT_COLUMN_MAP
test_that("mapping of measurement columns is consistent", {
  expect_false(is.null(names(MEASUREMENT_COLUMN_MAP)))
  expect_true(all(
    names(MEASUREMENT_COLUMN_MAP) %in% param_names("reserved.md.names")))
})

