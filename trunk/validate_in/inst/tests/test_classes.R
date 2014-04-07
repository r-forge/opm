
library(testthat)
context("Testing the classes of the validate package")


################################################################################


## ?
test_that("we cannot generate invalid ATOMIC_VALIDATOR objects", {
  expect_error(new("ATOMIC_VALIDATOR", what = letters, how = -3))
  expect_error(new("ATOMIC_VALIDATOR", what = -3, how = letters))
})

## ?
test_that("we cannot generate invalid ATOMIC_VALIDATORS objects", {
  expect_error(new("ATOMIC_VALIDATORS", checks = list(letters)))
  expect_error(new("ATOMIC_VALIDATORS", checks = list(4, 9)))
})

## ?
test_that("we cannot generate invalid ELEMENT_VALIDATOR objects", {
  expect_error(new("ELEMENT_VALIDATOR", checks = list(), required = NA))
  expect_error(new("ELEMENT_VALIDATOR", checks = list(),
    required = c(TRUE, FALSE)))
  expect_error(new("ELEMENT_VALIDATOR", checks = list(-3, 0)))
  expect_error(new("ELEMENT_VALIDATOR", required = 99))
})


################################################################################





