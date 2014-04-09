

library(testthat)
context("Testing the conversion functions of the validate package")


################################################################################


## as
test_that("conversion of ATOMIC_VALIDATOR works", {
  x <- new("ATOMIC_VALIDATOR")
  got <- as(x, "ATOMIC_VALIDATION")
  expect_is(got, "ATOMIC_VALIDATION")
  expect_true(nzchar(got@error))
  expect_true(is.na(got@result))
  expect_equal(x, as(got, "ATOMIC_VALIDATOR"))
})


## as
test_that("conversion of ATOMIC_VALIDATORS works", {
  x <- new("ATOMIC_VALIDATORS")

  got <- as(x, "ATOMIC_VALIDATIONS")
  expect_is(got, "ATOMIC_VALIDATIONS")
  validObject(got)
  back <- as(got, "ATOMIC_VALIDATORS")
  validObject(back)
  expect_equal(x, back)

  got <- as(x, "ELEMENT_VALIDATOR")
  expect_is(got, "ELEMENT_VALIDATOR")
  validObject(got)
  back <- as(got, "ATOMIC_VALIDATORS")
  validObject(back)
  expect_equal(x, back)
})


## as
test_that("conversion of ELEMENT_VALIDATION works", {
  x <- new("ELEMENT_VALIDATION")

  got <- as(x, "ATOMIC_VALIDATIONS")
  expect_is(got, "ATOMIC_VALIDATIONS")
  validObject(got)
  back <- as(got, "ELEMENT_VALIDATION")
  validObject(back)
  expect_equal(x, back)

  got <- as(x, "ELEMENT_VALIDATOR")
  expect_is(got, "ELEMENT_VALIDATOR")
  validObject(got)
  back <- as(got, "ELEMENT_VALIDATION")
  validObject(back)
  expect_equal(x, back)
})


## as
test_that("conversion of MAP_VALIDATOR works", {
  x <- new("MAP_VALIDATOR")
  got <- as(x, "MAP_VALIDATION")
  expect_is(got, "MAP_VALIDATION")
  validObject(got)
  back <- as(got, "MAP_VALIDATOR")
  validObject(back)
  expect_equal(x, back)
})


################################################################################


