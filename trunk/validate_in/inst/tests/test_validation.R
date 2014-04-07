
library(testthat)
context("Testing the validation functions of the validate package")


################################################################################


validate_atomic <- function(x, how, what) {
  validate(x, new("ATOMIC_VALIDATOR", how = how, what = what))
}

expect_T <- function(actual) {
  expect_true(as(actual, "logical"))
}

expect_F <- function(actual) {
  expect_false(as(actual, "logical"))
}

expect_NA <- function(actual) {
  expect_equivalent(as(actual, "logical"), NA)
}


################################################################################


## validate
test_that("character vector lengths can be validated", {
  got <- validate_atomic(letters, "min_elems", 1)
  expect_T(got)
  got <- validate_atomic(letters, "max_elems", 1)
  expect_F(got)
  got <- validate_atomic(letters, "max_elems", 26)
  expect_T(got)
})


## validate
test_that("character vector bounds can be validated", {
  got <- validate_atomic(letters, "lower_bound", "a")
  expect_T(got)
  got <- validate_atomic(letters, "lower_bound", "b")
  expect_F(got)
  got <- validate_atomic(letters, "upper_bound", "z")
  expect_T(got)
  got <- validate_atomic(letters, "upper_bound", "y")
  expect_F(got)
})


## validate
test_that("character vector types can be validated", {
  got <- validate_atomic(letters, "type", "character")
  expect_T(got)
  got <- validate_atomic(letters, "type", c("logical", "character"))
  expect_T(got)
  got <- validate_atomic(letters, "type", c("logical", "integer"))
  expect_F(got)
})


## validate
test_that("character vector character numbers can be validated", {
  got <- validate_atomic(letters, "max_chars", 1)
  expect_T(got)
  got <- validate_atomic(letters, "min_chars", 1)
  expect_T(got)
  got <- validate_atomic(letters, "min_chars", 2)
  expect_F(got)
})


## validate
test_that("character vector pattern matches can be validated", {
  got <- validate_atomic(letters, "pattern", c("^[a-z]$", "^\\w$"))
  expect_T(got)
  got <- validate_atomic(letters, "pattern", c("^[a-z]$", "^\\W$"))
  expect_F(got)
})


## validate
test_that("character vector controlled vocabularies can be validated", {
  got <- validate_atomic(letters, "enum", letters)
  expect_T(got)
  got <- validate_atomic(letters, "enum", letters[-1L])
  expect_F(got)
})


## validate
test_that("character vector sorted- and uniqueness can be validated", {
  got <- validate_atomic(letters, "sorted", TRUE)
  expect_T(got)
  got <- validate_atomic(letters, "sorted", FALSE)
  expect_F(got)
  got <- validate_atomic(letters, "unique", TRUE)
  expect_T(got)
  got <- validate_atomic(letters, "unique", FALSE)
  expect_F(got)
})


## validate
test_that("character vector validation responds to junk input", {
  got <- validate_atomic(letters, "bozo", TRUE)
  expect_NA(got)
  got <- validate_atomic(letters, "sorted", list(a = 99, c = -77:100))
  expect_NA(got)
  got <- validate_atomic(letters, "unique", letters)
  expect_NA(got)
  got <- validate_atomic(letters, "FALSE", list(k = letters, j = 1:10))
  expect_NA(got)
})


################################################################################

