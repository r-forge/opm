library(testthat)
context("Testing the auxiliary coding of the pkgutils package")


################################################################################


## case
test_that("case works as expected", {
  expect_equal(case(0, "a", "b"), "a")
  expect_equal(case(1, "a", "b"), "b")
  expect_equal(case(10, "a", "b"), "b")
  expect_error(case(NA_real_, "a", "b"))
  expect_error(case(-1, "a", "b"))
  expect_equal(case("a", a = "a", b = "b"), "a")
  expect_equal(case("b", a = "a", b = "b"), "b")
  expect_error(case("c", a = "a", b = "b"))
  expect_error(case(NA_character_, a = "a", b = "b"))
})

## must
test_that("must works as expected", {
  expect_error(must(warning("abc")))
})

## L
## UNTESTED

## LL
test_that("LL works as expected", {
  x <- 3
  y <- 9:10
  z <- 'a'
  expect_equal(c("x", "z"), LL(x, z))
  expect_error(LL(x, y))
  expect_error(LL(x, y, .wanted = 2L))
  expect_error(LL(y, z, .wanted = 2L))
  expect_equal("y", LL(y, .wanted = 2L))
})

## listing
## UNTESTED

## flatten
## UNTESTED

## collect
## UNTESTED

