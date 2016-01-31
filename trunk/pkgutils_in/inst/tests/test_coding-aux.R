library(testthat)
context("Testing the coding helper functions of the pkgutils package")


################################################################################


## prepare_class_names
## UNTESTED


################################################################################


## adist2map
test_that("we can create maps from string similarities", {
  x <- c("fibroblast", "fibroblast", "oth", letters[13:20], "FIB", "", " ",
    "fibroblasts", "fib", "control", "CONT", "other", "other", "FIBRO", "fib ",
    NA_character_, "fibroblasts ")
  got <- adist2map(x)
  exp <- c(FIB = "fib", fibroblasts = "fibroblast",
    `fibroblasts ` = "fibroblast")
  expect_equal(got, exp)
  got <- adist2map(x, partial = TRUE)
  exp <- c(FIB = "fib", fibroblasts = "fibroblasts ")
  expect_equal(got, exp)
  got <- adist2map(x, exclude = "FIB")
  exp <- c(fibroblasts = "fibroblast", `fibroblasts ` = "fibroblast")
  expect_equal(got, exp)
  got <- adist2map(character())
  expect_equal(got, structure(character(), names = character()))
})


