

library(testthat)
context("Testing the conversion functions of the pfn package")


################################################################################


## sanitize
test_that("new file names are suggested for non-portable ones", {

  tmpdir <- tempdir()

  # one subdirectory with acceptable name
  subdir <- file.path(tmpdir, c("abc", "a b c"))
  for (path in subdir)
    dir.create(path)

  files1 <- tempfile(c("x y z", "UVW", "i j k"), tmpdir)
  files2 <- tempfile(c("z26%", "#0987-b3", "_kiu_p"), subdir[[1L]])
  files3 <- tempfile(c("k$rtw!", "ffff", "+348576-39456"), subdir[[2L]])

  for (path in c(files1, files2, files3))
    cat(path, file = path)

  result1 <- sanitize(tmpdir, FALSE, TRUE, 0L)
  result2 <- sanitize(tmpdir, FALSE, FALSE, 0L)
  result3 <- sanitize(tmpdir, TRUE, TRUE, 0L)

  expect_true(nrow(result1) < length(c(files1, files2, files3)))
  expect_true(nrow(result1) > nrow(result2))
  expect_true(nrow(result1) < nrow(result3))

  expect_identical(ncol(result1), ncol(result2))
  expect_identical(ncol(result1), ncol(result3))

  for (i in seq_along(result1)) {
    expect_true(all(is.element(result2[, i], result1[, i])))
    expect_true(all(is.element(tolower(result1[, i]), tolower(result3[, i]))))
  }

  result1full <- sanitize(result1, FALSE, FALSE, 0L)

  expect_identical(ncol(result1), ncol(result1full))
  expect_identical(nrow(result1), nrow(result1full))
  for (i in seq_along(result1))
    expect_identical(result1full[, i], result1[, i])

})


################################################################################
