
################################################################################
#
# This is the testing master file for the package. We use unit testing via the
# 'testthat' package. The proper files with the tests are in the inst/tests
# subdirectory.
#
################################################################################


library(pfn)

if (require(testthat)) {
  test_package("pfn")
}
