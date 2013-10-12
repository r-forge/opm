

library(tools)
library(pkgutils)


pkg <- "pkgutils"


################################################################################


must_be <- function(got, wanted) if (!identical(got, wanted))
  stop(deparse(got), " not identical to ", deparse(wanted))


################################################################################
#
# Use the package's own functionality to see what's there
#

must_be(length(x <- pkg_files(pkg, "scripts")), 3L)
stopifnot(!check_R_code(x))

must_be(length(pkg_files(pkg, "highlighting")), 2L)

must_be(length(pkg_files(pkg, "auxiliary")), 2L)


################################################################################
#
# Check the datasets
#

stopifnot(nrow(data(package = pkg)$results) == 1L)


################################################################################
#
# Tests for case()
#

must_be(case(0, "a", "b"), "a")
must_be(case(1, "a", "b"), "b")
must_be(case(10, "a", "b"), "b")
assertError(case(NA_real_, "a", "b"))
assertError(case(-1, "a", "b"))
must_be(case("a", a = "a", b = "b"), "a")
must_be(case("b", a = "a", b = "b"), "b")
assertError(case("c", a = "a", b = "b"))
assertError(case(NA_character_, a = "a", b = "b"))


################################################################################
#
# Tests for must(), L() and LL()
#

x <- 3
y <- 9:10
z <- 'a'
must_be(c("x", "z"), LL(x, z))
assertError(LL(x, y))
assertError(LL(x, y, .wanted = 2L))
assertError(LL(y, z, .wanted = 2L))
must_be("y", LL(y, .wanted = 2L))
assertError(must(warning("abc")))




