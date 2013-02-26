
library(testthat)
context("Testing the multiple-testing functions of the OPM package")


# get example objects
if (!exists("TEST.DIR"))
  attach(objects_for_testing())


A_VALUES <- extract(c(THIN.AGG, THIN.AGG),
  as.labels = list("organism", "run"), subset = "A", dataframe = TRUE)



## opm_mcp
test_that("mcp without mcp", {
  #   Without computation of multiple comparisons of means
  x <- opm_mcp(A_VALUES, as.labels = list("run", "organism"), per.mcp = FALSE)
  expect_is(x, "data.frame")
  expect_equal(dim(x), c(384L, 6L))
})


## opm_mcp
test_that("mcp with specific model, but without mcp.def", {
  #   Without computation of multiple comparisons of means
   expect_error(x <- opm_mcp(A_VALUES, as.labels = list("run"), m.type = "lm"))
  })

# 
# test_that("mcp with specific model, but with mcp.def", {
#   #   Without computation of multiple comparisons of means
#  expect_message(x <- opm_mcp(A_VALUES, as.labels = list("run"), m.type = "lm", 
#    mcp.def = mcp(run = "Dunnett")))
# expect_is(x, "glht")
#   expect_equal(x$type, "Dunnett")
#   expect_true(is.list(x))
#   expect_equal(length(x), 9)
#   expect_equal(length(coef(x)), 1)
# })


## opm_mcp
test_that("comparisons of Species pooled over complete plates", {
  # LEA please uncomment and fix
#   expect_message(x <- opm_mcp(A_VALUES, as.labels = list("run"), m.type = "lm",
#     mcp.def = mcp(run = "Dunnett"), glht.arg = list())
#   expect_is(x, "glht")
#   expect_equal(x$type, "Dunnett")
#   expect_true(is.list(x))
#   expect_equal(length(x), 9)
#   expect_equal(length(coef(x)), 1)
})


## opm_mcp
test_that("comparisons of only A01 - A04 against intercept", {
  # LEA please uncomment and fix
#   x <- opm_mcp(A_VALUES, as.labels = list("run"), sub.list = c(1:4), 
#     model = "Value ~ Well")
#   expect_is(x, "glht")
#   expect_equal(x$type, NULL)
#   expect_true(is.list(x))
#   expect_equal(length(x), 8)
#   expect_equal(length(coef(x)), 5)
})


## opm_mcp
test_that("user defined a contrastmatrix", {
  # LEA please uncomment and fix
#   a <- mcp(Well = "Dunnett")
#   x <- opm_mcp(A_VALUES, as.labels = list("run"), sub.list = c(1:4),
#     model = "Value ~ Well", m.type = "lm", mcp.def = a)
#   expect_is(x, "glht")
#   expect_equal(x$type, "Dunnett")
#   expect_true(is.list(x))
#   expect_equal(length(x), 9)
#   expect_equal(length(coef(x)), 3)
})

# TODO: lea hier weitermachen, erst markus fragen, ob es eine matrix in
# objects_for_testing() gibt

# ## opm_mcp
# test_that("matrix-method", {
#   x
#
# })
#'
#' ## matrix method
#' x <- extract(vaas_4, as.labels = list("Species", "Strain"), subset = "A",
#'   dataframe = TRUE)
#'
#' (xx <- opm_mcp(x, as.labels = c("Species"), m.type = "lm"))
#' # plot method is available
#' par(mar = c(3, 15, 3, 2))
#' plot(xx)
#~ TODO LEA: include check; I can't see from the code what the result should be
#~ TODO LEA: shouldn't par() be reset?
#'
#' # without performing the MCP
#' xx <- opm_mcp(x, per.mcp = FALSE)
#'
#' # testing for subsets of object
#' (xx <- opm_mcp(subset(x, Species == "Escherichia coli"),
#'   mcp.def = mcp(Strain = "Dunnett"), as.labels = c("Strain"), m.type = "lm"))
#' # plot method available
#' par(mar = c(3, 15, 3, 2))
#' plot(xx)
#~ TODO LEA: include check; I can't see from the code what the result should be
#~ TODO LEA: shouldn't par() be reset?
#'
