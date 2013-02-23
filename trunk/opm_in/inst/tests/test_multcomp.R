
library(testthat)
library(pkgutils)

pack_desc("C:\\Users\\lea\\Documents\\biolog 2013\\svn-next-try\\opm_in", "source")

context("Testing the multiple-testing functions of the OPM package")


# get example objects
if (!exists("TEST.DIR"))
  attach(objects_for_testing())


A_VALUES <- extract(c(THIN.AGG, THIN.AGG),
  as.labels = list("organism", "run"), subset = "A", dataframe = TRUE)


## opm_mcp
## UNTESTED


# # Without computation of multiple comparisons of means
# (xx <- opm_mcp(vaas_4, as.labels = list("Species", "Strain"),
#   per.mcp = FALSE))
#

