
library(testthat)
library(pkgutils)

pack_desc("C:\\Users\\lea\\Documents\\biolog 2013\\Work\\opm_in\\opm_in", "source")
source("C:\\Users\\lea\\Documents\\biolog 2013\\Work\\opm_in\\opm_in\\inst\\tests\\test_mult-opm.R")


context("Testing the multiple-testing functions of the OPM package")

# um die beispieldaten zu laden
if (!exists("TEST.DIR"))
  attach(objects_for_testing())

# beispieldaten jetzt in 
objects_for_testing()

A_VALUES <- extract(c(THIN.AGG, THIN.AGG), as.labels = list("organism", "run"),
                    subset = "A", dataframe = TRUE)


#' # Without computation of multiple comparisons of means
#' (xx <- opm_mcp(vaas_4, as.labels = list("Species", "Strain"), 
#'   per.mcp = FALSE))
#'