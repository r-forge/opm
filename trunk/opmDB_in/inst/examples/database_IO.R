

library(tools)
library(opmDB)
library(RODBC)


################################################################################


slots_equal <- function(a, b) {
  sapply(slotNames(a), function(n) all.equal(slot(a, n), slot(b, n)))
}


################################################################################


x <- vaas_4[, 1:5, 12:14] # subsetting speeds things up

# Metadata removal is crucial because columns are missing; otherwise a seqfault
# results. We add names to empty list to ease the comparisons.
#
metadata(x) <- structure(list(), names = character())

xDB <- as(x, "OPMD_DB")


################################################################################
#
# RODBC
#

chan <- odbcConnect("test_opm;TextAsLongVarchar=1;MaxLongVarcharSize=8190")

# 'rownames = FALSE' is crucial, otherwise seqfault.
#
by(xDB, TRUE, sqlSave, channel = chan, append = TRUE, test = FALSE,
  rownames = FALSE, verbose = FALSE, fast = TRUE)

# Cannot INSERT the same IDs again.
#
assertError(by(xDB, TRUE, sqlSave, channel = chan, append = TRUE, test = FALSE,
  rownames = FALSE, verbose = FALSE, fast = TRUE))

# Receiving data again, using the known IDs.
#
y <- new("OPMD_DB")
y <- collect(y, xDB@plates[, "id"], sqlQuery, channel = chan,
  stringsAsFactors = FALSE)

## TODO: add drop example

odbcClose(chan)

# Comparison with original OPMS object
#
y <- as(y, "OPMS")
stopifnot(dim(y) == dim(x))
stopifnot(sapply(1:4, function(i) slots_equal(y[i], x[i])))


################################################################################

# TODO: RSQLite
# TODO: RPostgreSQL
# TODO: RMySQL



