

library(tools)
library(opmDB)
library(RODBC)


################################################################################


slots_equal <- function(a, b) {
  sapply(setdiff(slotNames(a), "csv_data"),
    function(n) all.equal(slot(a, n), slot(b, n)))
}


################################################################################

# Insertions via RODBC in this manner are slow. Subsetting speeds things up.
#
x <- vaas_4[, 1:5, 12:14]

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

# 'rownames = FALSE' is crucial, otherwise segfault.
#
by(xDB, TRUE, sqlSave, channel = chan, append = TRUE, test = FALSE,
  rownames = FALSE, verbose = FALSE, fast = TRUE, simplify = FALSE)

# Cannot INSERT the same IDs again.
#
assertError(by(xDB, TRUE, sqlSave, channel = chan, append = TRUE, test = FALSE,
  rownames = FALSE, verbose = FALSE, fast = TRUE))

# Receiving data again, using the known IDs.
#
y <- new("OPMD_DB")
y <- by(y, xDB@plates[, "id"], sqlQuery, channel = chan,
  stringsAsFactors = FALSE, do_inline = TRUE)

## TODO: add drop example

odbcClose(chan)

# Comparison with original OPMS object
#
y <- as(y, "OPMS")
stopifnot(dim(y) == dim(x))
stopifnot(sapply(1:4, function(i) slots_equal(y[i], x[i])))


################################################################################



