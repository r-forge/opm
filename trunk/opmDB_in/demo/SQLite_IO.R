

library(opm)
library(opmDB)
library(RSQLite)


slots_equal <- function(a, b) {
  sapply(setdiff(slotNames(a), "csv_data"),
    function(n) all.equal(slot(a, n), slot(b, n)))
}


################################################################################


# Metadata removal is crucial because columns are missing; otherwise a seqfault
# results. We add names to the empty list to ease the comparisons.
#
x <- vaas_4
metadata(x) <- structure(list(), names = character())

conn <- dbConnect("SQLite",
  dbname = Sys.getenv("OPM_SQLITE_DB", file.path("misc", "pmdata.db")))

got <- opm_dbput(x, conn)

# Receiving data again, using the known IDs.
y <- opm_dbget(got, conn)

# Deleting all inserted plates.
opm_dbclear(got, conn)

dbDisconnect(conn)

# Comparison with original OPMS object
stopifnot(dim(y) == dim(x))
stopifnot(sapply(1:4, function(i) slots_equal(y[i], x[i])))

