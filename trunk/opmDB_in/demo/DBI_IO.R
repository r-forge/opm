

library(opm)
library(opmDB)
library(RPostgreSQL)

drv <- dbDriver("PostgreSQL")

con <- dbConnect(drv, dbname = "pmdata")


################################################################################


slots_equal <- function(a, b) {
  sapply(slotNames(a), function(n) all.equal(slot(a, n), slot(b, n)))
}


delete <- function(name, table, conn) {
  q <- sprintf("DELETE FROM %s WHERE id = %i", name, table[, "id"])
  dbSendQuery(conn, q)
}


################################################################################



x <- vaas_4 #[, 1:5, 12:14] # subsetting speeds things up

# Metadata removal is crucial because columns are missing; otherwise a seqfault
# results. We add names to the empty list to ease the comparisons.
#
metadata(x) <- structure(list(), names = character())

xDB <- as(x, "OPMD_DB")


by(xDB, TRUE, dbWriteTable, conn = con, append = TRUE, row.names = FALSE)

# Receiving data again, using the known IDs.
#
y <- new("OPMD_DB")
y <- by(y, xDB@plates[, "id"], dbGetQuery, conn = con,
  stringsAsFactors = FALSE, do_inline = TRUE, simplify = TRUE)

#by(xDB, TRUE, delete, conn = con)

dbDisconnect(con)

# Comparison with original OPMS object
#
y <- as(y, "OPMS")
stopifnot(dim(y) == dim(x))
stopifnot(sapply(1:4, function(i) slots_equal(y[i], x[i])))

## TODO: add drop example
# TODO: RSQLite
# TODO: RMySQL
