NULL

print.DBTABLES_Summary <- function(x, ...) {
  cat("An object of class ", sQuote(x$Class), ".\n", sep = "")
  cat("Number of rows in first table: ", x$Size, "\n", sep = "")
  cat("Defined cross-references between tables:\n")
  cr <- x$Crossrefs
  cr[is.na(cr)] <- "<MISSING>"
  cr <- structure(.Data = paste(cr[, "to.tbl"], cr[, "to.col"], sep = "."),
    names = paste(cr[, "from.tbl"], cr[, "from.col"], sep = "."))
  cat(formatDL(cr), ..., sep = "\n")
  invisible(x)
}

