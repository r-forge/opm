

################################################################################
################################################################################
#
# DBTABLES helper functions.
#


#' Print
#'
#' Print \code{\link{DBTABLES}} summaries to the screen.
#'
#' @param x Object of class \sQuote{DBTABLES_Summary}.
#' @param ... Optional arguments passed to and from other methods.
#' @return \code{x} is returned invisibly.
#' @keywords internal
#' @name print
#'
NULL

#' @rdname print
#' @method print DBTABLES_Summary
#' @export
#'
print.DBTABLES_Summary <- function(x, ...) {
  cat("An object of class ", sQuote(x$Class), ".\n", sep = "")
  cat("Number of rows in first table: ", x$Size, "\n", sep = "")
  cat("Defined cross-references between tables:\n")
  cr <- x$Crossrefs
  cr[is.na(cr)] <- "<MISSING>"
  cr <- structure(paste(cr[, "to.tbl"], cr[, "to.col"], sep = "."),
    names = paste(cr[, "from.tbl"], cr[, "from.col"], sep = "."))
  cat(formatDL(cr), ..., sep = "\n")
  invisible(x)
}


