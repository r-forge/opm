


################################################################################


#' Listing of measurements
#'
#' Create a textual listing of the measurements suitable for, e.g., reports in
#' taxonomic journals such as \acronym{IJSEM}.
#'
#' @param x \code{\link{FAMES}} object.
#' @param as.groups Key used for querying the metadata.
#' @param cutoff Numeric scalar determining the minimum frequency to report.
#' @return Character vector with one element per group as defined by
#'   \code{as.groups}.
#' @export
#' @keywords attribute character
#' @family naming-functions
#' @details TODO
#' @examples
#' # TODO
#'
setMethod("listing", FAMES, function(x, as.groups, cutoff = -Inf) {
  x <- as(x, "matrix")
  if (!length(as.groups))
    as.groups <- TRUE
  as.groups <- as.factor(extract_columns(object, as.groups, TRUE))
  stop("not yet finished")
}, sealed = SEALED)


################################################################################


#' Normalize fatty-acid names
#'
#' Convert names of fatty acids from the \acronym{MIDI} system to a standardized
#' way to express them.
#'
#' @param x Character vector containing the fatty-acid names to be mapped. May
#'   also be a matrix; in that case, its row names are mapped.
#' @param warn Logical scalar. Issue a warning message if one to several names
#'   are not found?
#' @inheritParams fatty_acids
#' @return Character vector.
#' @keywords internal
#' @details Renaming is currently based on a precomputed lookup table, i.e. the
#'   names are not parsed. The lookup table, however, is based on the complete
#'   set of peak naming tables in use at \acronym{DSMZ}.
#'
norm_fa <- function(x, ...) UseMethod("norm_fa")

#' @rdname norm_fa
#' @method norm_fa NULL
#' @export
#'
norm_fa.NULL <- function(x, ...) {
  x
}

#' @rdname norm_fa
#' @method norm_fa character
#' @export
#'
norm_fa.character <- function(x, warn = FALSE, ...) {
  y <- FA_MAP[x]
  if (any(isna <- is.na(y))) {
    ok <- x[isna] %in% FA_MAP
    y[isna][ok] <- x[isna][ok]
    isna[isna][ok] <- FALSE
    if (any(isna)) {
      if (warn)
        warning("could not find these fatty-acid names:\n",
          paste0(x[isna], collapse = "\n"))
      y[isna] <- x[isna]
    }
  }
  unname(y)
}

#' @rdname norm_fa
#' @method norm_fa matrix
#' @export
#'
norm_fa.matrix <- function(x, ...) {
  rownames(x) <- norm_fa(rownames(x), ...)
  x
}


################################################################################



