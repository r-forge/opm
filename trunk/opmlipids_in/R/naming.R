


################################################################################


setOldClass(c("FAMES_Listing", "OPMD_Listing"))


#' Listing of measurements
#'
#' Create a textual listing of the measurements suitable for, e.g., reports in
#' taxonomic journals such as \acronym{IJSEM}.
#'
#' @param x \code{\link{FAMES}} object.
#' @param as.groups Key used for querying the metadata.
#' @param cutoff Numeric scalar determining the frequency below which to ignore
#'   measurements in output.
#' @param html Logical scalar indicating whether \acronym{HTML} shall be
#'   produced.
#' @param digits Numeric scalar indicating the number of decimal places in the
#'   output.
#' @return Character vector with one element per group as defined by
#'   \code{as.groups}; an object of the class \code{FAMES_Listing}.
#' @export
#' @keywords attribute character
#' @family naming-functions
#' @examples
#' ## TODO
#'
setMethod("listing", FAMES, function(x, as.groups, cutoff = 0, html = FALSE,
    digits = 1L) {
  to_text <- function(x, cutoff, fmt) {
    result <- colMeans(x)
    result <- result[ok <- result > cutoff]
    result <- result[ord <- order(result, decreasing = TRUE)]
    if (nrow(x) > 1L) {
      sdev <- apply(x[, ok, drop = FALSE], 2L, sd)[ord]
      result <- structure(sprintf(fmt[2L], result, sdev), names = names(result))
    } else {
      result <- structure(sprintf(fmt[1L], result), names = names(result))
    }
    listing(result, style = "sentence", prepend = "%s (%s %%)")
  }
  #LL(html, cutoff, digits)
  if (!length(as.groups))
    as.groups <- TRUE
  grps <- as.factor(extract_columns(x, as.groups, TRUE, dups = "ignore"))
  x <- as(x, "matrix")
  if (html)
    colnames(x) <- fs_to_html(colnames(x))
  grps <- split.default(seq_len(nrow(x)), grps)
  fmt <- c(sprintf("%%.%if", digits), "")
  fmt[2L] <- paste(fmt[1L], if (html)
      "&plusmn;"
    else
      "+/-", fmt[1L])
  x <- vapply(grps, function(i) to_text(x[i, , drop = FALSE], cutoff, fmt), "")
  fmt <- if (missing(cutoff) || identical(cutoff, -Inf))
      "The fatty acids of %s were %s."
    else
      "The major fatty acids of %s were %s."
  x <- structure(sprintf(fmt, names(x), x), names = names(x))
  structure(x, class = c("FAMES_Listing", "OPMD_Listing"), html = html,
    cutoff = cutoff)
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

#' @rdname norm_fa
#'
fs_to_html <- function(x) {
  x <- safe_labels(x, "html")
  gsub("(?<=\\bC)(\\d+:\\d+)\\b", "<sub>\\1</sub>", x, FALSE, TRUE)
}


################################################################################


