


################################################################################


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
#' @return Character matrix with one row per group as defined by
#'   \code{as.groups}, one column, and additional attributes. The class is
#'   \code{FAMES_Listing}, which inherits from \code{OPMS_Listing} from the
#'   \pkg{opm} package.
#' @export
#' @keywords attribute character
#' @family naming-functions
#' @examples
#' x <- c(DSM_44549, DSM_7109)
#' metadata(x) <- Strain ~ sub("C-", "-", sub("[(].+", "", `Sample ID`))
#' (y <- listing(x, "Strain"))
#' stopifnot(inherits(y, "FAMES_Listing"))
#' (z <- listing(x, "Strain", cutoff = 5)) # report only on frequent ones
#' stopifnot(nchar(z) < nchar(y))
#' (z <- listing(x, "Strain", html = TRUE))
#' stopifnot(nchar(z) > nchar(y))
#' write(phylo_data(z), file = "") # create complete HTML document
#'
setMethod("listing", "FAMES", function(x, as.groups, cutoff = 0, html = FALSE,
    digits = 1L) {

  format_for_single_fa <- function(digits, html) {
    fmt <- sprintf("%%.%if", digits)
    c(fmt, paste(fmt, if (html)
        "&plusmn;"
      else
        "+/-", fmt))
  }

  format_for_all_fa <- function(char.group, digits, html, cutoff) {
    if (identical(cutoff, -Inf)) {
      fmt <- tolower(char.group)
    } else {
      fmt <- sprintf("principal %%s (%%s %%.%if %%%%%%%%)", digits)
      fmt <- sprintf(fmt, tolower(char.group), if (html)
          "&gt;"
        else
          ">", cutoff)
    }
    paste("The", fmt, "of %s were %s.")
  }

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

  LL(html, cutoff, digits)
  if (!length(as.groups))
    as.groups <- TRUE
  grps <- as.factor(extract_columns(x, as.groups, TRUE, dups = "ignore"))
  char.group <- get_for(plate_type(x), "char.group")
  x <- as(x, "matrix")
  if (html)
    colnames(x) <- fs_to_html(colnames(x))
  grps <- split.default(seq_len(nrow(x)), grps)
  fmt <- format_for_single_fa(digits, html)
  x <- vapply(grps, function(i) to_text(x[i, , drop = FALSE], cutoff, fmt), "")
  fmt <- format_for_all_fa(char.group, digits, html, if (missing(cutoff))
      -Inf
    else
      cutoff)
  x <- structure(sprintf(fmt, names(x), x), names = names(x))
  x <- matrix(x, length(x), 1L, FALSE, list(names(x), char.group))
  structure(x, class = c("FAMES_Listing", "OPMS_Listing"), html = html,
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
  gsub("(?<=\\bC)(\\d+:\\d+\\b[^/]*)", "<sub>\\1</sub>", x, FALSE, TRUE)
}


################################################################################

