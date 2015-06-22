


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


#' Get or enter plate-type settings
#'
#' Set or retrieve settings for a given plate type.
#'
#' @param x Character vector, named list, or missing. If a character vector,
#'   the names of the plates types for which settings should be stored (if
#'   \code{...} is non-empty) or retrieved (if \code{...} is empty). If a list,
#'   then the list must be named with plate types and contain named lists which
#'   are supposed to contain the settings for each plate type.
#' @param ... Optional arguments used for determining the settings for each
#'   plate type contained in \code{x}. Missing settings are replaced by default
#'   ones. If \code{...} is totally missing, nothing is set, but the already
#'   stored settings for the plate types given in \code{x} are returned.
#' @return This returns a named nested lists, with the names corresponding to
#'   plate-type names after normalisation.
#' @details The \pkg{opmlipids} package uses various settings for customising
#'   the behaviour of \code{\link{FAMES}} objects depending on their plate type.
#'   \describe{
#'   \item{char.group}{The name used in textual output for the kind of
#'   characters stored in this plate type. Used by \code{\link{listing}}.}
#'   \item{file.entry}{How to name input-file entries within the metadata.}
#'   \item{na.yields}{When converting \code{\link{FAMES}} objects to matrices
#'   some values might be missing from some of the contained \code{\link{FAME}}
#'   objects, which are replaced by this default value.}
#'   \item{row.names}{How to name the \code{rownames} entry of the
#'   \code{\link{measurements}} slot when converting to a list.}
#'   \item{sum.up}{Logical scalar indicating whether or not duplicate row names
#'   should be summed up. (The alternative is to make them unique, but this is
#'   not guaranteed to work across distinct plates.)}
#'   \item{tolerance}{How much deviation from a sum of 100 percent to accept in
#'   percentage input.}
#'   \item{value.col}{The name of the column in the \code{\link{measurements}}
#'   data frame that stores the numeric measurements (all other columns, if any,
#'   should contain additional information on the respective measurements.)}
#'   }
#' @family naming-functions
#' @keywords utilities
#' @seealso \link{plate_type}
#' @seealso base::options base::getOption opm::opm_opt
#' @export
#' @examples
#' oli_opt("MIDI") # this one is already defined
#'
setGeneric("oli_opt", function(x, ...) standardGeneric("oli_opt"))

setMethod("oli_opt", "character", function(x, ...) {
  x <- make.names(x)
  if (missing(...))
    return(mget(x, PLATE_TYPE_SETTINGS))
  oli_opt(structure(rep.int(list(list(...)), length(x)), names = x))
}, sealed = SEALED)

setMethod("oli_opt", "list", function(x) {
  add_defaults <- function(x) {
    lacking <- setdiff(names(DEFAULT_PLATE_TYPE_SETTINGS), names(x))
    x[lacking] <- DEFAULT_PLATE_TYPE_SETTINGS[lacking]
    x
  }
  if (is.null(names(x)) || !all(vapply(x, is.list, NA)))
    stop("'x' must be a named list of lists")
  x <- structure(lapply(x, add_defaults), names = make.names(names(x)))
  list2env(x, PLATE_TYPE_SETTINGS)
  invisible(x)
}, sealed = SEALED)

setMethod("oli_opt", "missing", function(x) {
  as.list(PLATE_TYPE_SETTINGS)
}, sealed = SEALED)


################################################################################


