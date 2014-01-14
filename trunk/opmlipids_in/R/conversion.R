

################################################################################


#' Extract values and metadata
#'
#' Extract selected values into common matrix or data frame, optionally
#' including selected metadata entries for use as additional columns in a data
#' frame or (after joining) as character vector with labels.
#'
#' @param object \code{\link{FAMES}} object.
#' @param as.labels List, character vector or formula indicating the metadata to
#'   be joined and used as row names (if \code{dataframe} is \code{FALSE}) or
#'   additional columns (if otherwise). Ignored if \code{NULL}. See the
#'   \pkg{opm} package for further details on this \code{extract} argument.
#' @param dataframe Logical scalar. Return data frame or matrix?
#'
#' @param as.groups For the \code{\link{FAMES}} method, a list, character vector
#'   or formula indicating the metadata to be joined and either used as
#'   \sQuote{row.groups} attribute of the output matrix or as additional columns
#'   of the output data frame. See \code{\link{heat_map}} for its usage. Ignored
#'   if empty.
#'
#'   If a \code{as.groups} is a formula and \code{dataframe} is \code{TRUE}, the
#'   pseudo-function \code{J} within the formula can be used to trigger
#'   combination of factors immediately after selecting them as data-frame
#'   columns, much like \code{as.labels}.
#'
#'   If \code{as.groups} is a logical scalar, \code{TRUE} yields a trivial group
#'   that contains all elements, \code{FALSE} yields one group per element, and
#'   \code{NA} yields an error. The column name in which this factor is placed
#'   if \code{dataframe} is \code{TRUE} is determined using
#'   \code{opm_opt("group.name")}.
#' @param sep Character scalar. Used as separator between the distinct metadata
#'   entries if these are to be pasted together.
#' @param dups Character scalar specifying what to do in the case of duplicate
#'   labels: either \sQuote{warn}, \sQuote{error} or \sQuote{ignore}. Ignored
#'   unless \code{join} is \code{TRUE} and if \code{object} is an
#'   \code{\link{OPM}} object. For the data-frame method of \code{extract}, a
#'   character scalar defining the action to conduct if \code{as.groups}
#'   contains duplicates.
#'
#' @param exact Logical scalar. Passed to \code{\link{metadata}}.
#' @param strict Logical scalar. Also passed to \code{\link{metadata}}.
#'
#' @export
#' @return Numeric matrix or data frame.
#'
#' @details \code{extract_columns} is not normally directly called by an
#'   \pkg{opm} user because \code{extract} is available, which uses this
#'   function, but can be used for testing the applied metadata selections
#'   beforehand.
#'
#'   The \code{extract_columns} data-frame method is partially trivial (extract
#'   the selected columns and join them to form a character vector or new
#'   data-frame columns), partially more useful (extract columns with data of a
#'   specified class).
#'
#' @family conversion-functions
#' @author Markus Goeker
#' @seealso
#'   base::data.frame base::as.data.frame base::as.matrix base::cbind
#' @keywords manip dplot htest
#' @examples
#'
#' ## introduce useful metadata entries
#' x <- DSM_44549
#' metadata(x) <- Strain ~ sub("[(].+", "", `Sample ID`)
#' metadata(x) <- Time ~ tolower(substr(sub("[^(]+[(]", "", `Sample ID`), 1, 3))
#' metadata(x) <- Cultivation ~ sub("[)]", "", sub("[^,]+,", "", `Sample ID`))
#'
#' ## extract data frame
#' (y <- extract(x, list("Strain", "Time"), TRUE, "Cultivation"))
#' stopifnot(is.data.frame(y), c("Strain", "Time") %in% names(y))
#' stopifnot(c("Cultivation", opm::param_names("split.at")) %in% names(y))
#' # note positioning of separator column
#'
#' ## extract matrix
#' (y <- extract(x, list("Strain", "Time"), FALSE, "Cultivation",
#'   dups = "ignore"))
#' stopifnot(is.matrix(y), is.numeric(y))
#' stopifnot(!c("Strain", "Time") %in% names(y))
#' stopifnot(!c("Cultivation", opm::param_names("split.at")) %in% names(y))
#' # i.e. the selected metadata are not inserted as columns but elsewhere
#' # see opm for details
#'
setGeneric("extract", function(object, ...) standardGeneric("extract"))

setMethod("extract", FAMES, function(object, as.labels, dataframe = FALSE,
    as.groups = NULL, sep = " ", dups = "warn", exact = TRUE, strict = TRUE) {

  do_extract <- function(what, join, dups) {
    extract_columns(object, what = what, join = join, sep = sep, dups = dups,
      exact = exact, strict = strict)
  }
  create_groups <- function(x, join) {
    numeric_groups <- function(how) {
      if (L(how))
        rep.int(1L, length(object))
      else
        seq_len(length(object))
    }
    if (join) {
      result <- if (is.logical(x))
        numeric_groups(x)
      else
        do_extract(x, TRUE, "ignore")
      result <- as.factor(result)
    } else if (is.logical(x)) {
      result <- as.data.frame(numeric_groups(x))
      rownames(result) <- opm_opt("group.name")
    } else {
      result <- do_extract(x, FALSE, "ignore")
    }
    result
  }

  result <- as(object, "matrix")

  if (dataframe) {

    result <- as.data.frame(result)
    if (length(as.labels)) {
      columns <- do_extract(as.labels, FALSE, "ignore")
      columns <- cbind(columns, plate_type(object))
      colnames(columns)[ncol(columns)] <- param_names("split.at")
      rownames(result) <- rownames(columns) # otherwise a warning is likely
      result <- cbind(columns, result)
    } else {
      params <- rownames(result)
      rownames(result) <- seq_len(nrow(result))
      result <- cbind(params, result)
      colnames(result)[1L] <- param_names("split.at")
    }
    if (length(as.groups))
      result <- cbind(result, create_groups(as.groups, FALSE))

  } else {

    if (length(as.labels)) {
      labels <- do_extract(as.labels, TRUE, dups)
      rownames(result) <- labels
    } else {
      rownames(result) <- seq_len(nrow(result))
    }
    if (length(as.groups))
      attr(result, "row.groups") <- create_groups(as.groups, TRUE)
  }

  result

}, sealed = SEALED)


################################################################################

