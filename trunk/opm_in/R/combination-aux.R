################################################################################
################################################################################
#
# Conditional conversions to OPMS objects or according lists
#


#' Convert to OPM list
#'
#' Convert to list of \code{\link{OPM}} objects, or conditionally convert a list
#' to an \code{\link{OPMS}} object. Used for building an \code{\link{OPMS}}
#' object by \code{\link{opms}} and optionally bulding one by \code{\link{c}}.
#' Finally, \code{close_index_gaps} is a helper function for the assignment
#' methods (bracket operators).
#'
#' @param object List of objects that can be passed to \code{\link{opms}}
#'   (optional for \code{try_opms}).
#' @param precomputed Logical scalar. See \code{\link{opms}}.
#' @param skip Logical scalar. See \code{\link{opms}}.
#' @param group Logical scalar. See \code{\link{opms}}.
#' @return \code{to_opm_list} yields a list, \code{try_opms} an
#'   \code{\link{OPMS}} object (if conversions was successful) or just the input
#'   \code{object} (if conversions was unsuccessful).
#' @keywords internal
#'
to_opm_list <- function(object, ...) UseMethod("to_opm_list")

#' @method to_opm_list list
#' @rdname to_opm_list
#' @export
#'
to_opm_list.list <- function(object, precomputed = TRUE, skip = FALSE,
    group = FALSE) {
  LL(precomputed, skip, group)
  opmd.slots <- setdiff(slotNames("OPMD"), opma.slots <- slotNames("OPMA"))
  opma.slots <- setdiff(opma.slots, opm.slots <- slotNames("OPM"))
  convert_recursively <- function(item) {
    if (!is.list(item))
      if (skip)
        return(NULL)
    else
      stop("non-list element encountered")
    keys <- names(item)
    if (all(opm.slots %in% keys))
      as(item, if (all(opma.slots %in% keys))
        if (all(opmd.slots %in% keys))
          "OPMD"
        else
          "OPMA"
        else
          "OPM")
    else
      lapply(item, FUN = convert_recursively)
  }
  get_plates <- function(item) {
    if (is(item, "OPM"))
      item
    else if (is(item, "OPMS"))
      plates(item)
    else if (skip)
      NULL
    else
      stop("need object derived from 'OPM' or 'OPMS', got ", class(item)[1L])
  }
  result <- if (precomputed)
    rapply(object, get_plates, "ANY", NULL, "unlist")
  else
    c(convert_recursively(object), recursive = TRUE)
  if (group)
    result <- if (is.null(result))
        list()
      else
        result <- split.default(result, vapply(result, plate_type, ""))
  result
}

#' @rdname to_opm_list
#'
try_opms <- function(object, ...) UseMethod("try_opms")

#' @method try_opms list
#' @rdname to_opm_list
#' @export
#'
try_opms.list <- function(object, precomputed = TRUE, skip = FALSE) {
  tryCatch(expr = new(Class = "OPMS",
    plates = to_opm_list.list(object, precomputed, skip, FALSE)),
    error = function(e) object)
}


