


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
  opmd.slots <- setdiff(slotNames(OPMD), opma.slots <- slotNames(OPMA))
  opma.slots <- setdiff(opma.slots, opm.slots <- slotNames(OPM))
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
          OPMD
        else
          OPMA
        else
          OPM)
    else
      lapply(item, FUN = convert_recursively)
  }
  get_plates <- function(item) {
    if (is(item, OPM))
      item
    else if (is(item, OPMS))
      plates(item)
    else if (skip)
      NULL
    else
      stop("need object derived from ", OPM, " or ", OPMS, ", got ",
        class(item)[1L])
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
  tryCatch(
    new(OPMS, plates = to_opm_list.list(object, precomputed, skip, FALSE)),
    error = function(e) object)
}


################################################################################
################################################################################
#
# Assigning into OPMS objects
#


#' Assign subset
#'
#' Assign subsets of \code{\link{OPMS}} objects.
#'
#' @param x \code{\link{OPMS}} or \code{\link{MOPMX}} object.
#' @param i One to several plate indexes. Should be compatible with the length
#'   of \code{value}. Otherwise any resulting \code{NULL} elements will be
#'   removed (with a warning), causing the resulting plate indexes to be unequal
#'   to \code{i}, which might be confusing.
#' @param j Must \strong{not} be set. See the examples.
#' @param name Unevaluated symbol used for as index of a single element.
#' @param value Value to be assigned. \code{NULL} causes the selected plates to
#'   be removed. Alternatively, \code{\link{OPM}} or \code{\link{OPMS}} objects
#'   or lists of \code{\link{OPM}} objects can be assigned. All assignments are
#'   subject to the restrictions explained in the help entries of the
#'   \code{\link{OPMS}} and \code{\link{MOPMX}} classes.
#' @return \code{value}.
#' @family combination-functions
#' @keywords manip
#' @rdname bracket.set
#' @exportMethod "[<-"
#' @export
#' @examples
#' copy <- vaas_4
#' copy[5] <- NULL # has no effect
#' stopifnot(identical(vaas_4, copy))
#' length(copy)
#' copy[2:3] <- NULL # removes these plates
#' length(copy)
#' stopifnot(length(vaas_4) == length(copy) + 2)
#' copy[1:4] <- vaas_4 # set all plates to the plates from 'vaas_4'
#' stopifnot(identical(vaas_4, copy))
#' copy[3] <- copy[3] # no change
#' stopifnot(identical(vaas_4, copy))
#' copy[3] <- copy[2] # now assign other plate
#' stopifnot(!identical(vaas_4, copy))
#' copy[6] <- copy[1] # gaps will be closed
#' stopifnot(length(copy) == 5) # not 6
#'
setMethod("[<-", c(OPMS, "ANY", "missing", "NULL"), function(x, i, j,
    value) {
  x@plates[i] <- NULL
  case(length(x@plates), NULL, x@plates[[1L]], x) # no checks necessary here
}, sealed = SEALED)

setMethod("[<-", c(OPMS, "ANY", "missing", OPM), function(x, i, j, value) {
  x@plates[i] <- value
  new(OPMS, plates = close_index_gaps(x@plates)) # checks and unnaming needed
}, sealed = SEALED)

setMethod("[<-", c(OPMS, "ANY", "missing", OPMS), function(x, i, j, value) {
  x@plates[i] <- value@plates
  new(OPMS, plates = close_index_gaps(x@plates)) # checks and unnaming needed
}, sealed = SEALED)

setMethod("[<-", c(OPMS, "ANY", "missing", "list"), function(x, i, j, value) {
  x@plates[i] <- value
  new(OPMS, plates = close_index_gaps(x@plates)) # checks and unnaming needed
}, sealed = SEALED)

setMethod("[<-", c(MOPMX, "ANY", "missing", OPMX), function(x, i, j, value) {
  x@.Data[i] <- value
  x@.Data <- close_index_gaps(x@.Data)
  x
})

setMethod("[<-", c(MOPMX, "character", "missing", OPMX), function(x, i, j,
    value) {
  n <- names(x)
  x@.Data[i] <- value
  names(x) <- fix_names(names(x), n)
  x@.Data <- close_index_gaps(x@.Data)
  x
})

setMethod("[<-", c(MOPMX, "ANY", "missing", "list"), function(x, i, j, value) {
  x@.Data[i] <- value
  x@.Data <- close_index_gaps(x@.Data)
  validObject(x)
  x
})

setMethod("[<-", c(MOPMX, "character", "missing", "list"), function(x, i, j,
    value) {
  n <- names(x)
  x@.Data[i] <- value
  names(x) <- fix_names(names(x), n)
  x@.Data <- close_index_gaps(x@.Data)
  validObject(x)
  x
})

setMethod("[<-", c(MOPMX, "ANY", "missing", "NULL"), function(x, i, j, value) {
  x@.Data[i] <- NULL
  x@.Data <- close_index_gaps(x@.Data)
  x
})

setMethod("[<-", c(MOPMX, "character", "missing", "NULL"), function(x, i, j,
    value) {
  x@.Data[match(i, names(x), 0L)] <- NULL
  x
})

setMethod("[<-", c(MOPMX, "ANY", "missing", "ANY"), function(x, i, j, value) {
  stop("'value' must be object inheriting from 'OPMX' or list of such objects")
})

#= double.bracket.set bracket.set

#' @exportMethod "[[<-"
#' @rdname bracket.set
#' @export
#'
setMethod("[[<-", c(MOPMX, "ANY", "missing", OPMX), function(x, i, j, value) {
  x@.Data[[i]] <- value
  x@.Data <- close_index_gaps(x@.Data)
  x
})

setMethod("[[<-", c(MOPMX, "character", "missing", OPMX), function(x, i, j,
    value) {
  n <- names(x)
  x@.Data[[i]] <- value
  names(x) <- fix_names(names(x), n)
  x
})

setMethod("[[<-", c(MOPMX, "ANY", "missing", "NULL"), function(x, i, j, value) {
  x@.Data[[i]] <- value
  x
})

setMethod("[[<-", c(MOPMX, "character", "missing", "NULL"), function(x, i, j,
    value) {
  if (m <- match(i, names(x), 0L))
    x@.Data[[m]] <- value
  x
})

setMethod("[[<-", c(MOPMX, "ANY", "missing", "ANY"), function(x, i, j, value) {
  stop("'value' must be NULL or inherit from 'OPMX'")
})

#= dollar.set bracket.set

#' @exportMethod "$<-"
#' @rdname bracket.set
#' @export
#'
setMethod("$<-", c(MOPMX, "OPMX"), function(x, name, value) {
  x[[name]] <- value
  x
})

setMethod("$<-", c(MOPMX, "ANY"), function(x, name, value) {
  x[[name]] <- value
  x
})

setMethod("$<-", c(MOPMX, "NULL"), function(x, name, value) {
  x[[name]] <- value
  x
})


################################################################################
################################################################################
#
# Combination functions
#


#' Combination and addition of plates
#'
#' Combine a \code{\link{OPMX}} or \code{\link{MOPMX}} object with other
#' objects.
#'
#' @param x \code{\link{OPMX}} or \code{\link{MOPMX}} object.
#' @param ... Other \R objects.
#' @param recursive Logical scalar. See \code{c} from the \pkg{base} package.
#' @param e1 \code{\link{OPMX}} object. If \code{e2} is a \code{\link{MOPMX}}
#'   object, anything that can be converted with \code{as} to that class.
#' @param e2 \code{\link{OPMX}} object, or list. If \code{e1} is a
#'   \code{\link{MOPMX}} object, anything that can be converted with \code{as}
#'   to that class.
#' @export
#' @return
#'   The \code{\link{OPMX}} method of \code{c} creates an \code{\link{OPMS}}
#'   object if possible, otherwise a list, or an \code{\link{OPM}} object (if
#'   \code{\dots} is not given and \code{x} is such an object). Similarly, the
#'   \code{\link{MOPMX}} method of \code{c} creates a \code{\link{MOPMX}} object
#'   is possible and a list otherwise.
#'
#'   If successful, \code{+} yields an \code{\link{OPMS}} object that contains
#'   the plates from both \code{e1} and \code{e2}, but it raises an error if the
#'   plates cannot be combined.
#'
#' @family combination-functions
#' @seealso base::c
#' @keywords manip
#' @examples
#'
#' # Adding nothing
#' dim(x <- c(vaas_1))
#' stopifnot(identical(x, vaas_1))
#' dim(x <- c(vaas_4))
#' stopifnot(identical(x, vaas_4))
#'
#' # Not particularly useful: adding identical plates!
#' dim(x <- c(vaas_1, vaas_1)) # yields a two-plate OPMS object
#' stopifnot(identical(dim(x), c(2L, dim(vaas_1))))
#'
#' # Also not particularly useful: adding partially identical plates!
#' dim(x <- c(vaas_4, vaas_1))
#' stopifnot(identical(dim(x), c(5L, dim(vaas_1))))
#'
#' # The following examples do not show particularly useful additions, as the
#' # plates are either entirely or partially identical. Note the changes in the
#' # dimensions.
#'
#' # OPM+OPM method
#' dim(x <- vaas_1 + vaas_1)
#' stopifnot(identical(dim(x), c(2L, dim(vaas_1))))
#'
#' # OPM+OPMS method
#' dim(x <- vaas_1 + vaas_4)
#' stopifnot(identical(dim(x), c(5L, dim(vaas_1))))
#'
#' # OPM+list method
#' dim(x <- vaas_1 + list(vaas_1, vaas_1))
#' stopifnot(identical(dim(x), c(3L, dim(vaas_1))))
#'
#' # OPMS+OPMS method
#' dim(x <- vaas_4 + vaas_4)
#' stopifnot(identical(dim(x), c(8L, dim(vaas_4)[-1L])))
#'
#' # OPMS+OPM method
#' dim(x <- vaas_4 + vaas_1)
#' stopifnot(identical(dim(x), c(5L, dim(vaas_1))))
#'
#' # OPMS+list method
#' dim(x <- vaas_4 + list(vaas_1))
#' stopifnot(identical(dim(x), c(5L, dim(vaas_1))))
#'
setMethod("c", OPMX, function(x, ..., recursive = FALSE) {
  if (missing(..1))
    return(x)
  try_opms.list(c(list(x), ..., recursive = recursive))
}, sealed = SEALED)

setMethod("c", MOPMX, function(x, ..., recursive = FALSE) {
  if (missing(..1))
    return(x)
  y <- as.list(c(x@.Data, ..., recursive = recursive))
  tryCatch(new(class(x), y), error = function(e) y)
}, sealed = SEALED)

#= plus c

#' @rdname c
#' @name plus
#' @exportMethod "+"
#'
setMethod("+", c(OPM, OPM), function(e1, e2) {
  new(OPMS, plates = list(e1, e2))
}, sealed = SEALED)

setMethod("+", c(OPM, OPMS), function(e1, e2) {
  e2@plates <- c(list(e1), e2@plates)
  validObject(e2)
  e2
}, sealed = SEALED)

setMethod("+", c(OPM, MOPMX), function(e1, e2) {
  e2@.Data <- c(list(e1), e2@.Data)
  e2
}, sealed = SEALED)

setMethod("+", c(OPM, "list"), function(e1, e2) {
  new(OPMS, plates = c(list(e1), e2))
}, sealed = SEALED)

setMethod("+", c(OPMS, OPMS), function(e1, e2) {
  e1@plates[seq_along(e2@plates) + length(e1@plates)] <- e2@plates
  validObject(e1)
  e1
}, sealed = SEALED)

setMethod("+", c(OPMS, OPM), function(e1, e2) {
  e1@plates <- c(e1@plates, list(e2))
  validObject(e1)
  e1
}, sealed = SEALED)

setMethod("+", c(OPMS, MOPMX), function(e1, e2) {
  e2@.Data <- c(list(e1), e2@.Data)
  e2
}, sealed = SEALED)

setMethod("+", c(OPMS, "list"), function(e1, e2) {
  new(OPMS, plates = c(e1@plates, e2)) # unnaming also needed
}, sealed = SEALED)

setMethod("+", c(MOPMX, OPMX), function(e1, e2) {
  e1@.Data <- c(e1@.Data, list(e2))
  e1
}, sealed = SEALED)

setMethod("+", c(MOPMX, "ANY"), function(e1, e2) {
  e1@.Data <- c(e1@.Data, as(e2, class(e1))@.Data)
  e1
}, sealed = SEALED)

setMethod("+", c("ANY", MOPMX), function(e1, e2) {
  e2@.Data <- c(as(e1, class(e2))@.Data, e2@.Data)
  e2
}, sealed = SEALED)


################################################################################


## Not an S4 method for flexibility regarding its first argument

#' \acronym{OPMS} constructor
#'
#' Easily build \code{\link{OPMS}} objects.
#'
#' @rdname opms.function
#'
#' @param ... One to several objects which are either potentially nested lists
#'   of \code{\link{OPMS}}, \code{\link{OPM}} or \code{\link{OPMA}} objects, or
#'   really nested lists whose contained lists can be converted to an
#'   \code{\link{OPM}} or \code{\link{OPMA}} object.
#' @param precomputed Logical scalar. If \code{TRUE}, contained lists have
#'   already been converted to one of the three classes. Otherwise, suitable
#'   contained lists will be converted.
#' @param skip Logical scalar. If \code{precomputed} is \code{TRUE}, silently
#'   skip non-list elements of nested lists? If \code{precomputed} is
#'   \code{FALSE}, silently skip objects that do not belong to the three target
#'   classes? Otherwise, an error is generated if such a list element is
#'   encountered.
#' @param group Logical or character scalar. If \code{TRUE}, split the list of
#'   collected \code{\link{OPM}} objects according to the plate type and convert
#'   the contained lists separately if they contain more than one plate;
#'   otherwise just keep the \code{\link{OPM}} object. \code{FALSE} is the
#'   default: all plates are tried to be forced into a single \code{\link{OPMS}}
#'   object. If a character scalar, the name of the plate type to be extracted.
#' @export
#' @return \code{\link{OPMS}} object, or list (\code{\link{MOPMX}} object) of
#'   such objects (and/or \code{\link{OPM}} objects), or \code{\link{OPM}}
#'   object, or \code{NULL}.
#' @family combination-functions
#' @keywords manip
#' @details While otherwise rather flexible, this function will fail to return
#'   an \code{\link{OPMS}} object if \code{group} is set to \code{FALSE} and the
#'   plate types do not match (simply because such \code{\link{OPMS}} objects
#'   are disallowed). But if \code{group} is set to \code{TRUE}, a list
#'   (\code{\link{MOPMX}} object), not a single \code{\link{OPMS}} object will
#'   be returned; and if \code{group} is of mode \sQuote{character}, this
#'   extracts the plate type(s) of interest.
#'
#'   Note that \code{\link{read_opm}} already has plate-type selection options.
#' @examples
#'
#' ## Testing distinct OPM/OPMS combinations -- all should work.
#' ## Note the number of contained plates in the generated objects.
#'
#' (x <- opms()) # 0 objects
#' stopifnot(is.null(x))
#' (x <- opms(group = TRUE)) # 0 also objects
#' stopifnot(is(x, "MOPMX"), length(x) == 0)
#'
#' dim(x <- opms(vaas_1)) # 1 object
#' stopifnot(identical(x, vaas_1))
#' dim(x <- opms(vaas_4, group = plate_type(vaas_4)))
#' stopifnot(identical(x, vaas_4))
#' dim(x <- opms(vaas_4, group = "PM01"))
#' stopifnot(is.null(x)) # no such plate type => empty object!
#'
#' dim(x <- opms(vaas_1, vaas_1)) # 2 objects
#' stopifnot(is(x, "OPMS"), length(x) == 2L)
#' dim(x <- opms(vaas_4, vaas_1))
#' stopifnot(is(x, "OPMS"), length(x) == 5L)
#' dim(x <- opms(vaas_1, vaas_4))
#' stopifnot(is(x, "OPMS"), length(x) == 5L)
#' dim(x <- opms(vaas_4, vaas_4))
#' stopifnot(is(x, "OPMS"), length(x) == 8L)
#'
opms <- function(..., precomputed = TRUE, skip = FALSE, group = FALSE) {
  opms_or_first_or_NULL <- function(x) case(length(x), NULL, x[[1L]],
    new(OPMS, plates = x))
  if (is.character(group)) {
    wanted <- plate_type(group) # for normalization
    group <- TRUE
  } else {
    wanted <- NULL
    group <- as.logical(group)
  }
  # to_opm_list() checks the argument lengths
  result <- to_opm_list.list(list(...), precomputed, skip, group)
  if (is.null(wanted)) {
    if (group)
      new(MOPMX, lapply(result, opms_or_first_or_NULL))
    else
      opms_or_first_or_NULL(result)
  } else # group was TRUE in that case, and to_opm_list() has split the list
    opms_or_first_or_NULL(result[[wanted]])
}


################################################################################

