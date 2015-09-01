

################################################################################
################################################################################
#
# Getter functions for the measurements
#


#' Stored measurements
#'
#' Return the measurements, optionally only from selected wells and with or
#' without the time points, or only the time points.
#'
#' @param object \code{\link{OPM}}, \code{\link{OPMS}} or \code{\link{MOPMX}}
#'   object.
#' @param i Optional character or numeric vector with name(s) or position(s) of
#'   well(s). Wells are originally named \sQuote{A01} to \sQuote{H12} but might
#'   have been subset beforehand. \code{i} can also be a formula, allowing for
#'   sequences of well coordinates. See the examples.
#' @param drop Logical scalar. If only a single well was selected, simplify it
#'   to a vector?
#' @param use.names Logical scalar indicating whether the time points should
#'   be used as names for the measurements.
#' @param what Character scalar determining the output mode as follows:
#'   \describe{
#'     \item{all}{Numeric vector: all time points, in order.}
#'     \item{interval}{The difference between each pair of adjacent time
#'       points, \code{NA} if this is irregular or only one time point is left.}
#'     \item{max}{Numeric scalar: the largest time point.}
#'     \item{minmax}{Numeric scalar: the smallest maximum. For \code{\link{OPM}}
#'       objects this is apparently identical to \sQuote{max}.}
#'     \item{size}{Integer scalar: the number of time points.}
#'     \item{summary}{Display a summary.}
#'   }
#' @param ... Optional arguments passed between the methods.
#' @return
#'   \code{measurements} returns a numeric matrix with column names indicating
#'   the well coordinate and a first column containing the time points. The
#'   other columns contain the values from each well. There is one row per time
#'   point. Column names are appropriately set, but not translated (as, e.g., to
#'   substrate names). It is possible to select wells, but the time points are
#'   always included as first column (in contrast to \code{well}). The \code{i}
#'   argument refers only to the remaining matrix.
#'
#'   Do not confuse \code{well} with \code{\link{wells}}. \code{well} yields a
#'   numeric matrix or vector, depending on \code{i} and \code{drop}. It will
#'   always ignore the time points as values, in contrast to
#'   \code{measurements}. But depending on \code{use.names} they would be
#'   inserted as names.
#'
#'   The return value of \code{hours} is dependent on the \code{what} argument;
#'   see there.
#' @export
#' @family getter-functions
#' @keywords attribute
#' @examples
#'
#' # 'OPM' methods
#'
#' head(x <- measurements(vaas_1))[, 1:5] # => numeric matrix
#' stopifnot(is.matrix(x), is.numeric(x))
#' stopifnot(dim(x) == c(384, 97))
#' head(x <- measurements(vaas_1, "B03"))
#' stopifnot(is.matrix(x), is.numeric(x), dim(x) == c(384, 2))
#' head(y <- measurements(vaas_1, ~ B03)) # => same result with formula
#' stopifnot(identical(y, x))
#'
#' head(x <- well(vaas_1, "B04")) # => numeric vector
#' stopifnot(is.numeric(x), length(x) == 384)
#' head(x <- well(vaas_1, c("B08", "C07"))) # => numeric matrix
#' stopifnot(is.matrix(x), dim(x) == c(384, 2))
#' # selecting adjacent wells is easer when using a formula
#' head(x <- well(vaas_1, c("B12", "C01", "C02")))
#' stopifnot(is.matrix(x), dim(x) == c(384, 3))
#' head(y <- well(vaas_1, ~ B12:C02)) # => same result
#' stopifnot(identical(x, y))
#'
#' (x <- hours(vaas_1)) # the default is 'max'
#' stopifnot(identical(x, 95.75))
#' (x <- hours(vaas_1, "minmax"))
#' stopifnot(identical(x, 95.75))
#' (x <- hours(vaas_1, "summary"))
#' stopifnot(is.table(x))
#' (x <- hours(vaas_1, "interval"))
#' stopifnot(identical(x, 0.25))
#' (x <- hours(vaas_1, "size"))
#' stopifnot(identical(x, 384L))
#'
#' # 'OPMS' methods
#'
#' summary(x <- measurements(vaas_4)) # => list of numeric matrices
#' stopifnot(is.list(x), length(x) == length(vaas_4))
#' stopifnot(sapply(x, is.matrix), sapply(x, is.numeric))
#'
#' head(x <- well(vaas_4, "B04"))[, 1:5] # => numeric matrix
#' stopifnot(is.matrix(x), dim(x) == c(4, 384))
#' head(y <- well(vaas_4, ~ B04))[, 1:5] # using a formula
#' stopifnot(identical(x, y)) # => same result
#'
#' (x <- hours(vaas_4)) # all with the same overall running time
#' stopifnot(length(x) == 4, x == 95.75)
#'
setGeneric("measurements",
  function(object, ...) standardGeneric("measurements"))

setMethod("measurements", OPM, function(object, i) {
  if (missing(i))
    object@measurements
  else
    cbind(object@measurements[, 1L, drop = FALSE],
      object@measurements[, -1L, drop = FALSE][,
        well_index(i, colnames(object@measurements)[-1L]), drop = FALSE])
}, sealed = SEALED)

#= well measurements

#' @rdname measurements
#' @export
#'
setGeneric("well", function(object, ...) standardGeneric("well"))

setMethod("well", OPM, function(object, i, drop = TRUE, use.names = TRUE) {
  x <- object@measurements[, -1L, drop = FALSE]
  if (L(use.names))
    rownames(x) <- object@measurements[, 1L]
  x[, well_index(i, colnames(object@measurements)[-1L]), drop = drop]
}, sealed = SEALED)

#= hours measurements

#' @rdname measurements
#' @export
#'
setGeneric("hours", function(object, ...) standardGeneric("hours"))

setMethod("hours", OPM, function(object,
    what = c("max", "all", "size", "summary", "interval", "minmax")) {
  tp <- object@measurements[, HOUR]
  case(match.arg(what),
    all = tp,
    interval = {
      if (length(tp) < 2L)
        NA_real_
      else {
        diffs <- unique(tp[-1L] - tp[-length(tp)])
        if (length(diffs) > 1L)
          NA_real_
        else
          diffs[1L]
      }
    },
    minmax =,
    max = max(tp),
    size = length(tp),
    summary = summary(tp)
  )
}, sealed = SEALED)


################################################################################


## NOTE: "[" is a primitive and needs no setGeneric().


#' Select subset
#'
#' Select a subset of the \code{\link{measurements}} (and, if present, of the
#' \code{\link{aggregated}} data and the \code{\link{discretized}} data) or
#' plates. Return this subset (or these subsets) together with the other slots
#' (which are unchanged).
#'
#' @rdname bracket
#' @exportMethod "["
#' @export
#'
#' @param x \code{\link{OPM}}, \code{\link{OPMA}} or \code{\link{OPMS}} object.
#' @param i Vector or missing. For the \code{\link{OPM}} and \code{\link{OPMA}}
#'   method, the indexes of one to several time points.
#'
#'   For the \code{\link{OPMS}} method, the indexes of one to several plates. A
#'   warning is issued if indexing goes beyond the range. If \code{i} is neither
#'   a numeric nor a logical vector, the \code{\link{OPMS}} method passes it
#'   through \code{\link{infix.q}} to yield a logical vector for indexing. If
#'   \code{i} is a formula, its left side can be used to choose another infix
#'   operator as \code{\link{infix.q}}.
#'
#'   For the \code{\link{MOPMX}} method, either missing or a vector or a formula
#'   or expression. The latter work like for \code{\link{OPMS}} objects and
#'   yield a list of logical vectors (one per element of \code{x}) Such a list
#'   is then further used for selecting subset within the elements of \code{x}.
#'   A list can also be provided directly.
#'
#' @param j Vector or missing. \itemize{
#'   \item For the \code{\link{OPM}} and \code{\link{OPMA}} method, the indexes
#'   or names of one to several wells. Can also be a formula, which allows for
#'   sequences of well coordinates, which are translated to their positions
#'   within the currently present well names. Be aware that this means that the
#'   content of a sequence of well coordinates is dependent on \code{x}!
#'   \item For the \code{\link{OPMS}} method, the indexes of one to several time
#'   points. In that case, if \code{j} is a list, its values are passed to the
#'   respective \code{\link{OPM}} object separately, allowing for individual
#'   choices of time points. Otherwise \code{j} is used as the \code{i} argument
#'   of the \code{\link{OPM}} and \code{\link{OPMA}} method.
#'   }
#' @param k Vector or missing. The \code{\link{OPMS}} method passes \code{k} as
#'   \code{j} argument of the \code{\link{OPM}} and \code{\link{OPMA}} method.
#'   That is, in that case \emph{this} parameter selects the wells. See \code{j}
#'   for details.
#' @param ... This should \strong{not} be set. It is an error to specify
#'   additional dimensions.
#' @param drop Logical scalar. Remove the aggregated data (and the discretised
#'   data, if any) and turn an \code{\link{OPMA}} or \code{\link{OPMD}} object
#'   to an \code{\link{OPM}} object? Has no effect if \code{x} already is an
#'   \code{\link{OPM}} object or contains only such objects. For the
#'   \code{\link{MOPMX}} method, \code{TRUE} means dropping the class and
#'   generating a list.
#' @return \code{\link{OPM}}, \code{\link{OPMA}} or \code{\link{OPMS}} object,
#'   or \code{NULL}.
#'
#' @details The \code{\link{OPMA}} method works like the \code{\link{OPM}} one,
#'   but the function applies the subset creation to the original and the
#'   aggregated data in parallel. The \code{\link{OPMD}} method applies the
#'   selection also to the discretised data.
#'
#'   The aggregated and discretised data may also be dropped entirely; this
#'   might be appropriate if a subset of the time points is selected,
#'   potentially yielding aggregated values that do not fit to the measurements
#'   anymore.
#'
#'   In contrast to the usual `[` methods, with respect to the measurements this
#'   always return a matrix (as a component of the returned object), even if it
#'   could be simplified to a vector. The time column is not counted and always
#'   copied. It is an error to delete the entire matrix. In all other respects,
#'   the \code{\link{OPM}} method behaves like the `[` methods from the
#'   \pkg{base} package.
#'
#'   The \code{\link{OPMS}} method selects a subset of the plates and/or the
#'   measurements of the individual plates. It simplifies the outcome to a
#'   \code{\link{OPM}} or \code{\link{OPMA}} object if only a single plate
#'   remains and to \code{NULL} if no plate remains. This is different from
#'   creating subsets of a list in \R. \code{\link{OPMS}} subset creation rather
#'   behaves like subset creation a three-dimensional array with plates as first
#'   dimension, time points as second, and wells as third.
#'
#' @seealso base::`[` base::`[[`
#' @keywords manip
#'
#' @examples
#'
#' ## OPM(A) method
#'
#' # complete dataset, full 96-well plates
#' (x <- dim(vaas_1))
#' stopifnot(x == c(384, 96))
#'
#' # selecting specific wells
#' copy <- vaas_1[, 11:22]
#' (x <- dim(copy))
#' stopifnot(x == c(384, 12))
#' # indexing with formulae allows for sequences of well coordinates
#' copy <- vaas_1[, ~ A11:B10] # "A11" is 11th, "B10" is 22th well name
#' stopifnot(dim(copy) == c(384, 12)) # same result as above
#' # can also be combined
#' copy <- vaas_1[, ~ A11:22]
#' stopifnot(dim(copy) == c(384, 12)) # same result as above
#'
#' # dropping aggregated data
#' copy <- vaas_1[] # normal selection
#' stopifnot(has_aggr(copy), identical(copy, vaas_1))
#' copy <- vaas_1[drop = TRUE] # selection with dropping
#' stopifnot(!has_aggr(copy), !identical(copy, vaas_1))
#'
#'
#' ## OPMS method
#'
#' # Create OPMS object with fewer plates (the first two ones)
#' (x <- vaas_4[1:2])
#' stopifnot(is(x, "OPMS"), dim(x) == c(2, 384, 96))
#' # we can select the same objects with a formula (which is passed through
#' # the infix-q operator)
#' stopifnot(identical(vaas_4[~ Species == "Escherichia coli"], x))
#' # we can select another infix operator with the left side of the formula
#' stopifnot(identical(vaas_4[k ~ Species], vaas_4))
#'
#' # If only a single plate is selected, this is reduced to OPM(A)
#' x <- vaas_4[3]
#' stopifnot(!is(x, "OPMS"), dim(x) == c(384, 96))
#'
#' # Create OPMS object with fewer time points (the first 100 in that case;
#' # usually this would correspond to the first 25 hours)
#' x <- vaas_4[, 1:100]
#' stopifnot(dim(x) == c(4, 100, 96))
#'
#' # Create OPMS object with fewer wells
#' (x <- vaas_4[, , 1:12])
#' stopifnot(dim(x) == c(4, 384, 12))
#'
#' # The same with well names
#' x <- vaas_4[, , ~ A01:A12] # within x, these are well names 1 to 12
#' stopifnot(dim(x) == c(4, 384, 12))
#' # to do this with a vector, one would need sprintf("A%02i", 1:12)
#'
#' # Select all plates that have aggregated values
#' x <- vaas_4[has_aggr(vaas_4)]
#' stopifnot(identical(x, vaas_4)) # all have such values!
#'
#' # Traverse all contained OPM objects
#' for (i in seq(vaas_4)) { # OR: for (i in 1:length(vaas_4))
#'   x <- vaas_4[i]
#'   # now do something with 'x'...
#'   stopifnot(dim(x) == c(384, 96))
#' }
#' # see also oapply() for a more elegant approach
#'
#' ## MOPMX method
#' (x <- new("MOPMX", list(vaas_1, vaas_4))) # create MOPMX object
#' stopifnot(is(x, "MOPMX"), length(x) == 2)
#' (y <- x[~ Species != "Escherichia coli"])
#' stopifnot(is(y, "MOPMX"), length(y) == 1)
#' (y <- x[list(1, 3:4)]) # only 2nd element reduced
#' stopifnot(is(y, "MOPMX"), length(y) == 2, !identical(x, y))
#'
setMethod("[", c(OPM, "ANY", "ANY", "ANY"), function(x, i, j, ...,
    drop = FALSE) {
  mat <- x@measurements[, -1L, drop = FALSE]
  mat <- mat[i, well_index(j, colnames(mat)), ..., drop = FALSE]
  if (!all(dim(mat)))
    stop("selection resulted in empty matrix")
  mat <- cbind(x@measurements[i, 1L, drop = FALSE], mat)
  names(dimnames(mat)) <- names(dimnames(x@measurements))
  x@measurements <- mat
  x
}, sealed = SEALED)

setMethod("[", c(OPMA, "ANY", "ANY", "ANY"), function(x, i, j, ...,
    drop = FALSE) {
  x <- callNextMethod(x, i, j, ..., drop = drop)
  if (drop)
    return(as(x, OPM))
  if (!missing(j))
    x@aggregated <- x@aggregated[, well_index(j, colnames(x@aggregated)), ...,
      drop = FALSE]
  x
}, sealed = SEALED)

setMethod("[", c(OPMD, "ANY", "ANY", "ANY"), function(x, i, j, ...,
    drop = FALSE) {
  x <- callNextMethod(x, i, j, ..., drop = drop)
  if (drop)
    return(x) # ... which is an OPM object in that case
  if (!missing(j))
    x@discretized <- x@discretized[well_index(j, names(x@discretized))]
  x
}, sealed = SEALED)

setMethod("[", c(OPMS, "ANY", "ANY", "ANY"), function(x, i, j, k, ...,
    drop = FALSE) {
  if (!missing(...))
    stop("incorrect number of dimensions")
  if (missing(i) || identical(i, TRUE)) {
    y <- x@plates
  } else {
    if (!is.logical(i) && !is.numeric(i))
      if (inherits(i, "formula"))
        i <- do.call(formula2infix(i), list(i, x))
      else
        i <- i %q% x
    y <- close_index_gaps(x@plates[i])
    if (!length(y))
      return(NULL)
  }
  k <- well_index(k, colnames(y[[1L]]@measurements)[-1L])
  if (missing(j) || identical(j, TRUE)) {
    # no call of OPM method if j and k are missing/TRUE and drop is FALSE
    if (!identical(k, TRUE) || drop)
      y <- mapply(`[`, x = y, MoreArgs = list(j = k, drop = drop),
        SIMPLIFY = FALSE, USE.NAMES = FALSE)
  } else if (is.list(j)) {
    y <- mapply(`[`, x = y, i = j, MoreArgs = list(j = k, drop = drop),
      SIMPLIFY = FALSE, USE.NAMES = FALSE)
  } else
    y <- mapply(`[`, x = y, MoreArgs = list(i = j, j = k, drop = drop),
      SIMPLIFY = FALSE, USE.NAMES = FALSE)
  if (length(y) == 1L)
    return(y[[1L]])
  x@plates <- y
  x
}, sealed = SEALED)

setMethod("[", c(MOPMX, "missing", "missing", "missing"), function(x, i, j,
    drop) {
  x
}, sealed = SEALED)

setMethod("[", c(MOPMX, "missing", "missing", "ANY"), function(x, i, j,
    drop) {
  if (drop)
    x@.Data
  else
    x
}, sealed = SEALED)

setMethod("[", c(MOPMX, "character", "missing", "missing"), function(x, i, j,
    drop) {
  x@.Data <- close_index_gaps(x@.Data[match(i, names(x))])
  x
}, sealed = SEALED)

setMethod("[", c(MOPMX, "character", "missing", "ANY"), function(x, i, j,
    drop) {
  if (drop) # remove the class, return a list
    return(x@.Data[match(i, names(x))])
  x@.Data <- close_index_gaps(x@.Data[match(i, names(x))]) # keeps the class
  x
}, sealed = SEALED)

setMethod("[", c(MOPMX, "expression", "missing", "missing"), function(x, i, j,
    drop) {
  x[i %q% x]
}, sealed = SEALED)

setMethod("[", c(MOPMX, "expression", "missing", "ANY"), function(x, i, j,
    drop) {
  x[i %q% x, drop = drop]
}, sealed = SEALED)

setMethod("[", c(MOPMX, "formula", "missing", "missing"), function(x, i, j,
    drop) {
  x[do.call(formula2infix(i), list(i, x))]
}, sealed = SEALED)

setMethod("[", c(MOPMX, "formula", "missing", "ANY"), function(x, i, j,
    drop) {
  x[do.call(formula2infix(i), list(i, x)), drop = drop]
}, sealed = SEALED)

setMethod("[", c(MOPMX, "list", "missing", "missing"), function(x, i, j,
    drop) {
  x@.Data <- mapply(do_select, x@.Data, i, SIMPLIFY = FALSE, USE.NAMES = TRUE)
  x@.Data <- close_index_gaps(x@.Data)
  x
}, sealed = SEALED)

setMethod("[", c(MOPMX, "list", "missing", "ANY"), function(x, i, j, drop) {
  x@.Data <- mapply(do_select, x@.Data, i, SIMPLIFY = FALSE, USE.NAMES = TRUE)
  if (drop)
    return(x@.Data)
  x@.Data <- close_index_gaps(x@.Data)
  x
}, sealed = SEALED)

setMethod("[", c(MOPMX, "ANY", "missing", "missing"), function(x, i, j, drop) {
  x@.Data <- close_index_gaps(x@.Data[i])
  x
}, sealed = SEALED)

setMethod("[", c(MOPMX, "ANY", "missing", "ANY"), function(x, i, j, drop) {
  if (drop) # remove the class, return a list
    return(x@.Data[i])
  x@.Data <- close_index_gaps(x@.Data[i]) # keeps the class
  x
}, sealed = SEALED)


################################################################################


## NOTE: 'max' is part of the S4 summary group generic and needs no
## setGeneric().


#' Overall or minimal maximum
#'
#' Get the maximum of all wells or (a) specified one(s), or their smallest
#' maximum. The \code{\link{OPMS}} method works by calling the \code{\link{OPM}}
#' method on all plates and then determining the overall maximum or overall
#' minimum of the maxima.
#'
#' @param x \code{\link{OPM}} or \code{\link{OPMS}} object.
#' @param ... Coordinate of one to several wells. If missing, the maximum or
#'   smallest of all wells is returned. See \code{\link{well}} for details. If
#'   only as single well is selected, the result of \code{minmax} is actually
#'   identical to the one of \code{max}.
#' @param na.rm Logical scalar. See \code{max} from the \pkg{base} package. Has
#'   no effect here because \code{NA} values are not allowed within the
#'   measurements.
#' @return Numeric scalar.
#' @export
#' @seealso base::max base::min
#' @family getter-functions
#' @keywords attribute dplot
#' @examples
#'
#' # OPM method
#' (x <- max(vaas_1))
#' (y <- max(vaas_1, 1)) # this is the negative control
#' stopifnot(x > y) # i.e., some stronger reactions present
#' (x <- minmax(vaas_1))
#' stopifnot(max(vaas_1) > x) # obviously
#'
#' # OPMS method
#' (x <- max(vaas_4))
#' (y <- max(vaas_4, 1)) # this is the negative control
#' stopifnot(x > y) # i.e., some stronger reactions present
#' (x <- minmax(vaas_4))
#' stopifnot(max(vaas_4) > x) # obviously
#'
setMethod("max", OPM, function(x, ..., na.rm = FALSE) {
  if (missing(...))
    max(x@measurements[, -1L, drop = FALSE], na.rm = na.rm)
  else
    max(well(x, ...), na.rm = na.rm)
}, sealed = SEALED)

setMethod("max", OPMS, function(x, ..., na.rm = FALSE) {
  max(vapply(X = x@plates, FUN = max, FUN.VALUE = 1, ..., na.rm = na.rm),
    na.rm = na.rm)
}, sealed = SEALED)

#= minmax max

#' @rdname max
#' @export
#'
setGeneric("minmax", function(x, ...) standardGeneric("minmax"))

setMethod("minmax", OPM, function(x, ..., na.rm = FALSE) {
  min(apply(x@measurements[, -1L, drop = FALSE][, ..., drop = FALSE],
    2L, FUN = max, na.rm = na.rm))
}, sealed = SEALED)

setMethod("minmax", OPMS, function(x, ..., na.rm = FALSE) {
  min(vapply(X = x@plates, FUN = minmax, FUN.VALUE = 1, ..., na.rm = na.rm))
}, sealed = SEALED)


################################################################################


## NOTE: 'dim' and 'length' are primitive and need no setGeneric().

#' Get dimensions
#'
#' Get the dimensions of the measurements of an \code{\link{OPM}} object, or get
#' the dimensions of an \code{\link{OPMS}} object, or the number of plates
#' stored in an \code{\link{OPMX}} object, or the indexes of all these plates.
#'
#' @param x \code{\link{OPMX}} object.
#' @param ... \code{\link{OPMS}} objects. Several ones can be provided, but all
#'   but the first one are ignored. For reasons of comparability, the
#'   \code{\link{OPM}} method of \code{seq} deliberately results in an error.
#' @return For the \code{\link{OPM}} method of \code{dim}, a two-element numeric
#'   vector (number of time points and number of wells). For the
#'   \code{\link{OPMS}} method, a numeric vector with (i) the number of
#'   contained \code{\link{OPM}} objects, and (ii) and (iii) the dimensions of
#'   the first plate. \code{length} returns an integer scalar. This \code{seq}
#'   method yields an integer vector (starting with 1 and at least of length 2).
#' @details
#' Note that \code{dim} cannot be used to determine the correspondence of the
#' time points between all plates as it reports only the time points of the
#' first plate. Instead the \code{\link{OPMS}} method of \code{\link{hours}}
#' must be used.
#'
#' \code{seq} yields the indexes of all plates contained in an
#' \code{\link{OPMS}} object. This is mainly useful for looping over such
#' objects. See \code{\link{[}} for a loop-construct usage example, and note
#' that \code{\link{oapply}} is also available.
#'
#' @export
#' @family getter-functions
#' @keywords attribute
#' @seealso base::dim base::length base::seq
#' @examples
#'
#' # OPM methods
#' (x <- dim(vaas_1))
#' stopifnot(identical(x, c(384L, 96L)))
#' (x <- length(vaas_1))
#' stopifnot(identical(x, 1L)) # 1 plate contained
#' (x <- try(seq(vaas_1), silent = TRUE)) # deliberately yields an error
#' stopifnot(inherits(x, "try-error"))
#'
#' # OPMS methods
#' (x <- dim(vaas_4)) # 2nd value needs not be correct for all plates
#' stopifnot(identical(x, c(4L, 384L, 96L)))
#' (x <- length(vaas_4))
#' stopifnot(identical(x, 4L)) # 4 plates contained
#' (x <- seq(vaas_4))
#' stopifnot(identical(x, 1:4)) # indexes for 4 plates
#' (y <- seq(vaas_4, letters, LETTERS)) # other arguments are ignored
#' stopifnot(identical(x, y))
#'
setMethod("dim", OPM, function(x) {
  dim(measurements(x)[, -1L, drop = FALSE])
}, sealed = SEALED)

setMethod("dim", OPMS, function(x) {
  c(length(x@plates), dim(x@plates[[1L]]))
}, sealed = SEALED)

#= length dim

setMethod("length", WMD, function(x) {
  1L
}, sealed = SEALED)

setMethod("length", "WMDS", function(x) {
  length(x@plates)
}, sealed = SEALED)

#= seq dim

#' @rdname dim
#' @export
#'
setGeneric("seq")

setMethod("seq", WMD, function(...) {
  stop("one cannot loop over an object of class ", class(..1))
}, sealed = SEALED)

setMethod("seq", "WMDS", function(...) {
  seq_along(..1@plates)
}, sealed = SEALED)


################################################################################
################################################################################
#
# Getter functions for the CSV data
#


#' Information from input \acronym{CSV} file
#'
#' Information about the plate as originally read from the input \acronym{CSV}
#' file (see \code{\link{read_opm}} and \code{\link{read_single_opm}} for
#' reading such files).
#'
#' @param object \code{\link{OPM}}, \code{\link{OPMS}} or \code{\link{MOPMX}}
#'   object.
#' @param keys Character vector (or other objects usable as vector index). An
#'   optional sub-selection. If empty (the default), all \acronym{CSV} data are
#'   returned. By default it is an error to select non-existing items. Ignored
#'   unless \code{what} is \sQuote{select}.
#' @param strict Logical scalar indicating whether or not it is an error if
#'   \code{keys} are not found. Ignored unless \code{what} is \sQuote{select}.
#' @param what Character scalar specifying a subset of the data. If
#'   \sQuote{select}, use \code{keys} and \code{strict}. If \sQuote{other},
#'   select all \acronym{CSV} entries that have no special meaning (it makes
#'   sense to include only these in the metadata, see the examples).
#'   Otherwise a shortcut for one of the more important \acronym{CSV} entries.
#' @param normalize Logical scalar indicating whether plate position and setup
#'   time entries (if selected) should be normalised. This should always work
#'   for the plate positions, but for the setup times it depends on the values
#'   for the \code{\link{opm_opt}} keys \code{time.fmt} and \code{time.zone}
#'   (see also \code{\link{merge}}). For other entries, normalisation means
#'   replacing backslashes by slashes.
#' @param ... Optional arguments passed between the methods.
#' @return For the \code{\link{OPM}} method, a named character vector (unnamed
#'   character scalar in the case of \code{filename}, \code{setup_time} and
#'   \code{filename} and if \code{what} is not \sQuote{select}). For the other
#'   methods either a named character vector (if the selection yields a single
#'   entry) or a character matrix with one row per plate. Missing entries in one
#'   of the plates yield \code{NA} in the output.
#' @details It is easy to copy the \acronym{CSV} data to the
#'   \code{\link{metadata}}; see the examples section. Editing of the
#'   \acronym{CSV} data has deliberately not been implemented into \pkg{opm},
#'   but the \code{\link{metadata}} can be modified using a plethora of methods,
#'   even manually with \code{\link{edit}}.
#' @export
#' @seealso base::strptime
#' @family getter-functions
#' @keywords attribute
#' @examples
#'
#' ## 'OPM' method
#'
#' (x <- csv_data(vaas_1, "Setup Time"))
#' stopifnot(identical(x, c(`Setup Time` = "8/30/2010 1:53:08 PM")))
#' # compare this to 'what = "setup_time"'; here, names are kept
#' (y <- csv_data(vaas_1, "Setup Time", normalize = TRUE))
#' stopifnot(!is.na(y), y != x, names(y) == names(x))
#'
#' (x <- csv_data(vaas_1, what = "filename")) # one file name (of course)
#' stopifnot(is.character(x), length(x) == 1L)
#'
#' (x <- csv_data(vaas_1, what = "position")) # single position (of course)
#' (y <- csv_data(vaas_1, what = "position", normalize = TRUE))
#' stopifnot(x == " 7-B", y == "07-B") # four characters when normalised
#'
#' (x <- csv_data(vaas_1, what = "setup_time")) # single setup time (of course)
#' (y <- csv_data(vaas_1, what = "setup_time", normalize = TRUE))
#' stopifnot(length(x) == 1, x != y) # converted to canonical data/time format
#' # WARNING: It is unlikely that all OmniLog output has the setup-time format
#' # defined by default in opm_opt("time.fmt")
#'
#' ## 'OPMS' method
#'
#' (x <- csv_data(vaas_4, "Setup Time")) # one setup time per plate
#' stopifnot(is.character(x), length(x) == 4)
#'
#' (x <- csv_data(vaas_4, what = "filename"))  # one file name per plate
#' stopifnot(is.character(x), length(x) == 4L)
#'
#' (x <- csv_data(vaas_4, what = "position")) # one position per plate
#' stopifnot(is.character(x), length(x) == length(vaas_4))
#'
#' (x <- csv_data(vaas_4, what = "setup_time")) # one setup time per plate
#' (y <- csv_data(vaas_4, what = "setup_time", normalize = TRUE))
#' stopifnot(length(x) == 4, x != y) # converted to canonical data/time format
#' # see the warning above
#'
#' ## Useful application: copying selected CSV data to the metadata
#'
#' x <- vaas_4
#' # this appends the CSV data after conversion to a suitable data frame
#' metadata(x, -1) <- to_metadata(csv_data(x, what = "other"))
#' to_metadata(x)
#' stopifnot(sapply(metadata(x), length) > sapply(metadata(vaas_4), length))
#'
setGeneric("csv_data", function(object, ...) standardGeneric("csv_data"))

setMethod("csv_data", OPM, function(object,
    keys = character(), strict = TRUE,
    what = c("select", "filename", "setup_time", "position", "other"),
    normalize = FALSE) {
  no_backslash <- function(x) gsub("\\",
    "/", x, FALSE, FALSE, TRUE)
  LL(strict, normalize)
  result <- case(match.arg(what),
      select = NULL,
      filename = object@csv_data[[CSV_NAMES[["FILE"]]]],
      setup_time = if (normalize)
          as.character(parse_time(object@csv_data[[CSV_NAMES[["SETUP"]]]]))
        else
          object@csv_data[[CSV_NAMES[["SETUP"]]]],
      position = if (normalize)
          clean_plate_positions(object@csv_data[[CSV_NAMES[["POS"]]]])
        else
          object@csv_data[[CSV_NAMES[["POS"]]]],
      other = if (normalize)
          no_backslash(object@csv_data[!names(object@csv_data) %in% CSV_NAMES])
        else
          object@csv_data[!names(object@csv_data) %in% CSV_NAMES]
    )
  if (length(result))
    return(result)
  if (!length(keys) || all(is.na(keys) | !nzchar(keys))) {
    result <- object@csv_data
  } else {
    result <- object@csv_data[keys]
    if (any(isna <- is.na(result)))
      if (strict)
        stop("could not find key ", keys[isna][1L])
      else
        names(result)[isna] <- keys[isna]
  }
  if (normalize) {
    pos <- match(CSV_NAMES[c("SETUP", "POS")], names(result), 0L)
    if (pos[1L])
      result[pos[1L]] <- as.character(parse_time(result[pos[1L]]))
    if (pos[2L])
      result[pos[2L]] <- clean_plate_positions(result[pos[2L]])
    pos <- setdiff(seq_along(result), pos)
    result[pos] <- no_backslash(result[pos])
  }
  result
}, sealed = SEALED)

setMethod("csv_data", "OPMS", function(object, ...) {
  x <- lapply(X = object@plates, FUN = csv_data, ...)
  if (all(vapply(x, length, 0L) == 1L))
    return(unlist(x, FALSE, TRUE))
  collect_rows(lapply(x, vector2row))
}, sealed = SEALED)

setMethod("csv_data", "MOPMX", function(object, ...) {
  x <- lapply(X = object, FUN = csv_data, ...)
  if (all(is.vec <- !vapply(x, is.matrix, 0L)))
    return(unlist(x, FALSE, TRUE))
  x[is.vec] <- lapply(x[is.vec], vector2row)
  collect_rows(x)
}, sealed = SEALED)


################################################################################
################################################################################
#
# Other getter functions
#


#' Are aggregated or discretised data present?
#'
#' Check whether aggregated or discretised data are present. (See
#' \code{\link{do_aggr}} and \code{\link{do_disc}} for generating such data.)
#' This always returns \code{FALSE} for the \code{\link{OPM}} class, but not
#' necessarily for its child classes.
#'
#' @param object \code{\link{OPM}}, \code{\link{OPMS}} or \code{\link{MOPMX}}
#'   object.
#' @param ... Optional arguments passed between the methods.
#' @return Logical vector, one element per plate.
#' @export
#' @family getter-functions
#' @keywords attribute
#' @examples
#' stopifnot(has_aggr(vaas_1), has_disc(vaas_1)) # OPM methods
#' stopifnot(has_aggr(vaas_4), has_disc(vaas_4)) # OPMS methods
#'
setGeneric("has_aggr", function(object, ...) standardGeneric("has_aggr"))

setMethod("has_aggr", OPM, function(object) {
  .hasSlot(object, "aggregated")
}, sealed = SEALED)

#= has_disc has_aggr

#' @rdname has_aggr
#' @export
#'
setGeneric("has_disc", function(object, ...) standardGeneric("has_disc"))

setMethod("has_disc", OPM, function(object) {
  .hasSlot(object, "discretized")
}, sealed = SEALED)


################################################################################
################################################################################
#
# Getter functions for aggregated data
#


#' Get aggregated data
#'
#' Get the aggregated kinetic data or the aggregation settings used. (See
#' \code{\link{do_aggr}} for generating aggregated data.)
#'
#' @param object \code{\link{OPMA}}, \code{\link{OPMS}} or \code{\link{MOPMX}}
#'   object.
#' @param subset Character vector. If not \code{NULL}, restrict to this or these
#'   parameter(s). See \code{\link{param_names}} for the possible values.
#' @param ci Logical scalar. Include the estimates of confidence intervals
#'   (\acronym{CI}s) in the output?
#' @param trim Character scalar. Parameter estimates from intrinsically negative
#'   reactions (i.e., no respiration) are sometimes biologically unreasonable
#'   because they are too large or too small, and some corrections might be
#'   appropriate. \describe{
#'   \item{no}{No modification.}
#'   \item{full}{Negative
#'   lambda estimates are set to zero.}
#'   \item{medium}{Lambda estimates larger than \code{\link{hours}(object)}
#'   (i.e., the maximum time value observed) are set to that value. Negative
#'   lambda estimates smaller than \code{-hours(object)} are set to this value
#'   (i.e., the negative maximum time).}
#'   \item{full}{Like \sQuote{medium}, but all negative values are set to zero,
#'   which is a less moderate treatment.}
#'   }
#'   Currently the other parameters are not checked, and all \code{NA} values,
#'   if any, also remain unchanged.
#' @param full Logical scalar passed to \code{\link{wells}}. This and the
#'   following arguments affect the column names of the resulting matrix.
#' @param in.parens Logical scalar also passed to that function.
#' @param max Numeric scalar also passed to that function.
#' @param join Empty or character scalar. If empty, a list is returned; a nested
#'   list in the case of \code{\link{OPMS}} objects with one contained list per
#'   plate. Otherwise this nested list is converted to a matrix or data frame,
#'   depending on the value of \code{join}. The following values yield a matrix
#'   in character mode and differ in how they would convert non-scalar values in
#'   a matrix in list mode, if encountered: \describe{
#'     \item{json}{Converts to a \acronym{JSON} string.}
#'     \item{yaml}{Converts to a \acronym{YAML} string.}
#'     \item{rcode}{Converts by deparsing, yielding an \R code string.}
#'   }
#'   All other values of \code{join} are passed as \code{what} argument to the
#'   \code{collect} method form the \pkg{pkgutils} package, with
#'   \code{dataframe} and \code{keep.unnamed} set to \code{TRUE} but
#'   \code{stringsAsFactors} to \code{FALSE}.
#' @param ... Optional arguments passed between the methods or to
#'   \code{\link{wells}}.
#' @export
#' @family getter-functions
#' @details Note that the conversion of the settings list to a matrix or data
#'   frame might not work for all combinations of \code{object} and \code{join},
#'   mainly because the options entry can hold arbitrary content. For similar
#'   conversions of the metadata, see the \code{\link{OPMX}} methods of
#'   \code{\link{to_metadata}}.
#'
#' @return \code{aggregated} yields a numeric matrix of aggregated values
#'   (a.k.a. the curve parameters). If bootstrapping was used, their
#'   \acronym{CI}s are included. The columns represent the wells, the rows the
#'   estimated parameters and their \acronym{CI}s.
#'
#'   \code{aggr_settings} returns a named list if \code{join} is empty. Other
#'   values yield a matrix or data frame (or an error). See the description of
#'   the argument above and the examples below for further details.
#' @keywords attribute
#' @examples
#'
#' # 'OPMA' methods
#' # Get full matrix
#' (x <- aggregated(vaas_1))[, 1:3]
#' stopifnot(is.matrix(x), dim(x) == c(12, 96))
#' (y <- aggregated(vaas_1, full = TRUE))[, 1:3] # full names
#' stopifnot(x == y, nchar(colnames(x)) < nchar(colnames(y)))
#' # Subsetting
#' (x <- aggregated(vaas_1, "lambda"))[, 1:3]
#' stopifnot(is.matrix(x), dim(x) == c(3, 96), any(x < 0))
#' # Now with lambda correction
#' (x <- aggregated(vaas_1, "lambda", trim = "full"))[, 1:3]
#' stopifnot(is.matrix(x), dim(x) == c(3, 96), !any(x < 0))
#'
#' # settings
#' (x <- aggr_settings(vaas_1)) # yields named list
#' stopifnot(is.list(x), !is.null(names(x)))
#' (x <- aggr_settings(vaas_1, join = "json")) # yields a matrix
#' stopifnot(is.matrix(x), is.character(x), nrow(x) == 1)
#'
#' # 'OPMS' methods
#' summary(x <- aggregated(vaas_4)) # => one matrix per OPM object
#' stopifnot(is.list(x), length(x) == length(vaas_4), sapply(x, is.matrix))
#'
#' # settings
#' summary(x <- aggr_settings(vaas_4)) # list of named lists, one per plate
#' stopifnot(is.list(x), length(x) == length(vaas_4), sapply(x, is.list))
#' (x <- aggr_settings(vaas_4, join = "yaml")) # matrix, one row per plate
#' stopifnot(is.matrix(x), is.character(x), nrow(x) == 4)
#'
setGeneric("aggregated", function(object, ...) standardGeneric("aggregated"))

setMethod("aggregated", OPMA, function(object, subset = NULL, ci = TRUE,
    trim = c("no", "full", "medium"), full = FALSE, in.parens = TRUE,
    max = opm_opt("max.chars"), ...) {

  # lambda trimming functions
  trim_into_hours <- function(x, hour, trim) {
    if (trim == "no")
      return(x)
    ok <- !is.na(x)
    x[ok & x > hour] <- hour
    case(trim,
      full = x[ok & x < 0] <- 0,
      medium = x[ok & x < -hour] <- -hour
    )
    x
  }
  trim_mat_into_hours <- function(x, hours, trim) {
    structure(trim_into_hours(x, hours, trim), dim = dim(x),
      dimnames = dimnames(x))
  }
  trim_lambda <- function(x, hours, trim) {
    is.lambda <- grepl(CURVE_PARAMS[2L], rownames(x), FALSE, FALSE, TRUE)
    x[is.lambda, ] <- trim_mat_into_hours(x[is.lambda, , drop = FALSE],
      hours, trim = trim)
    x
  }

  trim <- match.arg(trim)

  # NULL as software entry is allowed to increase backwards compatibility
  if (is.null(software <- object@aggr_settings[[SOFTWARE]]))
    warning(sprintf("object has no '%s' entry", SOFTWARE))
  else if (software != opm_string())
    warning(sprintf("unknown '%s' entry '%s': subset creation may not work",
      SOFTWARE, software))

  # generate subset
  wanted <- unlist(map_param_names(subset, ci))
  result <- object@aggregated[wanted, , drop = FALSE]
  if (CURVE_PARAMS[2L] %in% subset)
    result <- trim_lambda(result, hours(object), trim = trim)

  if (L(full))
    colnames(result) <- map_well_names(wells = colnames(result),
      plate = plate_type(object), in.parens = in.parens, max = max, ...)

  result

}, sealed = SEALED)

#= aggr_settings aggregated

#' @rdname aggregated
#' @export
#'
setGeneric("aggr_settings",
  function(object, ...) standardGeneric("aggr_settings"))

setMethod("aggr_settings", OPMA, function(object, join = NULL) {
  if (length(join))
    list2matrix(list(object@aggr_settings), join)
  else
    object@aggr_settings
}, sealed = SEALED)

setMethod("aggr_settings", OPMS, function(object, join = NULL) {
  result <- lapply(object@plates, slot, "aggr_settings")
  if (length(join))
    list2matrix(result, join)
  else
    result
}, sealed = SEALED)

setMethod("aggr_settings", MOPMX, function(object, join = NULL) {
  result <- lapply(object@.Data, aggr_settings, join)
  if (length(join))
    do.call(rbind, result)
  else
    result
}, sealed = SEALED)


################################################################################


#' Get discretised data
#'
#' Get the discretised kinetic data or the discretisation settings used. (See
#' \code{\link{do_disc}} for generating discretised data.)
#'
#' @param object \code{\link{OPMD}}, \code{\link{OPMS}} or \code{\link{MOPMX}}
#'   object.
#' @param full Logical scalar passed to \code{\link{wells}}. This and the
#'   following arguments affect the names of the resulting vector.
#' @param in.parens Logical scalar also passed to that function.
#' @param max Numeric scalar also passed to that function.
#' @param join Empty or character scalar. Works like the eponymous argument of
#'   \code{\link{aggr_settings}}; see there for details.
#' @param ... Optional arguments passed between the methods or to
#'   \code{\link{wells}}.
#' @export
#' @family getter-functions
#' @return Logical vector or matrix in the case of \code{discretized}, named
#'   list in the case of \code{disc_settings}. See the examples for details.
#' @keywords attribute
#' @examples
#'
#' # 'OPM' methods
#' (x <- discretized(vaas_1))[1:3] # => logical vector
#' stopifnot(is.logical(x), !is.matrix(x), length(x) == dim(x)[2L])
#' stopifnot(names(x) == colnames(aggregated(vaas_1)))
#' (x <- discretized(vaas_1, full = TRUE))[1:3] # => with full names
#' stopifnot(names(x) == colnames(aggregated(vaas_1, full = TRUE)))
#'
#' # settings
#' (x <- disc_settings(vaas_1)) # => named list
#' stopifnot(is.list(x), !is.null(names(x)))
#' (x <- disc_settings(vaas_1, join = "yaml")) # matrix, one row per plate
#' stopifnot(is.matrix(x), is.character(x), nrow(x) == 1)
#'
#' # 'OPMS' methods
#' (x <- discretized(vaas_4))[, 1:3] # => logical matrix
#' stopifnot(is.logical(x), is.matrix(x), ncol(x) == dim(x)[2L])
#' stopifnot(colnames(x) == colnames(aggregated(vaas_1)))
#'
#' # settings
#' summary(x <- disc_settings(vaas_4)) # => list of named lists, one per plate
#' stopifnot(is.list(x), is.null(names(x)), length(x) == length(vaas_4))
#' stopifnot(duplicated(x)[-1])
#' (x <- disc_settings(vaas_4, join = "json")) # matrix, one row per plate
#' stopifnot(is.matrix(x), is.character(x), nrow(x) == 4)
#'
setGeneric("discretized", function(object, ...) standardGeneric("discretized"))

setMethod("discretized", OPMD, function(object, full = FALSE, in.parens = TRUE,
    max = opm_opt("max.chars"), ...) {
  result <- object@discretized
  if (L(full)) # => currently not very efficient for the OPMS methods
    names(result) <- map_well_names(wells = names(result),
      plate = plate_type(object), in.parens = in.parens, max = max, ...)
  result
}, sealed = SEALED)

#= disc_settings discretized

#' @rdname discretized
#' @export
#'
setGeneric("disc_settings",
  function(object, ...) standardGeneric("disc_settings"))

setMethod("disc_settings", OPMD, function(object, join = NULL) {
  if (length(join))
    list2matrix(list(object@disc_settings), join)
  else
    object@disc_settings
}, sealed = SEALED)

setMethod("disc_settings", OPMS, function(object, join = NULL) {
  result <- lapply(object@plates, slot, "disc_settings")
  if (length(join))
    list2matrix(result, join)
  else
    result
}, sealed = SEALED)

setMethod("disc_settings", MOPMX, function(object, join = NULL) {
  result <- lapply(object@.Data, disc_settings, join)
  if (length(join))
    do.call(rbind, result)
  else
    result
}, sealed = SEALED)


################################################################################


#' Select a subset of the plates (or time points)
#'
#' Select a subset of the plates in an \code{\link{OPMS}} object based on the
#' content of the metadata. Alternatively, select a common subset of time points
#' from all plates. \code{thin_out} keeps only a regular subset of the time
#' points from \code{\link{OPM}} measurements. This is a mainly experimental
#' function that might be of use in testing.
#'
#' @param x \code{\link{OPMX}} or \code{\link{MOPMX}} object.
#' @param query Logical or numeric vector or object accepted as query by the
#'   infix operators. If a logical or numeric vector, \code{query} is directly
#'   used as the first argument of \code{\link{[}}, and all following arguments,
#'   if any, are ignored. If otherwise, it is used for conducting a query based
#'   on one of the infix operators as described below.
#' @param values Logical scalar. If \code{TRUE}, the values of \code{query} are
#'   also considered (by using \code{\link{infix.q}} or
#'   \code{\link{infix.largeq}}). If \code{FALSE} only the keys are considered
#'   (by using \code{\link{infix.k}}).
#'
#'   That is, choose either the plates for which certain metadata entries
#'   contain certain values, or choose the plates for which these metadata have
#'   been set at all (to some arbitrary value). See the mentioned functions for
#'   details, and note the special behaviour if \code{query} is a character
#'   vector and \code{values} is \code{FALSE}.
#' @param invert Logical scalar. If \code{TRUE}, return the plates for which the
#'   condition is not \code{TRUE}.
#' @param exact Logical scalar. If the values of \code{query} are considered,
#'   should this be done using \code{\link{infix.q}} (when \code{FALSE}) or
#'   \code{\link{infix.largeq}} (when \code{TRUE})? See these functions and
#'   \code{\link{contains}}  for details.
#' @param time Logical scalar. If \code{TRUE}, all other arguments are ignored
#'   and the object is reduced to the common subset of time points (measurement
#'   hours and minutes).
#' @param positive Character scalar. If \sQuote{ignore}, not used. Otherwise all
#'   previous arguments except \code{object} are ignored, and the function
#'   yields an error unless \code{object} has been discretised throughout, i.e.
#'   either it is an \code{\link{OPMD}} object or all elements of \code{object}
#'   have discretised values.
#'
#'   If \code{object} has the necessary discretised values, if \sQuote{any},
#'   wells are selected that contain positive reactions in at least one plate.
#'   If \sQuote{all}, wells are selected that contain positive reactions in all
#'   plates. Using \code{invert} means selecting all negative or weak reactions.
#' @param negative Character scalar. Like \code{positive}, but returns the
#'   negative reactions. Using \code{invert} means selecting all positive or
#'   weak reactions.
#' @param use Character scalar. An alternative way to specify the settings.
#'
#'   If \sQuote{i} or \sQuote{I}, ignored. If \sQuote{t} or \sQuote{T},
#'   \code{time} is set to \code{TRUE}. If \sQuote{p} or \sQuote{P},
#'   \code{positive} is set to \sQuote{any} or \sQuote{all}, respectively. If
#'   \sQuote{n} or \sQuote{N}, \code{negative} is set to \sQuote{any} or
#'   \sQuote{all}, respectively.
#'
#'   Otherwise, \code{use} is taken directly as the one-latter name of the infix
#'   operators to use for plate selection, overriding \code{values} and
#'   \code{exact}.
#'
#' @param object \code{\link{OPMX}} object.
#' @param factor Numeric scalar >= 1 indicating how much the data set shall be
#'   thinned out.
#' @param drop Logical scalar. See \code{\link{[}}.
#' @param ... Optional arguments passed between the methods.
#'
#' @export
#' @return \code{NULL} or \code{\link{OPM}} or \code{\link{OPMS}} object. This
#'   depends on how many plates are selected; see \code{\link{[}} for details.
#'   The \code{\link{MOPMX}} method always returns a \code{\link{MOPMX}} object.
#' @details The \code{\link{MOPMX}} method creates subsets of all contained
#'   \code{\link{OPMX}} objects (if any) in turn and then removes those that
#'   yielded \code{NULL}. Thus \code{subset} is not intended for directly
#'   creating subsets of \code{\link{MOPMX}} but of their elements to yield,
#'   e.g., elements that have a common set of \code{\link{metadata}} entries, as
#'   required under most circumstances by some other \code{\link{MOPMX}} methods
#'   such as \code{\link{extract}}.
#'
#'   Thinning the plates out is experimental insofar as it has \strong{not} been
#'   tested whether and how this could sensibly be applied before aggregating
#'   the data.
#' @family getter-functions
#' @keywords manip
#' @seealso base::`[` base::`[[` base::subset
#' @examples
#'
#' # simple object comparison function
#' mustbe <- function(a, b) stopifnot(identical(a, b))
#'
#' # all plates have that entry: selection identical to original object
#' mustbe(vaas_4, vaas_4["Species" %k% vaas_4, ])
#' mustbe(vaas_4, subset(vaas_4, list(Species = "Escherichia coli"),
#'   values = FALSE)) # equivalent
#' mustbe(vaas_4, subset(vaas_4, ~ Species == "Escherichia coli",
#'   values = FALSE)) # also equivalent
#'
#' # two plates also have that value: yielding OPMS object with only two plates
#' mustbe(vaas_4[1:2], vaas_4[list(Species = "Escherichia coli") %q% vaas_4, ])
#' mustbe(vaas_4[1:2], subset(vaas_4, list(Species = "Escherichia coli")))
#' mustbe(vaas_4[1:2], subset(vaas_4, ~ Species == "Escherichia coli"))
#'
#' # these are also equivalent
#' mustbe(vaas_4[c(1, 3)],
#'   vaas_4[list(Strain = c("DSM18039", "DSM1707")) %q% vaas_4])
#' mustbe(vaas_4[c(1, 3)],
#'   subset(vaas_4, list(Strain = c("DSM18039", "DSM1707"))))
#' mustbe(vaas_4[c(1, 3)],
#'   subset(vaas_4, ~ Strain %in% c("DSM18039", "DSM1707")))
#' mustbe(vaas_4[c(1, 3)],
#'   subset(vaas_4, ~ Strain == "DSM18039" || Strain == "DSM1707"))
#' # note that particularly formulae can be used to set up very complex queries
#'
#' # select all plates that have aggregated values
#' dim(x <- subset(vaas_4, has_aggr(vaas_4)))
#' mustbe(x, vaas_4) # all have such values
#'
#' # select a common set of time points
#' dim(x <- subset(vaas_4, time = TRUE))
#' mustbe(x, vaas_4) # the time points had already been identical
#' # create unequal time points
#' dim(copy <- vaas_4[, list(1:10, 1:20, 1:15, 1:10)])
#' mustbe(hours(copy), c(2.25, 4.75, 3.50, 2.25))
#' # now restrict to common subset
#' dim(x <- subset(copy, time = TRUE))
#' mustbe(hours(x), rep(2.25, 4))
#' # see also the example with split() given under "["
#'
#' # select all wells that have positive reactions
#' dim(x <- subset(vaas_4, use = "p")) # in at least one plate
#' stopifnot(dim(x)[3] < dim(vaas_4)[3])
#' dim(y <- subset(vaas_4, use = "P")) # in all plates
#' stopifnot(dim(y)[3] < dim(x)[3])
#'
#' # select all wells that have non-negative reactions in at least one plate
#' dim(y <- subset(vaas_4, use = "N", invert = TRUE))
#' stopifnot(dim(y)[3] > dim(x)[3])
#'
#' ## thin_out()
#'
#' # 'OPM' method
#' (x <- dim(vaas_1))
#' stopifnot(identical(x, c(384L, 96L)))
#' copy <- thin_out(vaas_1, 10) # keep every 10th time point and measurement
#' (x <- dim(copy))
#' stopifnot(identical(x, c(38L, 96L)), has_aggr(copy))
#' copy <- thin_out(vaas_1, 10, drop = TRUE) # also remove the parameters
#' (x <- dim(copy))
#' stopifnot(identical(x, c(38L, 96L)), !has_aggr(copy))
#'
#' # 'OPMS' method
#' (x <- dim(vaas_4))
#' stopifnot(identical(x, c(4L, 384L, 96L)))
#' copy <- thin_out(vaas_4, 10)
#' (x <- dim(copy))
#' stopifnot(identical(x, c(4L, 38L, 96L)))
#'
setGeneric("subset")

setMethod("subset", OPMX, function(x, query, values = TRUE,
    invert = FALSE, exact = FALSE, time = FALSE,
    positive = c("ignore", "any", "all"),
    negative = c("ignore", "any", "all"),
    use = c("i", "I", "k", "K", "n", "N", "p", "P", "q", "Q", "t", "T")) {
  if (missing(use))
    LL(values, invert, exact, time)
  else
    reassign_args_using(match.arg(use))
  case(negative <- match.arg(negative),
    ignore = NULL,
    any =,
    all = return(select_by_disc(x, TRUE, invert, negative))
  )
  case(positive <- match.arg(positive),
    ignore = NULL,
    any =,
    all = return(select_by_disc(x, FALSE, invert, positive))
  )
  if (time)
    return(common_times(x))
  if (!is.logical(query) && !is.numeric(query)) {
    query <- if (values) {
        if (exact)
          query %Q% x
        else
          query %q% x
      } else if (exact) {
        query %K% x
      } else {
        query %k% x
      }
    if (invert)
      query <- !query
  }
  do_select(x, query)
}, sealed = SEALED)

setMethod("subset", MOPMX, function(x, query, ...) {
  x@.Data <- lapply(X = x@.Data, FUN = subset, query = query, ...)
  x@.Data <- close_index_gaps(x@.Data)
  x
}, sealed = SEALED)

#= thin_out subset

#' @rdname subset
#' @export
#'
setGeneric("thin_out", function(object, ...) standardGeneric("thin_out"))

setMethod("thin_out", OPM, function(object, factor, drop = FALSE) {
  if (L(factor) < 1)
    stop("'factor' must be >= 1")
  idx <- seq_len(dim(object)[1L])
  idx <- idx[idx %% factor == 0L]
  object[idx, , drop = drop]
}, sealed = SEALED)

setMethod("thin_out", OPMS, function(object, ...) {
  new(OPMS, plates = lapply(X = object@plates, FUN = thin_out, ...))
}, sealed = SEALED)

setMethod("thin_out", MOPMX, function(object, ...) {
  object@.Data <- lapply(X = object@.Data, FUN = thin_out, ...)
  object
}, sealed = SEALED)


################################################################################


#' Determine duplicated plates
#'
#' Check whether some, or duplicated, \code{\link{OPM}} objects are contained
#' within an \code{\link{OPMS}} object, or whether \code{\link{OPMX}} objects
#' are contained within an \code{\link{MOPMX}} object. For reasons of
#' consistency, the \code{\link{OPM}} methods always returns \code{FALSE} or
#' \code{0}. Alternatively, query \code{\link{OPMX}} objects with other such
#' objects.
#'
#' @param x \code{\link{OPMX}} or \code{\link{MOPMX}} object.
#' @param incomparables Vector passed to \code{duplicated} from the \pkg{base}
#'   package. By default this is \code{FALSE}.
#' @param what Indicating which parts of \code{x} should be compared. If a
#'   character scalar, the following entries are special: \describe{
#'   \item{all}{Compares entire \code{OPM} objects.}
#'   \item{csv}{Compares the \acronym{CSV} data entries \code{setup_time} and
#'   \code{position} (see \code{\link{csv_data}}). Not for \code{\link{MOPMX}}
#'   objects.}
#'   \item{metadata}{Compares the entire metadata content.}
#'   \item{plate.type}{Compares the plate types (only for \code{\link{MOPMX}}
#'   objects).}
#'   }
#'   If \code{what} does not match any of these, or is not a character scalar at
#'   all, it is passed as \code{key} argument to \code{\link{metadata}}, and the
#'   resulting metadata subsets are compared. The following two arguments are
#'   only relevant in this case.
#' @param exact Logical scalar passed to \code{\link{metadata}}.
#' @param strict Logical scalar passed to \code{\link{metadata}}.
#' @param ... Optional arguments passed to \code{duplicated} from the \pkg{base}
#'   package. For \code{contains}, optional arguments passed to \code{identical}
#'   from the \pkg{base} package, allowing for fine-control of identity.
#' @param object \code{\link{OPMX}} object.
#' @param other For the \code{\link{OPMX}} method of \code{contains}, an
#'   \code{\link{OPMX}} object used as query.
#' @export
#' @return Logical vector in the case of \code{duplicated}, integer scalar in
#'   the case of \code{anyDuplicated}. \code{0} if no values are duplicated, the
#'   index of the first or last (depending on \code{fromLast}) duplicated object
#'   otherwise. \code{contains} returns a logical vector.
#' @details The \code{\link{OPMS}} and \code{\link{OPM}} methods of
#'   \code{contains} test, for instance, whether an \code{\link{OPM}} object is
#'   contained in an \code{\link{OPMS}} object. The length of the resulting
#'   logical vector is the length of \code{other}.
#' @family getter-functions
#' @keywords attribute
#' @seealso base::duplicated base::anyDuplicated base::identical
#' @examples
#'
#' # 'OPM' methods
#' (x <- duplicated(vaas_1)) # 1 element => nothing duplicated
#' stopifnot(identical(x, FALSE))
#'
#' (x <- anyDuplicated(vaas_1))
#' stopifnot(identical(x, 0L)) # no complete plate duplicated
#' (x <- anyDuplicated(vaas_1, what = list("Strain", "Species")))
#' stopifnot(identical(x, 0L)) # no organisms duplicated
#'
#' # 'OPMS' methods
#' stopifnot(!duplicated(vaas_4)) # => no complete plates duplicated
#' stopifnot(!duplicated(vaas_4, what = list("Species", "Strain")))
#' # => no organisms duplicated
#' stopifnot(duplicated(vaas_4, what = "Species") == rep(c(FALSE, TRUE), 2))
#' # => species duplicated
#' x <- vaas_4[c(1, 1)] # => complete plate duplicated
#' stopifnot(c(FALSE, TRUE) == duplicated(x))
#'
#' stopifnot(identical(anyDuplicated(vaas_4), 0L))
#' stopifnot(identical(anyDuplicated(vaas_4, what = list("Strain")), 0L))
#' # => no strains duplicated
#' stopifnot(identical(anyDuplicated(vaas_4, what = list("Species")), 2L))
#' # => species duplicated
#' x <- vaas_4[c(1, 1)] # complete plate duplicated
#' stopifnot(identical(anyDuplicated(x), 2L))
#'
#' ## contains: 'OPMS'/'OPM' methods
#' (x <- contains(vaas_4, vaas_4[3])) # single one contained
#' stopifnot(length(x) == 1, x)
#' (x <- contains(vaas_4, vaas_4))
#' stopifnot(length(x) == 4, x) # all contained
#' (x <- contains(vaas_4[3], vaas_4)) # one of four contained
#' stopifnot(length(x) == 4, sum(x) == 1)
#' stopifnot(contains(vaas_4[3], vaas_4[3])) # identical OPM objects
#' stopifnot(!contains(vaas_4[3], vaas_4[2])) # non-identical OPM objects
#'
setGeneric("duplicated")

setMethod("duplicated", c(OPM, "missing"), function(x, incomparables, ...) {
  duplicated(x = x, incomparables = FALSE, ...)
}, sealed = SEALED)

setMethod("duplicated", c(OPMS, "missing"), function(x, incomparables, ...) {
  duplicated(x = x, incomparables = FALSE, ...)
}, sealed = SEALED)

setMethod("duplicated", c(OPM, "ANY"), function(x, incomparables, ...) {
  FALSE
}, sealed = SEALED)

setMethod("duplicated", c(OPMS, "ANY"), function(x, incomparables,
    what = c("all", "csv", "metadata"), exact = TRUE, strict = FALSE, ...) {
  selection <- tryCatch(match.arg(what), error = function(e) "other")
  duplicated(x = case(selection,
    all = x@plates,
    csv = cbind(csv_data(x, what = "setup_time"),
      csv_data(x, what = "position")),
    metadata = metadata(x),
    other = metadata(object = x, key = what, exact = exact, strict = strict)
  ), incomparables = incomparables, ...)
}, sealed = SEALED)

setMethod("duplicated", c(MOPMX, "missing"), function(x, incomparables, ...) {
  duplicated(x = x, incomparables = FALSE, ...)
}, sealed = SEALED)

setMethod("duplicated", c(MOPMX, "ANY"), function(x, incomparables,
    what = c("all", "plate.type", "metadata"), exact = TRUE, strict = FALSE,
    ...) {
  selection <- tryCatch(match.arg(what), error = function(e) "other")
  duplicated(x = case(selection,
    all = x@.Data,
    metadata = metadata(x),
    other = metadata(object = x, key = what, exact = exact, strict = strict),
    plate.type = plate_type(x)
  ), incomparables = incomparables, ...)
}, sealed = SEALED)

#' @rdname duplicated
#' @export
#'
setGeneric("anyDuplicated")

#= anyDuplicated duplicated

setMethod("anyDuplicated", c(OPM, "missing"), function(x, incomparables, ...) {
  anyDuplicated(x = x, incomparables = FALSE, ...)
}, sealed = SEALED)

setMethod("anyDuplicated", c(OPMS, "missing"), function(x, incomparables, ...) {
  anyDuplicated(x = x, incomparables = FALSE, ...)
}, sealed = SEALED)


setMethod("anyDuplicated", c(OPM, "ANY"), function(x, incomparables, ...) {
  0L
}, sealed = SEALED)

setMethod("anyDuplicated", c(OPMS, "ANY"), function(x, incomparables, ...) {
  dups <- which(duplicated(x = x, incomparables = incomparables, ...))
  case(length(dups), 0L, dups[1L])
}, sealed = SEALED)

setMethod("anyDuplicated", c(MOPMX, "missing"), function(x, incomparables,
    ...) {
  anyDuplicated(x = x, incomparables = FALSE, ...)
}, sealed = SEALED)

setMethod("anyDuplicated", c(MOPMX, "ANY"), function(x, incomparables, ...) {
  dups <- which(duplicated(x = x, incomparables = incomparables, ...))
  case(length(dups), 0L, dups[1L])
}, sealed = SEALED)

#= contains duplicated

#' @rdname duplicated
#' @export
#'
setGeneric("contains")

setMethod("contains", c(OPMS, OPM), function(object, other, ...) {
  for (plate in object@plates)
    if (identical(x = plate, y = other, ...))
      return(TRUE)
  FALSE
}, sealed = SEALED)

setMethod("contains", c(OPMS, OPMS), function(object, other, ...) {
  single_contained <- function(x) {
    for (plate in object@plates)
      if (identical(x = plate, y = x, ...))
        return(TRUE)
    FALSE
  }
  vapply(other@plates, single_contained, NA)
}, sealed = SEALED)

setMethod("contains", c(OPM, OPMS), function(object, other, ...) {
  mapply(identical, y = other@plates, MoreArgs = list(x = object, ...),
    SIMPLIFY = TRUE, USE.NAMES = FALSE)
}, sealed = SEALED)

setMethod("contains", c(OPM, OPM), function(object, other, ...) {
  identical(x = object, y = other, ...)
}, sealed = SEALED)

setMethod("contains", c(OPMX, MOPMX), function(object, other, ...) {
  FALSE
}, sealed = SEALED)

setMethod("contains", c(MOPMX, OPMX), function(object, other, ...) {
  for (elem in object@.Data)
    if (all(contains(elem, other, ...)))
      return(TRUE)
  FALSE
}, sealed = SEALED)

setMethod("contains", c(MOPMX, MOPMX), function(object, other, ...) {
  vapply(X = other@.Data, FUN = contains, FUN.VALUE = NA, object = object, ...)
}, sealed = SEALED)


################################################################################


# OPM methods with function(object, ...) signature that can conditionally be
# simplified.
#
lapply(c(
    #+
    aggregated,
    discretized,
    has_aggr,
    has_disc,
    hours,
    measurements,
    well
    #-
  ), FUN = function(func_) {
  setMethod(func_, OPMS, function(object, ...) {
    simplify_conditionally(lapply(X = object@plates, FUN = func_, ...))
  }, sealed = SEALED)
})


# MOPMX methods with function(object, ...) signature that can conditionally be
# simplified.
#
lapply(c(
    #+
    aggregated,
    discretized,
    has_aggr,
    has_disc,
    hours,
    measurements,
    metadata,
    well
    #-
  ), FUN = function(func_) {
  setMethod(func_, MOPMX, function(object, ...) {
    simplify_conditionally(lapply(object@.Data, FUN = func_, ...))
  }, sealed = SEALED)
})


################################################################################
################################################################################
#
# Infix operators
#


#' Query metadata keys
#'
#' Search for the presence of metadata keys, either using a vector, factor,
#' list, formula, expression or \code{\link{WMD}} object.
#'
#' @name %k%
#' @aliases infix.k
#' @rdname infix.k
#'
#' @param x Character vector, factor, list, formula, expression or
#'   \code{\link{WMD}} object used as query. See \sQuote{Details}. \code{x} and
#'   \code{table} can swap their places.
#' @param table \code{\link{WMD}}, \code{\link{WMDS}} or \code{\link{MOPMX}}
#'   object. \code{x} and \code{table} can swap their places.
#' @return Logical vector of the length of the \code{\link{WMD}} or
#'   \code{\link{WMDS}} object. For \code{\link{MOPMX}} objects, a list of such
#'   vectors.
#' @exportMethod "%k%"
#' @export
#~ @family getter-functions
#' @keywords attribute
#'
#' @details The behaviour of these methods depends on the object used as query.
#'   \code{infix.largek} is usually stricter than \code{infix.k}, sometimes
#'   equivalent.
#' \itemize{
#'   \item Using a character vector as query, \code{infix.k} tests whether all
#'   given keys are present in the top-level names of the metadata (these may be
#'   nested, but all contained lists are ignored here). An empty query vector
#'   results in \code{TRUE}. Note that the values of the character vector, not
#'   its names, if any, are used for querying the metadata. In contrast,
#'   \code{infix.largek} tests whether a given key is present in the metadata
#'   and fetches an object that is not \code{NULL}. If the key has a length > 1,
#'   contained lists are queried.
#'
#'   \item Using a list as query, both methods tests whether all given keys are
#'   present in the names of the metadata. This works like the character method,
#'   but because a query list is given, the comparison of keys can be applied
#'   recursively (by using, of course, a nested query list). This is based on
#'   \code{\link{contains}} with the \code{values} argument set to \code{FALSE}.
#'
#'   \item When supplying a \code{\link{WMD}} object as query, its metadata will
#'   be used in a list query.
#'
#'   \item The factor method first converts \code{x} to \sQuote{character} mode.
#'
#'   \item The formula method attempts to evaluate the right side of the formula
#'   in the context of the metadata of \code{table} and returns whether or not
#'   this fails (yields an error). Symbols that are not found within the
#'   metadata are looked up in the enclosing environment \code{infix.k} or only
#'   in the base environment \code{infix.largek}. But note also that missing
#'   objects are not the only potential reason of failure.
#'
#'   \item The expression method works like the formula method, using the
#'   entire expression in place of the right side of the formula.
#' }
#' See \code{\link{subset}} for usage examples with \code{\link{OPMS}} objects.
#'
#' @examples
#'
#' # The data set contains the metadata keys 'Species' and 'Experiment' but
#' # neither 'Trial' nor 'Organism' nor 'Run':
#' # In the following we use stopifnot(), which fails unless all arguments
#' # passed are TRUE.
#'
#' ## Character methods
#'
#' # Zero-element queries
#' stopifnot(character() %k% vaas_1) # always results
#' stopifnot(character() %K% vaas_1)
#'
#' # Single-element queries
#' stopifnot("Experiment" %k% vaas_1) # present
#' stopifnot("Experiment" %K% vaas_1) # present
#' stopifnot("Species" %k% vaas_1) # present
#' stopifnot("Species" %K% vaas_1)  # present
#' stopifnot(!"Run" %k% vaas_1) # missing
#' stopifnot(!"Run" %K% vaas_1) # missing
#' stopifnot(!"Organism" %k% vaas_1) # missing
#' stopifnot(!"Trial" %K% vaas_1) # missing
#'
#' # Multi-element queries
#' stopifnot(!c("Species", "Trial") %k% vaas_1) # only one present
#' stopifnot(!c("Organism", "Experiment") %k% vaas_1) # only one present
#' stopifnot(c("Species", "Experiment") %k% vaas_1) # all present
#' # querying with %K% and vectors of length > 1 mean nested queries; compare
#' # this to the behaviour of %k%!
#' stopifnot(!c("Species", "Experiment") %K% vaas_1)
#' # i.e. "Experiment" is not within "Species".
#'
#' ## List methods
#'
#' stopifnot(list(Experiment = "whatever") %k% vaas_1) # key present
#' stopifnot(list(Species = "ignored") %k% vaas_1) # key present
#'
#' stopifnot(vaas_1 %k% vaas_1) # obviously
#' stopifnot(vaas_1 %K% vaas_1)
#'
#' # This fails because we query with a named 2nd-order list but the 'Species'
#' # metadata entry is not even a list.
#' stopifnot(!list(Species = list(Genus = "X", Epithet = "Y")) %k% vaas_1)
#'
#' # This is OK because we query with an unnamed 2nd-order list: it has no
#' # names that one would fail to find.
#' stopifnot(list(Species = list("X", "Y")) %k% vaas_1)
#'
#' # More non-nested query examples
#' stopifnot(!list(Run = 99) %k% vaas_1) # key not present
#' stopifnot(list(Species = "?", Experiment = NA) %k% vaas_1) # keys present
#' stopifnot(!list(Species = "?", Trial = NA) %k% vaas_1) # one key missing
#' stopifnot(!list(Organism = "?", Experiment = NA) %k% vaas_1) # likewise
#' stopifnot(list() %k% vaas_1) # empty query always results
#'
#' # Formulae for querying, compare with list examples above
#' stopifnot((~ Experiment) %k% vaas_1) # key present
#' stopifnot((~ Experiment) %K% vaas_1)
#' stopifnot(vaas_1 %k% ~ Experiment) # key present, no parens needed
#' stopifnot(vaas_1 %K% ~ Experiment)
#' stopifnot(vaas_1 %k% ~ Species) # key present, no parens needed
#' stopifnot(vaas_1 %K% ~ Species)
#' stopifnot(!vaas_1 %k% ~ Species$Epithet) # nested key not present
#' stopifnot(!vaas_1 %K% ~ Species$Epithet)
#' stopifnot(!vaas_1 %k% ~ missing.name) # key not present
#' stopifnot(!vaas_1 %K% ~ missing.name)
#' missing.name <- "abc"
#' stopifnot(vaas_1 %k% ~ missing.name) # key found in enclosing environment
#' stopifnot(!vaas_1 %K% ~ missing.name) # enclosing environment ignored
#' rm(missing.name) # tidy up
#'
setGeneric("%k%", function(x, table) standardGeneric("%k%"))

setMethod("%k%", c("character", WMD), function(x, table) {
  all(x %in% names(table@metadata))
}, sealed = SEALED)

setMethod("%k%", c("list", WMD), function(x, table) {
  contains(table@metadata, x, values = FALSE)
}, sealed = SEALED)

setMethod("%k%", c(WMD, WMD), function(x, table) {
  contains(table@metadata, x@metadata, values = FALSE)
}, sealed = SEALED)

setMethod("%k%", c("formula", WMD), function(x, table) {
  tryCatch({
    eval(x[[length(x)]], table@metadata, parent.frame())
    TRUE
  }, error = function(e) FALSE)
}, sealed = SEALED)

setMethod("%k%", c("expression", WMD), function(x, table) {
  tryCatch({
    eval(x, table@metadata, parent.frame())
    TRUE
  }, error = function(e) FALSE)
}, sealed = SEALED)

#= infix.largek infix.k

#' @name %K%
#' @aliases infix.largek
#' @rdname infix.k
#' @exportMethod "%K%"
#' @export
#'
setGeneric("%K%", function(x, table) standardGeneric("%K%"))

setMethod("%K%", c("character", WMD), function(x, table) {
  if (!length(x))
    return(TRUE) # for consistency with %k%
  tryCatch(!is.null(table@metadata[[x]]), error = function(e) FALSE)
}, sealed = SEALED)

setMethod("%K%", c("list", WMD), function(x, table) {
  contains(table@metadata, x, values = FALSE)
}, sealed = SEALED)

setMethod("%K%", c(WMD, WMD), function(x, table) {
  contains(table@metadata, x@metadata, values = FALSE)
}, sealed = SEALED)

setMethod("%K%", c("formula", WMD), function(x, table) {
  tryCatch({
    eval(x[[length(x)]], table@metadata, baseenv())
    TRUE
  }, error = function(e) FALSE)
}, sealed = SEALED)

setMethod("%K%", c("expression", WMD), function(x, table) {
  tryCatch({
    eval(x, table@metadata, baseenv())
    TRUE
  }, error = function(e) FALSE)
}, sealed = SEALED)


################################################################################


#' Query metadata
#'
#' Search for the presence of metadata values for given keys, either using a
#' vector, factor, list, formula, expression or \code{\link{WMD}} object.
#'
#' @name %q%
#' @aliases infix.q
#' @rdname infix.q
#'
#' @param x Character vector, factor, list, formula, expression or
#'   \code{\link{WMD}} object used as query. See \sQuote{Details}. \code{x} and
#'   \code{table} can swap their places.
#' @param table \code{\link{WMD}}, \code{\link{WMDS}} or \code{\link{MOPMX}}
#'   object. \code{x} and \code{table} can swap their places.
#' @return Logical vector of the length of the \code{\link{WMD}} or
#'   \code{\link{WMDS}} object. For \code{\link{MOPMX}} objects, a list of such
#'   vectors.
#'
#' @details The behaviour of these methods depends on the object used as query.
#'   \code{infix.largeq} is usually stricter than \code{infix.q}, sometimes
#'   equivalent.
#' \itemize{
#'
#'   \item Using a character vector as query, this tests whether all given query
#'   keys are present in the top-level names of the metadata and refer to the
#'   same query elements. The \code{names} of the vector are used to select
#'   elements from the top level of the metadata. When using \code{infix.q},
#'   these elements are then converted to \sQuote{character} mode before
#'   comparison with the values of \code{x}. A non-empty vector without a
#'   \code{names} attribute is accepted but will always yield \code{FALSE}. In
#'   contrast, an entirely empty vector yields \code{TRUE}.
#'
#'   \item Using a list, a non-exact query with a query list is conducted.  The
#'   comparison is applied recursively using \code{\link{contains}} with the
#'   \code{values} argument set to \code{TRUE} and \code{exact} set to either
#'   \code{FALSE} (\code{infix.q}) or \code{TRUE} (\code{infix.largeq}). The
#'   latter might be too strict for most applications. The main advantage of
#'   using a list over the character-based search is that it allows for a nested
#'   query.
#'
#'   \item When supplying a \code{\link{WMD}} object as query, its metadata will
#'   be used in a list query.
#'
#'   \item The factor method first converts \code{x} to \sQuote{character} mode.
#'
#'   \item The formula method attempts to evaluate the right side of the formula
#'   in the context of the metadata of \code{table} and returns the result. For
#'   the \code{\link{WMD}} method, it is up to the user to ensure that the
#'   result is a logical scalar, but the method would succeed anyway. The
#'   \code{\link{WMDS}} method yields an error unless each plate yields a
#'   logical scalar. Symbols that are not found within the metadata are looked
#'   up in the enclosing environment (\code{infix.q}) or only in the base
#'   environment (\code{infix.largeq}). The former approach is less strict.
#'   Because of missing objects and other reasons the method might nevertheless
#'   fail.
#'
#'   \item The expression method works like the formula method, using the
#'   entire expression in place of the right side of the formula.
#' }
#' See \code{\link{subset}} for usage examples with \code{\link{OPMS}} objects.
#' @exportMethod "%q%"
#' @export
#~ @family getter-functions
#' @keywords attribute
#'
#' @examples
#'
#' # The data set vaas_1 contains the metadata keys 'Species' and 'Experiment'
#' # with the values 'Escherichia coli' and 'First replicate'.
#'
#' ## Character methods
#'
#' stopifnot(!"Experiment" %q% vaas_1) # wrong query here; compare to %k%
#' stopifnot(!"First replicate" %q% vaas_1) # again wrong query
#' stopifnot(c(Experiment = "First replicate") %q% vaas_1) # correct query
#' stopifnot(c(Experiment = "First replicate") %Q% vaas_1)
#'
#' stopifnot(!"Species" %q% vaas_1) # wrong query
#' stopifnot(!"Escherichia coli" %q% vaas_1) # wrong query
#' stopifnot(c(Species = "Escherichia coli") %q% vaas_1) # correct query
#'
#' # This does not work because the value has the wrong type
#' stopifnot(!c(`Plate number` = "6") %Q% vaas_1)
#' # Compare to %q%
#' stopifnot(c(`Plate number` = "6") %q% vaas_1)
#'
#' stopifnot(c(Species = "Escherichia coli",
#'   Experiment = "First replicate") %q% vaas_1) # combined query, all TRUE
#' stopifnot(c(Species = "Escherichia coli",
#'   Experiment = "First replicate") %Q% vaas_1) # all present
#'
#' stopifnot(character() %q% vaas_1) # empty query always results
#' stopifnot(character() %Q% vaas_1)
#'
#' ## List methods
#'
#' stopifnot(list(Experiment = "First replicate") %q% vaas_1)
#' stopifnot(list(Experiment = "First replicate") %Q% vaas_1) # present
#'
#' # Choice among alternatives
#' stopifnot(list(Experiment = c("First replicate",
#'   "Second replicate")) %q% vaas_1) # one of them TRUE
#' stopifnot(!list(Experiment = c("Second replicate",
#'   "Third replicate")) %q% vaas_1) # none of them TRUE
#'
#' # Combined query together with choice among alternatives
#' stopifnot(list(Experiment = c("First replicate", "Second replicate"),
#'   Species = c("Escherichia coli", "Bacillus subtilis")) %q% vaas_1)
#'
#' # Choice among alternatives is not done here: this query fails unless this
#' # two-element vector is contained. Compare to %q%.
#' stopifnot(!list(Experiment = c("First replicate",
#'   "Second replicate")) %Q% vaas_1)
#'
#' stopifnot(list() %q% vaas_1) # empty query always results
#' stopifnot(list() %Q% vaas_1)
#'
#' stopifnot(vaas_1 %q% vaas_1) # obviously
#' stopifnot(vaas_1 %Q% vaas_1)
#'
#' ## Formulae for querying
#'
#' stopifnot((~ Experiment == "First replicate") %q% vaas_1)
#' stopifnot((~ Experiment == "First replicate") %Q% vaas_1)
#' stopifnot(vaas_1 %q% ~ Experiment == "First replicate")
#' stopifnot(vaas_1 %Q% ~ Experiment == "First replicate")
#' stopifnot(vaas_1 %q% ~ Species == "Escherichia coli")
#' stopifnot(vaas_1 %Q% ~ Species == "Escherichia coli")
#' stopifnot(vaas_1 %q% ~ Species != "Bacillus subtilis")
#' stopifnot(vaas_1 %Q% ~ Species != "Bacillus subtilis")
#'
#' x <- try(vaas_1 %q% ~ missing.name == "abc", silent = TRUE) # fails
#' stopifnot(inherits(x, "try-error"))
#' x <- try(vaas_1 %Q% ~ missing.name == "abc", silent = TRUE) # also fails
#' stopifnot(inherits(x, "try-error"))
#' missing.name <- "abc"  # enclosing environment considered or ignored
#' stopifnot(vaas_1 %q% ~ missing.name == "abc")
#' x <- try(vaas_1 %Q% ~ missing.name == "abc", silent = TRUE) # still fails
#' stopifnot(inherits(x, "try-error"))
#' rm(missing.name) # tidy up
#'
#' # examples for OPMS methods are given under subset()
#'
setGeneric("%q%", function(x, table) standardGeneric("%q%"))

setMethod("%q%", c("character", WMD), function(x, table) {
  if (length(keys <- names(x)) == 0L && length(x) > 0L)
    return(FALSE)
  got <- unlist(table@metadata[keys])
  length(x) == length(got) && all(x == got)
}, sealed = SEALED)

setMethod("%q%", c("list", WMD), function(x, table) {
  contains(table@metadata, x, values = TRUE, exact = FALSE)
}, sealed = SEALED)

setMethod("%q%", c(WMD, WMD), function(x, table) {
  contains(table@metadata, x@metadata, values = TRUE, exact = FALSE)
}, sealed = SEALED)

setMethod("%q%", c("formula", WMD), function(x, table) {
  eval(x[[length(x)]], table@metadata, parent.frame())
}, sealed = SEALED)

setMethod("%q%", c("expression", WMD), function(x, table) {
  eval(x, table@metadata, parent.frame())
}, sealed = SEALED)

#= infix.largeq infix.q

#' @name %Q%
#' @aliases infix.largeq
#' @rdname infix.q
#' @exportMethod "%Q%"
#' @export
#'
setGeneric("%Q%", function(x, table) standardGeneric("%Q%"))

setMethod("%Q%", c("character", WMD), function(x, table) {
  if (length(keys <- names(x)) == 0L && length(x) > 0L)
    return(FALSE)
  all(vapply(keys, function(key) identical(x[[key]], table@metadata[[key]]),
    NA))
}, sealed = SEALED)

setMethod("%Q%", c("list", WMD), function(x, table) {
  contains(table@metadata, x, values = TRUE, exact = TRUE)
}, sealed = SEALED)

setMethod("%Q%", c(WMD, WMD), function(x, table) {
  contains(table@metadata, x@metadata, values = TRUE, exact = TRUE)
}, sealed = SEALED)

setMethod("%Q%", c("formula", WMD), function(x, table) {
  eval(x[[length(x)]], table@metadata, baseenv())
}, sealed = SEALED)

setMethod("%Q%", c("expression", WMD), function(x, table) {
  eval(x, table@metadata, baseenv())
}, sealed = SEALED)


################################################################################
#
# Automatically generated factor methods
#

lapply(c(
    #+
    `%k%`,
    `%K%`,
    `%q%`,
    `%Q%`
    #-
  ), FUN = function(func_) {
  setMethod(func_, c("factor", WMD), function(x, table) {
    func_(structure(as.character(x), names = names(x)), table)
  }, sealed = SEALED)
})


################################################################################
#
# Automatically generated WMDS methods
#


# WMD methods with function(x, table, ...) signature (infix operators).
#
lapply(c(
    #+
    `%k%`,
    `%K%`,
    `%q%`,
    `%Q%`
    #-
  ), FUN = function(func_) {
  setMethod(func_, c("list", "WMDS"), function(x, table) {
    vapply(table@plates, func_, NA, x = x, USE.NAMES = FALSE)
  }, sealed = SEALED)
})

# WMD methods with function(x, table, ...) signature (infix operators).
#
lapply(c(
    #+
    `%k%`,
    `%K%`,
    `%q%`,
    `%Q%`
    #-
  ), FUN = function(func_) {
  setMethod(func_, c(WMD, "WMDS"), function(x, table) {
    vapply(table@plates, func_, NA, x = x, USE.NAMES = FALSE)
  }, sealed = SEALED)
})

lapply(c(
    #+
    `%k%`,
    `%K%`,
    `%q%`,
    `%Q%`
    #-
  ), FUN = function(func_) {
  setMethod(func_, c("character", "WMDS"), function(x, table) {
    vapply(table@plates, func_, NA, x = x, USE.NAMES = FALSE)
  }, sealed = SEALED)
})

lapply(c(
    #+
    `%k%`,
    `%K%`,
    `%q%`,
    `%Q%`
    #-
  ), FUN = function(func_) {
  setMethod(func_, c("factor", "WMDS"), function(x, table) {
    vapply(table@plates, func_, NA, x = x, USE.NAMES = FALSE)
  }, sealed = SEALED)
})

lapply(c(
    #+
    `%k%`,
    `%K%`,
    `%q%`,
    `%Q%`
    #-
  ), FUN = function(func_) {
  setMethod(func_, c("formula", "WMDS"), function(x, table) {
    vapply(table@plates, func_, NA, x = x, USE.NAMES = FALSE)
  }, sealed = SEALED)
})

lapply(c(
    #+
    `%k%`,
    `%K%`,
    `%q%`,
    `%Q%`
    #-
  ), FUN = function(func_) {
  setMethod(func_, c("expression", "WMDS"), function(x, table) {
    vapply(table@plates, func_, NA, x = x, USE.NAMES = FALSE)
  }, sealed = SEALED)
})

lapply(c(
    #+
    `%k%`,
    `%K%`,
    `%q%`,
    `%Q%`
    #-
  ), FUN = function(func_) {
  setMethod(func_, c(WMD, "ANY"), function(x, table) {
    func_(table, x)
  }, sealed = SEALED)
})

lapply(c(
    #+
    `%k%`,
    `%K%`,
    `%q%`,
    `%Q%`
    #-
  ), FUN = function(func_) {
  setMethod(func_, c("WMDS", "ANY"), function(x, table) {
    func_(table, x)
  }, sealed = SEALED)
})

################################################################################
#
# Repetitions are necessary here to avouid ambiguity in method dispatch.
#

lapply(c(
    #+
    `%k%`,
    `%K%`,
    `%q%`,
    `%Q%`
    #-
  ), FUN = function(func_) {
  setMethod(func_, c(MOPMX, "ANY"), function(x, table) {
    func_(table, x)
  }, sealed = SEALED)
})

lapply(c(
    #+
    `%k%`,
    `%K%`,
    `%q%`,
    `%Q%`
    #-
  ), FUN = function(func_) {
  setMethod(func_, c(MOPMX, WMD), function(x, table) {
    func_(table, x)
  }, sealed = SEALED)
})

lapply(c(
    #+
    `%k%`,
    `%K%`,
    `%q%`,
    `%Q%`
    #-
  ), FUN = function(func_) {
  setMethod(func_, c(MOPMX, "WMDS"), function(x, table) {
    func_(table, x)
  }, sealed = SEALED)
})

lapply(c(
    #+
    `%k%`,
    `%K%`,
    `%q%`,
    `%Q%`
    #-
  ), FUN = function(func_) {
  setMethod(func_, c("ANY", MOPMX), function(x, table) {
    lapply(table@.Data, func_, x = x)
  }, sealed = SEALED)
})

lapply(c(
    #+
    `%k%`,
    `%K%`,
    `%q%`,
    `%Q%`
    #-
  ), FUN = function(func_) {
  setMethod(func_, c(WMD, MOPMX), function(x, table) {
    lapply(table@.Data, func_, x = x)
  }, sealed = SEALED)
})

lapply(c(
    #+
    `%k%`,
    `%K%`,
    `%q%`,
    `%Q%`
    #-
  ), FUN = function(func_) {
  setMethod(func_, c("WMDS", MOPMX), function(x, table) {
    lapply(table@.Data, func_, x = x)
  }, sealed = SEALED)
})

lapply(c(
    #+
    `%k%`,
    `%K%`,
    `%q%`,
    `%Q%`
    #-
  ), FUN = function(func_) {
  setMethod(func_, c(MOPMX, MOPMX), function(x, table) {
    lapply(table@.Data, func_, x = x)
  }, sealed = SEALED)
})


################################################################################

