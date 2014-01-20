


################################################################################


#' Assign subset
#'
#' Assign subsets of \code{\link{FAMES}} objects.
#'
#' @param x \code{\link{FAMES}} object.
#' @param i One to several plate indexes. Should be compatible with the length
#'   of \code{value}. Otherwise any resulting \code{NULL} elements will be
#'   removed (with a warning), causing the resulting plate indexes to be unequal
#'   to \code{i}, which might be confusing.
#' @param j Must \strong{not} be set. See the examples.
#' @param value Value to be assigned. \code{NULL} causes the selected plates to
#'   be removed. Alternatively, \code{\link{FAME}} or \code{\link{FAMES}}
#'   objects or lists of \code{\link{FAME}} objects can be assigned. All
#'   assignments are subject to the restrictions explained in the help entries
#'   of the \code{\link{FAMES}} class.
#' @return \code{value}.
#' @family combination-functions
#' @keywords manip
#' @rdname bracket.set
#' @exportMethod "[<-"
#' @export
#' @examples
#' (copy <- DSM_44549)
#' size <- length(copy)
#' copy[5] <- NULL
#' stopifnot(length(copy) == size - 1)
#' copy[length(DSM_44549)] <- DSM_44549[5]
#' stopifnot(length(copy) == size)
#' copy[[4]] <- NULL
#' stopifnot(length(copy) == size - 1)
#' copy[2:1] <- copy[1:2]
#' stopifnot(length(copy) == size - 1)
#' copy[[3]] <- copy[[4]]
#' stopifnot(length(copy) == size - 1)
#'
setMethod("[<-", c(FAMES, "ANY", "missing", "NULL"), function(x, i, j, value) {
  x@plates[i] <- NULL
  x # no checks necessary here
}, sealed = SEALED)

setMethod("[<-", c(FAMES, "ANY", "missing", FAME), function(x, i, j, value) {
  x@plates[i] <- value
  new(FAMES, plates = close_index_gaps(x@plates)) # checks and unnaming needed
}, sealed = SEALED)

setMethod("[<-", c(FAMES, "ANY", "missing", FAMES), function(x, i, j, value) {
  x@plates[i] <- value@plates
  new(FAMES, plates = close_index_gaps(x@plates)) # checks and unnaming needed
}, sealed = SEALED)

setMethod("[<-", c(FAMES, "ANY", "missing", "list"), function(x, i, j, value) {
  x@plates[i] <- value
  new(FAMES, plates = close_index_gaps(x@plates)) # checks and unnaming needed
}, sealed = SEALED)

#= double.bracket.set bracket.set

#' @exportMethod "[[<-"
#' @rdname bracket.set
#' @export
#'
setMethod("[[<-", c(FAMES, "ANY", "missing", "NULL"), function(x, i, j, value) {
  x@plates[i] <- NULL
  x # no checks necessary here
}, sealed = SEALED)

setMethod("[[<-", c(FAMES, "ANY", "missing", FAME), function(x, i, j, value) {
  x@plates[i] <- value
  new(FAMES, plates = close_index_gaps(x@plates)) # checks and unnaming needed
}, sealed = SEALED)


################################################################################


#' Combination of plates
#'
#' Combine a \code{\link{FAME}} or \code{\link{FAMES}} object with other objects
#' into a \code{\link{FAMES}} object if this is possible, otherwise yield a
#' list.
#'
#' @param x \code{\link{FAME}} or \code{\link{FAMES}} object.
#' @param ... Other \R objects.
#' @param recursive Logical scalar. See \code{c} from the \pkg{base} package.
#'   Setting this ti \code{TRUE} increases the chance to generate a
#'   \code{\link{FAMES}} object because nested lists would be searched
#'   recursively for \code{\link{FAME}} or \code{\link{FAMES}} objects that fit.
#' @export
#' @return
#'   \code{c} creates an \code{\link{FAMES}} object if possible, otherwise a
#'   list, or an \code{\link{FAME}} object (if \code{\dots} is not given and
#'   \code{x} is such an object).
#' @family combination-functions
#' @seealso base::c
#' @keywords manip
#' @examples
#' (x <- c(DSM_44549)) # nothing to combine => no effect
#' stopifnot(identical(x, DSM_44549))
#'
#' (x <- c(DSM_44549[2], DSM_44549[3:4])) # yield 3 elements
#' stopifnot(is(x, "FAMES"), length(x) == 3)
#' # ...it would be easier to use 'DSM_44549[2:4]', of course
#'
#' (x <- c(DSM_44549, letters)) # cannot deeply be combined
#' stopifnot(is.list(x), length(x) == 27)
#' (x <- c(DSM_44549, letters, recursive = TRUE))
#' stopifnot(is.list(x), length(x) == 27)
#'
setMethod("c", FAME, function(x, ..., recursive = FALSE) {
  if (missing(..1))
    return(x)
  join_if_possible(callNextMethod(NULL, x, ..., recursive = recursive))
}, sealed = SEALED)

setMethod("c", FAMES, function(x, ..., recursive = FALSE) {
  if (missing(..1))
    return(x)
  join_if_possible(callNextMethod(NULL, x, ..., recursive = recursive))
}, sealed = SEALED)


################################################################################

