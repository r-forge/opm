

################################################################################


#' Getter methods for \acronym{FAME} and \acronym{FAMES} objects
#'
#' Simple getter methods for retrieving information on, or data from,
#' \code{\link{FAME}} or \code{\link{FAMES}} objects.
#'
#' @param x Object of class \code{\link{FAME}} or \code{\link{FAMES}}.
#' @param object Object of class \code{\link{FAME}} or \code{\link{FAMES}}.
#' @return The \code{length} method returns an integer scalar, \code{plate_type}
#'   a character scalar; \code{plates} yields a list of \code{\link{FAME}}
#'   objects, whereas \code{measurements} yields a data frame or a list of data
#'   frames.
#' @export
#' @name length
#' @aliases plates
#' @aliases plate_type
#' @aliases measurements
#' @family getter-functions
#' @keywords manip
#' @examples
#' (x <- length(DSM_44549))
#' stopifnot(x == 9)
#'
#' (x <- plate_type(DSM_44549))
#' stopifnot(is.character(x), length(x) == 1)
#'
#' summary(x <- measurements(DSM_44549)) # not a very useful format
#' stopifnot(is.list(x), sapply(x, is.data.frame))
#'
#' summary(x <- plates(DSM_44549))
#' stopifnot(is.list(x), length(x) == 9)
#'
setMethod("length", FAME, function(x) {
  1L
}, sealed = SEALED)

setMethod("length", FAMES, function(x) {
  length(x@plates)
}, sealed = SEALED)

#= plate_type length

#' @exportMethod plate_type
#'
setMethod("plate_type", FAME, function(object) {
  object@plate_type
}, sealed = SEALED)

setMethod("plate_type", FAMES, function(object) {
  object@plates[[1L]]@plate_type
}, sealed = SEALED)

#= measurements length

#' @exportMethod measurements
#'
setMethod("measurements", FAME, function(object) {
  object@measurements
}, sealed = SEALED)

setMethod("measurements", FAMES, function(object) {
  lapply(object@plates, slot, "measurements")
}, sealed = SEALED)

#= plates length

#' @exportMethod plates
#'
setMethod("plates", FAME, function(object) {
  list(object)
}, sealed = SEALED)

setMethod("plates", FAMES, function(object) {
  object@plates
}, sealed = SEALED)


################################################################################


## NOTE: "[" is a primitive and needs no setGeneric().


#' Select subset
#'
#' Select a subset of the entries of a \code{\link{FAMES}} object.
#'
#' @rdname bracket
#' @exportMethod "["
#' @export
#'
#' @param x \code{\link{FAMES}} object.
#' @param i Vector used for indexing, or missing. A warning is issued if
#'   indexing goes beyond the range of \code{x}.
#' @param j Missing.
#' @param drop Missing.
#' @return \code{\link{FAME}} or \code{\link{FAMES}} object or \code{NULL}.
#' @seealso base::`[` base::`[[`
#' @keywords manip
#'
#' @examples
#' x <- DSM_44549[3:4]
#' length(x)
#' stopifnot(is(x, "FAMES"), length(x) == 2)
#' x <- DSM_44549[5]
#' length(x)
#' stopifnot(is(x, "FAME"), length(x) == 1)
#' x <- DSM_44549[10] # beyond the range
#' length(x)
#' stopifnot(is.null(x))
#'
setMethod("[", c(FAMES, "missing", "missing", "missing"), function(x, i, j,
    drop) {
  x
}, sealed = SEALED)

setMethod("[", c(FAMES, "ANY", "missing", "missing"), function(x, i, j,
    drop) {
  x@plates <- close_index_gaps(x@plates[i])
  case(length(x@plates), NULL, x@plates[[1L]], x)
}, sealed = SEALED)


################################################################################

