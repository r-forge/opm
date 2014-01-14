

################################################################################


#' Getter methods for \acronym{FAME} and \acronym{FAMES} objects
#'
#' Simple getter methods for retrieving information on, or data from,
#' \code{\link{FAME}} or \code{\link{FAMES}} objects.
#'
#' @param object Object of class \code{\link{FAME}} or \code{\link{FAMES}}.
#' @return \code{plate_type} returns a character scalar, which is \code{NA} if
#'   and only if \code{object} is empty. (Plate types are enforced to be
#'   \strong{constant} within a \code{\link{FAMES}} object.) \code{measurements}
#'   yields a data frame or a list of data frames.
#' @details Other getter methods, such as \code{length}, \code{seq} or
#'   \code{plates} are inherited from \acronym{WMD} or \acronym{WMDS},
#'   respectively. See the \pkg{opm} package for more information.
#' @export
#'
#' @name plate_type
#' @aliases measurements
#'
#' @family getter-functions
#' @seealso \link{summary}
#' @keywords manip
#'
#' @examples
#' (x <- plate_type(DSM_44549))
#' stopifnot(is.character(x), length(x) == 1)
#'
#' summary(x <- measurements(DSM_44549)) # not a very useful format
#' stopifnot(is.list(x), sapply(x, is.data.frame))
#'
NULL

#' @exportMethod plate_type
#' @export
#'
setMethod("plate_type", FAME, function(object) {
  object@plate_type
}, sealed = SEALED)

setMethod("plate_type", FAMES, function(object) {
  if (length(object@plates))
    object@plates[[1L]]@plate_type
  else
    NA_character_
}, sealed = SEALED)

#= measurements plate_type

#' @exportMethod measurements
#' @export
#'
setMethod("measurements", FAME, function(object) {
  object@measurements
}, sealed = SEALED)

setMethod("measurements", FAMES, function(object) {
  lapply(object@plates, slot, "measurements")
}, sealed = SEALED)


################################################################################


#' Select subset
#'
#' Select a subset of the entries of a \code{\link{FAMES}} object.
#'
#' @rdname bracket
#' @aliases double.bracket
#' @exportMethod "["
#' @export
#' @family getter-functions
#'
#' @param x \code{\link{FAMES}} object.
#' @param i Vector used for indexing, or missing. A warning is issued if
#'   indexing goes beyond the range of \code{x}.
#' @param j Missing.
#' @param drop Missing.
#' @param exact Missing.
#' @return \code{\link{FAME}} or \code{\link{FAMES}} object or \code{NULL}.
#' @seealso base::`[` base::`[[`
#' @keywords manip
#'
#' @examples
#' x <- DSM_44549[3:4]
#' length(x)
#' stopifnot(is(x, "FAMES"), length(x) == 2)
#'
#' x <- DSM_44549[-3:-4]
#' length(x)
#' stopifnot(is(x, "FAMES"), length(x) == length(DSM_44549) - 2)
#'
#' x <- DSM_44549[5]
#' length(x)
#' stopifnot(is(x, "FAMES"), length(x) == 1)
#'
#' x <- DSM_44549[[5]] # reduction to FAME object
#' length(x)
#' stopifnot(is(x, "FAME"), length(x) == 1)
#'
#' x <- DSM_44549[10] # beyond the range, yields empty object
#' length(x)
#' stopifnot(is(x, "FAMES"), length(x) == 0)
#'
#' (x <- try(DSM_44549[[10]], TRUE)) # beyond the range, doesn't work
#' stopifnot(inherits(x, "try-error"))
#'
setMethod("[", c(FAMES, "missing", "missing", "missing"), function(x, i, j,
    drop) {
  x
}, sealed = SEALED)

setMethod("[", c(FAMES, "ANY", "missing", "missing"), function(x, i, j, drop) {
  x@plates <- close_index_gaps(x@plates[i])
  x
}, sealed = SEALED)

#= double.bracket bracket

#' @exportMethod "[["
#' @rdname bracket
#' @export
#'
setMethod("[[", c(FAMES, "ANY", "missing"), function(x, i, exact) {
  x@plates[[i]]
}, sealed = SEALED)


################################################################################

