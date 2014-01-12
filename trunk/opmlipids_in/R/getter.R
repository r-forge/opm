

################################################################################


#' Getter methods for \acronym{FAME} and \acronym{FAMES} objects
#'
#' Simple getter methods for retrieving information on, or data from,
#' \code{\link{FAME}} or \code{\link{FAMES}} objects.
#'
#' @param x Object of class \code{\link{FAME}} or \code{\link{FAMES}}.
#' @param object Object of class \code{\link{FAME}} or \code{\link{FAMES}}.
#' @return Character or integer scalar.
#' @export
#' @name length
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
setMethod("length", FAME, function(x) {
  1L
}, sealed = SEALED)

setMethod("length", FAMES, function(x) {
  length(x@plates)
}, sealed = SEALED)

#= plate_type length

setMethod("plate_type", FAME, function(object) {
  object@plate_type
}, sealed = SEALED)

setMethod("plate_type", FAMES, function(object) {
  object@plates[[1L]]@plate_type
}, sealed = SEALED)

#= measurements length

setMethod("measurements", FAME, function(object) {
  object@measurements
}, sealed = SEALED)

setMethod("measurements", FAMES, function(object) {
  lapply(object@plates, slot, "measurements")
}, sealed = SEALED)


################################################################################

