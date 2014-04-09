

################################################################################
################################################################################
#
# Internal auxiliary functions
#


#' Hidden auxiliary methods
#'
#' Methods that are currently not exported.
#'
#' @param object Any \R object. Not all combinations of \code{object} and
#'   \code{validator} are useful, however.
#' @param validator Object of one of the validator classes.
#' @return List.
#' @keywords internal
#'
setGeneric("raw_checks",
  function(object, validator) standardGeneric("raw_checks"))

setMethod("raw_checks", c("ANY", "ATOMIC_VALIDATORS"), function(object,
    validator) {
  lapply(X = validator@checks, FUN = validate, object = object)
}, sealed = SEALED)

setMethod("raw_checks", c("ANY", "MAP_VALIDATOR"), function(object,
    validator) {
  mapply(function(v, n, x) validate(x[[n]], v), validator@checks,
    names(validator@checks), MoreArgs = list(object), SIMPLIFY = FALSE)
}, sealed = SEALED)


################################################################################

