

################################################################################


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
setGeneric("raw_check",
  function(object, validator) standardGeneric("raw_check"))

setMethod("raw_check", c("ANY", "ATOMIC_VALIDATORS"), function(object,
    validator) {
  lapply(X = validator@checks, FUN = validate, object = object)
}, sealed = SEALED)


################################################################################

