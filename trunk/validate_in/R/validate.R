

################################################################################


#' Validate an \R object
#'
#' Check validity of an \R object using a suitable validator object.
#' 
#' @param object Any \R object. Not all combinations of \code{object} and
#'   \code{validator} are useful, however.
#' @param validator Object of one of the validator classes.
#' @return According validation object (depending on \code{validator}).
#' @export
#' @family validation-functions
#' @keywords utilities
#' @examples
#' ## TODO
#' 
setGeneric("validate", function(object, validator) standardGeneric("validate"))

setMethod("validate", c("ANY", "ATOMIC_VALIDATOR"), function(object,
    validator) {
  check <- tryCatch(list(as(validator, "function")(object), ""),
    condition = function(cond) list(NA, conditionMessage(cond)))
  result <- as(validator, "ATOMIC_VALIDATION")
  result@result <- check[[1L]]
  result@error <- check[[2L]]
  result
}, sealed = SEALED)


################################################################################


