

################################################################################


#' Validate an \R object
#'
#' Check validity of an \R object using a suitable validator object.
#'
#' @param object Any \R object. Not all combinations of \code{object} and
#'   \code{validator} are useful, however.
#' @param validator Object of one of the validator classes.
#' @return According validation object, depending on the \code{validator}
#'   argument. The rule is that a validator of class \code{<X>_VALIDATOR} yields
#'   an object of class \code{<X>_VALIDATION}.
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
  new("ATOMIC_VALIDATION", how = validator@how, what = validator@what,
    result = check[[1L]], error = check[[2L]])
}, sealed = SEALED)

setMethod("validate", c("ANY", "ATOMIC_VALIDATORS"), function(object,
    validator) {
  new("ATOMIC_VALIDATIONS", checks = raw_check(object, validator))
}, sealed = SEALED)

setMethod("validate", c("NULL", "ELEMENT_VALIDATOR"), function(object,
    validator) {
  as(validator, "ELEMENT_VALIDATION")
}, sealed = SEALED)

setMethod("validate", c("ANY", "ELEMENT_VALIDATOR"), function(object,
    validator) {
  new("ELEMENT_VALIDATION", required = validator@required, present = TRUE,
    checks = raw_check(object, validator))
}, sealed = SEALED)


################################################################################

