


################################################################################


#' Classes of the \pkg{validate} package
#'
#' Classes whose members can be generated and manipulated by a \pkg{validate}
#' user.
#'
#' @examples
#'
#' ## overview on the classes
#' showClass("ATOMIC_VALIDATOR")
#' showClass("ATOMIC_VALIDATION")
#' showClass("ATOMIC_VALIDATORS")
#' showClass("ATOMIC_VALIDATIONS")
#' showClass("ELEMENT_VALIDATORS")
#' showClass("ELEMENT_VALIDATIONS")
#'
#' ## conversions with as()
#' showMethods("coerce", classes = c("ATOMIC_VALIDATOR", "ATOMIC_VALIDATION",
#'   "ATOMIC_VALIDATORS", "ATOMIC_VALIDATIONS", "ELEMENT_VALIDATORS",
#'   "ELEMENT_VALIDATIONS"))
#'
#' @docType class
#' @export
#' @aliases ATOMIC_VALIDATOR-class
#' @seealso methods::Methods methods::new
#' @family classes
#' @keywords methods classes
#'
setClass("ATOMIC_VALIDATOR",
  slots = c(how = "character", what = "ANY"),
  prototype = list(how = "min_elems", what = 1L),
  validity = function(object) {
    errs <- NULL
    if (length(object@how) != 1L)
      errs <- c(errs, "'how' entry must be a character scalar")
    if (!length(object@what))
      errs <- c(errs, "'what' entry must be non-empty")
    if (length(errs))
      errs
    else
      TRUE
  },
  sealed = SEALED
)

#' @rdname ATOMIC_VALIDATOR
#' @name ATOMIC_VALIDATION
#' @aliases ATOMIC_VALIDATION-class
#' @docType class
#' @export
#'
setClass("ATOMIC_VALIDATION",
  contains = "ATOMIC_VALIDATOR",
  slots = c(result = "logical", error = "character"),
  prototype = list(result = NA, error = "no validation has taken place"),
  validity = function(object) {
    errs <- NULL
    if (length(object@result) != 1L)
      errs <- c(errs, "'result' entry must be a logical scalar")
    if (length(object@error) != 1L)
      errs <- c(errs, "'error' entry must be a character scalar")
    if (!length(errs))
      if (is.na(object@result)) {
        if (!nzchar(object@error))
          errs <- c(errs, "validation crashed but 'error' entry is empty")
      } else {
        if (nzchar(object@error))
          errs <- c(errs, "validation worked but 'error' entry is not empty")
      }
    if (length(errs))
      errs
    else
      TRUE
  },
  sealed = SEALED
)

#' @rdname ATOMIC_VALIDATOR
#' @name ATOMIC_VALIDATORS
#' @aliases ATOMIC_VALIDATORS-class
#' @docType class
#' @export
#'
setClass("ATOMIC_VALIDATORS", 
  slots = c(checks = "list"),
  prototype = list(checks = list(new("ATOMIC_VALIDATOR"))),
  validity = function(object) {
    if (all(vapply(object@checks, is, NA, "ATOMIC_VALIDATOR")))
      TRUE
    else
      "not all elements of 'checks' inherit from 'ATOMIC_VALIDATOR'"
  },
  sealed = SEALED
)

#' @rdname ATOMIC_VALIDATOR
#' @name ATOMIC_VALIDATIONS
#' @aliases ATOMIC_VALIDATIONS-class
#' @docType class
#' @export
#'
setClass("ATOMIC_VALIDATIONS", 
  contains = "ATOMIC_VALIDATORS",
  prototype = list(checks = list(new("ATOMIC_VALIDATION"))),
  validity = function(object) {
    if (all(vapply(object@checks, is, NA, "ATOMIC_VALIDATION")))
      TRUE
    else
      "not all elements of 'checks' inherit from 'ATOMIC_VALIDATION'"
  },
  sealed = SEALED
)

#' @rdname ATOMIC_VALIDATOR
#' @name ELEMENT_VALIDATORS
#' @aliases ELEMENT_VALIDATORS-class
#' @docType class
#' @export
#'
setClass("ELEMENT_VALIDATORS",
  contains = "ATOMIC_VALIDATORS",
  slots = c(required = "logical"),
  prototype = list(required = FALSE, checks = list(new("ATOMIC_VALIDATOR"))),
  validity = function(object) {
    if (length(object@required) != 1L)
      "'required' entry must be logical scalar"
    else if (is.na(object@required))
      "'required' entry must not be NA"
    else
      TRUE
  },
  sealed = SEALED
)

#' @rdname ATOMIC_VALIDATOR
#' @name ELEMENT_VALIDATIONS
#' @aliases ELEMENT_VALIDATIONS-class
#' @docType class
#' @export
#'
setClass("ELEMENT_VALIDATIONS",
  contains = "ATOMIC_VALIDATIONS",
  slots = c(present = "logical"),
  prototype = list(required = FALSE, present = FALSE, checks = list()),
  validity = function(object) {
    errs <- NULL
    if (length(object@present) != 1L)
      errs <- c(errs, "'present' entry must be logical scalar")
    else if (is.na(object@present))
      errs <- c(errs, "'present' entry must not be NA")
    if (!length(errs) && !object@present && length(object@checks))
      errs <- c(errs, "object not present but checks conducted")
    if (length(errs))
      errs
    else
      TRUE
  },
  sealed = SEALED
)


################################################################################



