


################################################################################
################################################################################
#
# Classes of the package
#



#' Validator classes of the \pkg{validate} package
#'
#' Classes of whose members can be used for validating (parts of) other \R
#' objects.
#'
#' @examples
#'
#' ## overview on the (non-virtual) classes
#' showClass("ATOMIC_VALIDATOR")
#' showClass("ATOMIC_VALIDATORS")
#' showClass("ELEMENT_VALIDATOR")
#' showClass("COLLECTION_VALIDATOR")
#'
#' ## conversions with as()
#' showMethods("coerce", classes = c("ATOMIC_VALIDATOR", "ATOMIC_VALIDATORS",
#'   "ELEMENT_VALIDATOR", "COLLECTION_VALIDATOR"))
#'
#' @docType class
#' @name validator-classes
#' @seealso methods::Methods methods::new
#' @family classes
#' @keywords methods classes
#'
NULL

#' @rdname validator-classes
#' @name ATOMIC_VALIDATOR
#' @aliases ATOMIC_VALIDATOR-class
#' @docType class
#' @export
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

#' @rdname validator-classes
#' @name VALIDATORS
#' @aliases VALIDATORS-class
#' @docType class
#' @export
#'
setClass("VALIDATORS",
  contains = "VIRTUAL",
  slots = c(checks = "list"),
  sealed = SEALED
)

#' @rdname validator-classes
#' @name PRESENCE_VALIDATOR
#' @aliases PRESENCE_VALIDATOR-class
#' @docType class
#' @export
#'
setClass("PRESENCE_VALIDATOR",
  contains = "VIRTUAL",
  slots = c(required = "logical"),
  prototype = list(required = TRUE),
  validity = function(object) {
    errs <- NULL
    if (length(object@required) != 1L)
      errs <- c(errs, "'required' entry must be logical scalar")
    else if (is.na(object@required))
      errs <- c(errs, "'required' entry must not be NA")
    if (length(errs))
      errs
    else
      TRUE
  },
  sealed = SEALED
)

#' @rdname validator-classes
#' @name ATOMIC_VALIDATORS
#' @aliases ATOMIC_VALIDATORS-class
#' @docType class
#' @export
#'
setClass("ATOMIC_VALIDATORS",
  contains = "VALIDATORS",
  prototype = list(checks = list(new("ATOMIC_VALIDATOR"))),
  validity = function(object) {
    if (all(vapply(object@checks, is, NA, "ATOMIC_VALIDATOR")))
      TRUE
    else
      "not all elements of 'checks' inherit from 'ATOMIC_VALIDATOR'"
  },
  sealed = SEALED
)

#' @rdname validator-classes
#' @name ELEMENT_VALIDATOR
#' @aliases ELEMENT_VALIDATOR-class
#' @docType class
#' @export
#'
setClass("ELEMENT_VALIDATOR",
  contains = c("ATOMIC_VALIDATORS", "PRESENCE_VALIDATOR"),
  sealed = SEALED
)

#' @rdname validator-classes
#' @name COLLECTION_VALIDATOR
#' @aliases COLLECTION_VALIDATOR-class
#' @docType class
#' @export
#'
setClass("COLLECTION_VALIDATOR",
  contains = c("ATOMIC_VALIDATORS", "PRESENCE_VALIDATOR"),
  prototype = list(checks = structure(list(), names = character())),
  validity = function(object) {
    check_names <- function(n) {
      if (is.null(n))
        return("names of 'checks' must not be null")
      errs <- NULL
      if (any(is.na(n)))
        errs <- c(errs, "names of 'checks' must not be NA")
      if (!all(nzchar(n)))
        errs <- c(errs, "names of 'checks' must not be empty")
      if (anyDuplicated.default(n, c("", NA_character_)))
        errs <- c(errs, "names of 'checks' must be unique")
      errs
    }
    errs <- check_names(names(object@checks))
    if (!all(vapply(object@checks, is, NA, "ELEMENT_VALIDATOR") |
        vapply(object@checks, is, NA, "COLLECTION_VALIDATOR")))
      errs <- c(errs, paste0("not all elements of 'checks' inherit from ",
        "'ELEMENT_VALIDATOR' or 'COLLECTION_VALIDATOR'"))
    if (length(errs))
      errs
    else
      TRUE
  },
  sealed = SEALED
)


################################################################################


#' Validation-result classes of the \pkg{validate} package
#'
#' Classes whose members result from a call to \code{\link{validate}}. Each
#' validator class has an according validation class.
#'
#' @examples
#'
#' ## overview on the classes
#' showClass("ATOMIC_VALIDATION")
#' showClass("ATOMIC_VALIDATIONS")
#' showClass("ELEMENT_VALIDATION")
#' showClass("COLLECTION_VALIDATION")
#'
#' ## conversions with as()
#' showMethods("coerce", classes = c("ATOMIC_VALIDATION", "ATOMIC_VALIDATIONS",
#'   "ELEMENT_VALIDATION", "COLLECTION_VALIDATION"))
#'
#' @seealso methods::Methods methods::new
#' @family classes
#' @keywords methods classes
#' @name validation-classes
#' @docType class
#'
NULL

#' @rdname validation-classes
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

#' @rdname validation-classes
#' @name ATOMIC_VALIDATIONS
#' @aliases ATOMIC_VALIDATIONS-class
#' @docType class
#' @export
#'
setClass("ATOMIC_VALIDATIONS",
  contains = "VALIDATORS",
  prototype = list(checks = list(new("ATOMIC_VALIDATION"))),
  validity = function(object) {
    if (all(vapply(object@checks, is, NA, "ATOMIC_VALIDATION")))
      TRUE
    else
      "not all elements of 'checks' inherit from 'ATOMIC_VALIDATION'"
  },
  sealed = SEALED
)

#' @rdname validation-classes
#' @name PRESENCE_VALIDATION
#' @aliases PRESENCE_VALIDATION-class
#' @docType class
#' @export
#'
setClass("PRESENCE_VALIDATION",
  contains = c("PRESENCE_VALIDATOR", "VIRTUAL"),
  slots = c(present = "logical"),
  prototype = list(present = TRUE),
  validity = function(object) {
    if (length(object@present) != 1L)
      "'present' entry must be logical scalar"
    else if (is.na(object@present))
      "'present' entry must not be NA"
    else
      TRUE
  },
  sealed = SEALED
)

#' @rdname validation-classes
#' @name ELEMENT_VALIDATION
#' @aliases ELEMENT_VALIDATION-class
#' @docType class
#' @export
#'
setClass("ELEMENT_VALIDATION",
  contains = c("ATOMIC_VALIDATIONS", "PRESENCE_VALIDATION"),
  validity = function(object) {
    if (length(object@checks) && !object@present)
      "object not present but checks conducted"
    else
      TRUE
  },
  sealed = SEALED
)

#' @rdname validation-classes
#' @name COLLECTION_VALIDATION
#' @aliases COLLECTION_VALIDATION-class
#' @docType class
#' @export
#'
setClass("COLLECTION_VALIDATION",
  contains = c("COLLECTION_VALIDATOR", "PRESENCE_VALIDATION"),
  validity = function(object) {
    if (length(object@checks) && !object@present)
      "object not present but checks conducted"
    else
      TRUE
  },
  sealed = SEALED
)


################################################################################

