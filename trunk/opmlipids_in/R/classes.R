

################################################################################


#' Classes of the \pkg{opmlipids} package
#'
#' Classes whose members can be generated and manipulated by an \pkg{opmlipids}
#' user.
#'
#' @details
#' \acronym{FAME} is an acronym for \sQuote{Fatty Acids with Metadata} (even
#' though \sQuote{Fatty Acid Methyl Ester} would also fit). It holds fatty-acid
#' (or some similar kind of) numeric measurements as well as an additional
#' arbitrary amount of arbitrarily organised metadata.
#'
#' \acronym{FAMES} is the class for holding multiple \acronym{FAME} objects that
#' have the \strong{same} \code{\link{plate_type}}. The name \acronym{FAMES} is
#' just the plural of \acronym{FAME}. As a rule, \acronym{FAMES} has the same
#' methods as the \acronym{FAME} class, but adapted to a collection of more than
#' one \acronym{FAME} object.
#'
#' Objects of these two classes are usually created by inputting files, not with
#' a call to \code{new} or \code{as}, even though this is possible (see below).
#'
#' \acronym{FAME} inherits from \code{WMD} from the \pkg{opm} package and,
#' hence, has all its methods. \acronym{FAMES} inherits from \code{WMDS} and,
#' hence, has all its methods.
#'
#' The coercion of the two classes to a list (and vice versa) is only for expert
#' users and relies on a mapping between slot names and keys in the list, i.e.
#' the list must be appropriately named. For instance, this is the mechanism
#' when reading from and writing to \acronym{YAML}, see \code{to_yaml} from the
#' \pkg{opm} package (both classes inherit from \code{YAML_VIA_LIST} and have
#' its \code{to_yaml} method).
#'
#' The behaviour of \acronym{FAMES} objects with a certain
#' \code{\link{plate_type}} can be specified in detail by the user, see
#' \code{\link{oli_opt}}.
#'
#' @examples
#'
#' ## overview on the classes
#' showClass("FAME")
#' showClass("FAMES")
#'
#' ## conversions with as()
#' showMethods("coerce", classes = "FAME")
#' showMethods("coerce", classes = "FAMES")
#'
#' @docType class
#' @export
#' @aliases FAME-class
#' @seealso methods::Methods methods::new
#' @family classes
#' @keywords methods classes
#'
setClass("FAME",
  slots = c(measurements = "data.frame"),
  contains = c("WMD", "YAML_VIA_LIST"),
  validity = function(object) {
    errs <- fame_problems(object@measurements)
    if (length(errs))
      errs
    else
      TRUE
  },
  sealed = SEALED
)

#' @docType class
#' @rdname FAME
#' @name FAMES
#' @export
#' @aliases FAMES-class
#'
setClass("FAMES",
  contains = c("WMDS", "YAML_VIA_LIST"),
  validity = function(object) {
    if (length(errs <- fame_problems(object@plates)))
      errs
    else
      TRUE
  },
  sealed = SEALED
)

#' @docType class
#' @rdname FAME
#' @name MIDI
#' @export
#' @aliases MIDI-class
#'
setClass("MIDI", contains = "data.frame", sealed = SEALED)


################################################################################
#
# The definitions of initialize() must be located after the class definitions
# to avoid a warning during the Roxygen2 runs.
#


#' Check or initialise \acronym{FAME} or \acronym{FAMES} object
#'
#' Called when constructing an object of one of these classes.
#'
#' @param object Object potentially suitable for one of the slots of these
#'   classes
#' @param .Object \code{\link{FAME}} or \code{\link{FAME}} object.
#' @param ... Additional arguments.
#' @return Character vector with description of problems (empty if there are
#'   none) or \code{\link{FAME}} or \code{\link{FAME}} object.
#' @keywords internal
#' @rdname initialize
#'
setGeneric("fame_problems",
  function(object, ...) standardGeneric("fame_problems"))

#= fame_problems initialize

setMethod("fame_problems", "data.frame", function(object) {
  plate.type <- class_head(object)
  value.col <- get_for_relaxedly(plate.type, "value.col", NA_character_)
  if (is.na(value.col))
    sprintf("no 'value.col' entry for plate type '%s'", plate.type)
  else if (!(pos <- match(value.col, colnames(object), 0L)))
    "value column missing"
  else if (!is.numeric(v <- object[, pos]))
    "value column contains non-numeric data"
  else if (is.na(tol <- get_for_relaxedly(plate.type, "tolerance", NA_real_)))
    sprintf("no 'tolerance' entry for plate type '%s'", plate.type)
  else if (!all(is.na(v)) && !isTRUE(all.equal(100, sum(v, na.rm = TRUE), tol)))
    sprintf("sum of relative frequencies not close enough to 100%")
  else
    NULL
}, sealed = SEALED)

setMethod("fame_problems", "list", function(object) {
  if (!all(vapply(object, is, NA, "FAME")))
    return("not all elements inherit from the 'FAME' class")
  x <- duplicated.default(vapply(object, plate_type, ""))
  if (!all(x[-1L]))
    return("non-uniform plate types")
  NULL
}, sealed = SEALED)

setMethod("initialize", "FAMES", function(.Object, ...) {
  .Object <- callNextMethod()
  names(.Object@plates) <- NULL
  .Object
}, sealed = SEALED)


################################################################################
#
# Conversion to and from FAME/FAMES objects
#


setOldClass("midi_entry")

setOldClass("midi_entries")

setAs("midi_entry", "FAME", function(from) {
  pos <- match("Measurements", names(from), 0L)
  if (!pos) # just to provide a more meaningful error message
    stop("object of class 'midi_entry' lacks 'Measurements' element")
  if (is.null(olif <- attr(from, ".file")))
    stop("object of class 'midi_entry' lacks '.file' attribute")
  olif <- structure(list(olif), names = get_for("MIDI", "file.entry"))
  new("FAME", measurements = as(from[[pos]], "MIDI"),
    metadata = c(from[-pos], olif))
})

setAs("midi_entries", "FAMES", function(from) {
  new("FAMES", plates = lapply(from, as, "FAME"))
})


################################################################################


setAs("FAME", "list", function(from) {
  to_list <- function(x, pt) {
    c(as.list(x),
      structure(list(rownames(x)), names = get_for(pt, "row.names")))
  }
  list(plate_type = tail(class(from@measurements), 1L),
    measurements = to_list(from@measurements, plate_type(from)),
    metadata = from@metadata)
})

setAs("list", "FAME", function(from) {
  from_list <- function(x, pt) {
    x <- as.data.frame(x, stringsAsFactors = FALSE, optional = TRUE)
    rn <- get_for(sub("[\\W_].*", "", pt, FALSE, TRUE), "row.names")
    rownames(x) <- x[, rn]
    x[, rn] <- NULL
    as(x, pt)
  }
  new("FAME", measurements = from_list(from[["measurements"]],
    from[["plate_type"]]), metadata = from[["metadata"]])
})

setAs("FAMES", "list", function(from) {
  lapply(from@plates, as, "list")
})

setAs("list", "FAMES", function(from) {
  new("FAMES", plates = lapply(from, as, "FAME"))
})


################################################################################


setAs("FAME", "numeric", function(from) {
  x <- from@measurements[, get_for(pt <- plate_type(from), "value.col")]
  names(x) <- rownames(from@measurements)
  x <- x[!is.na(x)]
  if (get_for(pt, "sum.dup")) {
    n <- vapply(strsplit(names(x), APPENDIX, TRUE), `[[`, "", 1L)
    if (anyDuplicated(n)) {
      warning("summing up duplicates")
      x <- vapply(split.default(x, n), sum, 0)
    }
  }
  x
})

setAs("FAMES", "matrix", function(from) {
  x <- collect(lapply(from@plates, as, "numeric"), "values")
  x[is.na(x)] <- get_for(plate_type(from), "na.yields")
  x
})


################################################################################

