

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
    fame_problems <- function(x) {
      ptype <- str_head(class(x))
      if (is.null(opt <- get0(ptype, PLATE_TYPE_SETTINGS)))
        return(sprintf("no stored settings for plate type '%s'", ptype))
      if (is.null(vcol <- opt$value.col))
        return(sprintf("no 'value.col' entry for plate type '%s'", ptype))
      if (!(pos <- match(vcol, colnames(x), 0L)))
        return(sprintf("no column named '%s'", vcol))
      if (!is.numeric(v <- x[, pos]))
        return(sprintf("column '%s' contains non-numeric data", vcol))
      if (is.null(tol <- opt$tolerance))
        return(sprintf("no 'tolerance' entry for plate type '%s'", ptype))
      if (!all(is.na(v)) && !isTRUE(all.equal(100, sum(v, na.rm = TRUE), tol)))
        return("sum of relative frequencies not close enough to 100%")
      NULL
    }
    if (length(errs <- fame_problems(object@measurements)))
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
    fames_problems <- function(x) {
      if (!all(vapply(x, is, NA, "FAME")))
        return("not all elements inherit from the 'FAME' class")
      x <- duplicated.default(vapply(x, plate_type, ""))
      if (!all(x[-1L]))
        return("non-uniform plate types")
      NULL
    }
    if (length(errs <- fames_problems(object@plates)))
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


#' Initialise \acronym{FAME} or \acronym{FAMES} object
#'
#' Called when constructing an object of one of these classes.
#'
#' @param .Object \code{\link{FAME}} or \code{\link{FAME}} object.
#' @param ... Additional arguments.
#' @return \code{\link{FAME}} or \code{\link{FAME}} object.
#' @keywords internal
#' @rdname initialize
#'
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


setOldClass("midi_entries")


setAs("midi_entries", "FAMES", function(from) {
  new("FAMES", plates = lapply(from, as, "FAME"))
})


################################################################################
#
# Conversion to and from lists (mainly for YAML I/O)
#


setAs("FAME", "list", function(from) {
  to_list <- function(x, ptype) c(as.list(x),
      structure(list(rownames(x)), names = get_for(ptype, "row.names")))
  list(plate_type = class(from@measurements)[[1L]],
    measurements = to_list(from@measurements, plate_type(from)),
    metadata = from@metadata)
})


setAs("list", "FAME", function(from) {
  from_list <- function(x, ptype) {
    x <- as.data.frame(x, stringsAsFactors = FALSE, optional = TRUE)
    rn <- get_for(str_head(ptype), "row.names")
    rownames(x) <- x[, rn]
    x[, rn] <- NULL
    as(x, ptype)
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
#
# Conversion to numeric vectors/matrices
#


setAs("FAME", "numeric", function(from) {
  x <- from@measurements[, get_for(ptype <- plate_type(from), "value.col")]
  names(x) <- rownames(from@measurements)
  x <- x[!is.na(x)]
  if (get_for(ptype, "sum.dup")) {
    n <- vapply(strsplit(names(x), APPENDIX, TRUE), `[[`, "", 1L)
    if (anyDuplicated.default(n)) {
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

