

################################################################################


#' Classes of the \pkg{opmlipids} package
#'
#' Classes whose members can directly be generated and manipulated by an
#' \pkg{opmlipids} user.
#'
#' @details
#' \acronym{FAME} is an acronym for \sQuote{Fatty Acids with Metadata} (even
#' though \sQuote{Fatty Acid Methyl Ester} would also fit). It holds fatty-acid
#' (or some similar kind of) numeric measurements as well as an additional
#' arbitrary amount of arbitrarily organised metadata.
#'
#' \acronym{FAMES} is the class for holding multiple \acronym{FAME} objects. The
#' name \acronym{FAMES} is just the plural of \acronym{FAME}. As a rule,
#' \acronym{FAMES} has the same methods as the \acronym{FAME} class, but adapted
#' to a collection of more than one \acronym{FAME} object.
#'
#' Objects of these two classes are usually created by inputting files, not with
#' a call to \code{new} or \code{as}, even though this is possible (see below).
#'
#' \acronym{FAME} inherits from \code{WMD} from the \pkg{opm} package and,
#' hence, has all its methods. \acronym{FAMES} inherits from \code{WMDS} and,
#' hence, has all its methods.
#'
#' The coercion of the  two classes to a list (and vice versa) is only for
#' expert users and relies on a mapping between slot names and keys in the list,
#' i.e. the list must be appropriately named. For instance, this is the
#' mechanism when reading from and writing to \acronym{YAML}, see \code{to_yaml}
#' from the \pkg{opm} package (both classes inherit from \code{YAML_VIA_LIST}
#' and have its \code{to_yaml} method).
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
setClass(FAME,
  slots = c(plate_type = "character", measurements = "data.frame"),
  contains = c("WMD", "YAML_VIA_LIST"),
  validity = function(object) {
    errs <- c(fame_problems(object@measurements),
      fame_problems(object@plate_type))
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
setClass(FAMES,
  contains = c("WMDS", "YAML_VIA_LIST"),
  validity = function(object) {
    if (length(errs <- fame_problems(object@plates)))
      errs
    else
      TRUE
  },
  sealed = SEALED
)


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
setGeneric("fame_problems", function(object) standardGeneric("fame_problems"))

#= fame_problems initialize

setMethod("fame_problems", "data.frame", function(object) {
  errs <- NULL
  pos <- match(VALUE_COL, colnames(object), 0L)
  if (!pos)
    errs <- c(errs, "value column missing")
  else if (!is.numeric(object[, pos]))
    errs <- c(errs, "value column contains non-numeric data")
  errs
}, sealed = SEALED)

setMethod("fame_problems", "character", function(object) {
  if (length(object) == 1L)
    NULL
  else
    sprintf("object has wrong length (%i)", length(object))
}, sealed = SEALED)

setMethod("fame_problems", "list", function(object) {
  if (length(object) < 2L)
    return("less than two plates contained")
  if (!all(vapply(object, is, NA, FAME)))
    return("not all elements inherit from the 'FAME' class")
  x <- duplicated.default(vapply(object, slot, "", "plate_type"))
  if (!all(x[-1L]))
    return("non-uniform plate types")
  NULL
}, sealed = SEALED)

setMethod("initialize", FAME, function(.Object, ...) {
  .Object <- callNextMethod()
  .Object@plate_type <- make.names(.Object@plate_type)
  .Object
}, sealed = SEALED)

setMethod("initialize", FAMES, function(.Object, ...) {
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

setAs("midi_entry", FAME, function(from) {
  pos <- match("Measurements", names(from), 0L)
  if (!pos) # just to provide a more meaningful error message
    stop("object of class 'midi_entry' lacks 'Measurements' element")
  new(FAME, measurements = from[[pos]], plate_type = "MIDI",
    metadata = unclass(from[-pos]))
})

setAs("midi_entries", FAMES, function(from) {
  new(FAMES, plates = lapply(from, as, FAME))
})


setAs(FAME, "list", function(from) {
  to_list <- function(x) {
    x[, ROWNAMES] <- rownames(x)
    as.list(x)
  }
  sn <- slotNames(from)
  result <- structure(vector("list", length(sn)), names = sn)
  for (i in seq_along(sn))
    result[[i]] <- slot(from, sn[[i]])
  result[["measurements"]] <- to_list(result[["measurements"]])
  result
})

setAs("list", FAME, function(from) {
  from_list <- function(x) {
    x <- as.data.frame(x, stringsAsFactors = FALSE, optional = TRUE)
    rownames(x) <- x[, ROWNAMES]
    x[, ROWNAMES] <- NULL
    x
  }
  from[["measurements"]] <- from_list(from[["measurements"]])
  do.call(new, c(list(Class = FAME), from))
})

setAs(FAMES, "list", function(from) {
  lapply(from@plates, as, "list")
})

setAs("list", FAMES, function(from) {
  new(FAMES, plates = lapply(from, as, "FAME"))
})


setAs(FAME, "numeric", function(from) {
  x <- from@measurements[, VALUE_COL]
  names(x) <- rownames(from@measurements)
  x[!is.na(x)]
})

setAs(FAME, "matrix", function(from) {
  x <- from@measurements[, VALUE_COL, drop = FALSE]
  t(as.matrix(x[!is.na(x[, VALUE_COL]), , drop = FALSE]))
})

setAs(FAMES, "matrix", function(from) {
  sortable <- function(x) sprintf(sprintf("V%%0%ii", nchar(x)), seq_len(x))
  x <- mapply(`rownames<-`, lapply(from@plates, as, "matrix"),
    sortable(length(from)), SIMPLIFY = FALSE, USE.NAMES = FALSE)
  x <- collect(x, "datasets")
  x[is.na(x)] <- 0
  x
})


################################################################################

