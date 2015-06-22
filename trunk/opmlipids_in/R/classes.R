

################################################################################


#' Classes of the \pkg{opmlipids} package
#'
#' Classes whose members can be generated and manipulated by an \pkg{opmlipids}
#' user, as well as auxiliary classes a user not normally manipulates directly.
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
#' The other classes referring to this page are auxiliary (they are used for
#' generating or displaying \acronym{FAME} or \acronym{FAMES} objects) and not
#' normally directly modified by a user.
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
      if (!exists(ptype, PLATE_TYPE_SETTINGS, , , "list", FALSE))
        return(sprintf("no stored settings for plate type '%s'", ptype))
      opt <- get(ptype, PLATE_TYPE_SETTINGS, , "list", FALSE)
      if (is.null(vcol <- opt$value.col))
        return(sprintf("no 'value.col' entry for plate type '%s'", ptype))
      if (!(pos <- match(vcol, colnames(x), 0L)))
        return(sprintf("no column named '%s'", vcol))
      if (!is.numeric(v <- x[, pos]))
        return(sprintf("column '%s' contains non-numeric data: %s", vcol,
          paste0(x[, pos], collapse = "/")))
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
setClass("MIDI", contains = "data.frame", validity = function(object) {
    errs <- assert_elements(object, c(RT = "numeric", Response = "numeric",
      `Ar/Ht` = "numeric", RFact = "numeric", ECL = "numeric",
      Percent = "numeric", `Peak Name` = "character",
      Comment1 = "character", Comment2 = "character"))
    if (length(errs))
      errs
    else
      TRUE
  },
  sealed = SEALED
)

#' @docType class
#' @rdname FAME
#' @name FAMES_Summary
#' @export
#' @aliases FAMES_Summary-class
#'
setClass("FAMES_Summary", contains = "data.frame", slots = c(Of = "character"),
  sealed = SEALED)


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
# Conversion to and from data frames
#


# Where 'from' must already contain the relevant columns.
setAs("data.frame", "MIDI", function(from) {
  for (name in c("RT", "Response", "Ar/Ht", "RFact", "ECL", "Percent"))
    if (!is.double(from[, name]))
      from[, name] <- as.double(chartr(",", ".", from[, name]))
  for (name in c("Peak Name", "Comment1", "Comment2"))
    from[, name] <- trim_whitespace(from[, name])
  pos <- grepl("^Summed\\s+Feature\\s+\\d+$", from[, "Peak Name"],
    TRUE, TRUE) | is.na(from[, "Percent"]) | from[, "Percent"] == 0
  value.col <- get_for("MIDI", "value.col")
  from[, value.col] <- ifelse(pos, NA_real_, from[, "Percent"])
  if (any(pos <- !is.na(from[, value.col]))) {
    rn <- ifelse(grepl("^Sum\\s*In\\s*Feature\\s*\\d+$",
      from[, "Peak Name"], TRUE, TRUE), from[, "Comment2"], from[, "Peak Name"])
    rownames(from)[pos] <- make.unique(norm_fa(rn[pos], TRUE), APPENDIX)
  }
  new("MIDI", from)
})


# This can be used to read from MIDI-generated Excel files. It converts a single
# worksheet.
setAs("data.frame", "FAME", function(from) {
  is_empty <- function(x) is.na(x) | !nzchar(x)
  drop_empty_columns <- function(x) {
    if (any(empty <- vapply(lapply(x, is_empty), all, NA)))
      x <- x[, !empty, drop = FALSE]
    x
  }
  convert_part <- function(x) {
    if (nrow(x) < 2L)
      stop("less than two rows")
    if (!all(nzchar(cn <- x[1L, ])))
      stop("empty column names")
    x <- x[-1L, , drop = FALSE]
    colnames(x) <- cn
    rownames(x) <- NULL
    x
  }
  for (i in which(vapply(from, is.factor, NA)))
    from[, i] <- as.character(from[, i])
  if (!all(vapply(from, is.character, NA)))
    stop("unsupported kind of data frame")
  f <- Reduce(`&`, lapply(from, is_empty))
  from <- split.data.frame(from, sections(f, FALSE))
  if (length(from) < 5L)
    from <- c(from[1L], split.data.frame(from[[2L]],
      sections(is_empty(from[[2L]][, 1L]), FALSE)), from[c(3L, 4L)])
  from <- lapply(from, drop_empty_columns)
  measurements <- as(convert_part(from[[3L]]), "MIDI")
  # entries absolutely needed are: Method, Sample ID, ID Number, Type
  metadata <- list()
  # irregular parts, must address fields individually
  metadata[c("Volume", "File")] <- from[[1L]][1L, ]
  metadata[c("ID Number", "Bottle")] <- from[[2L]][, 2L]
  metadata$`Sample ID` <- from[[2L]][1L, 3L]
  metadata[c("Type", "Method", "Created")] <- from[[2L]][2L, c(4L, 5L, 6L)]
  # more regularly structured
  metadata[from[[4L]][, 1L]] <- from[[4L]][, 2L]
  metadata[from[[4L]][, 3L]] <- from[[4L]][, 4L]
  # expect variable number of matches, but at least 'NO MATCH'
  metadata$Matches <- convert_part(from[[5L]])
  if (anyNA(metadata))
    stop("metadata selection yielded NA values")
  new("FAME", measurements = measurements, metadata = metadata)
})


################################################################################
#
# Conversion to and from lists (mainly for YAML I/O)
#


setAs("FAME", "list", function(from) {
  to_list <- function(x, ptype) c(as.list(x),
    structure(list(rownames(x)), names = get_for(ptype, "row.names")))
  klass <- class(from@measurements) # must be in sync with plate_type()
  list(plate_subtype = klass, measurements = to_list(from@measurements,
    str_head(klass)), metadata = from@metadata)
})


setAs("list", "FAME", function(from) {
  from_list <- function(x, klass) {
    if (!length(x))
      stop("missing or empty 'measurements' entry")
    if (!length(klass))
      stop("missing or empty 'plate_subtype' entry")
    x <- as.data.frame(x, stringsAsFactors = FALSE, optional = TRUE)
    # mechanism must be in sync with plate_type()
    rn <- get_for(str_head(klass), "row.names")
    rownames(x) <- x[, rn]
    x[, rn] <- NULL
    as(x, klass)
  }
  new("FAME", metadata = from[["metadata"]],
    measurements = from_list(from[["measurements"]], from[["plate_subtype"]]))
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

