


#' Get or enter plate-type settings
#'
#' Set or retrieve settings for a given plate type.
#'
#' @param x Character vector, named list, or missing. If a character vector,
#'   the names of the plates types for which settings should be stored (if
#'   \code{...} is non-empty) or retrieved (if \code{...} is empty). If a list,
#'   then the list must be named with plate types and contain named lists which
#'   are supposed to contain the settings for each plate type.
#' @param ... Optional arguments used for determining the settings for each
#'   plate type contained in \code{x}. Missing settings are replaced by default
#'   ones. If \code{...} is totally missing, nothing is set, but the already
#'   stored settings for the plate types given in \code{x} are returned.
#' @return This returns a named nested lists, with the names corresponding to
#'   plate-type names after normalisation.
#' @details The \pkg{opmlipids} package uses various settings for customising
#'   the behaviour of \code{\link{FAMES}} objects depending on their plate type.
#'   \describe{
#'   \item{char.group}{The name used in textual output for the kind of
#'   characters stored in this plate type. Used by \code{\link{listing}}.}
#'   \item{file.entry}{How to name input-file entries within the metadata.}
#'   \item{na.yields}{When converting \code{\link{FAMES}} objects to matrices
#'   some values might be missing from some of the contained \code{\link{FAME}}
#'   objects, which are replaced by this default value.}
#'   \item{row.names}{How to name the \code{rownames} entry of the
#'   \code{\link{measurements}} slot when converting to a list.}
#'   \item{sum.up}{Logical scalar indicating whether or not duplicate row names
#'   should be summed up. (The alternative is to make them unique, but this is
#'   not guaranteed to work across distinct plates.)}
#'   \item{tolerance}{How much deviation from a sum of 100 percent to accept in
#'   percentage input.}
#'   \item{value.col}{The name of the column in the \code{\link{measurements}}
#'   data frame that stores the numeric measurements (all other columns, if any,
#'   should contain additional information on the respective measurements.)}
#'   }
#' @family auxiliary-functions
#' @keywords utilities
#' @seealso \link{plate_type}
#' @seealso base::options base::getOption opm::opm_opt
#' @export
#' @examples
#' oli_opt("MIDI") # this one is already defined
#'
setGeneric("oli_opt", function(x, ...) standardGeneric("oli_opt"))

setMethod("oli_opt", "character", function(x, ...) {
  x <- make.names(x)
  if (missing(...))
    return(mget(x, PLATE_TYPE_SETTINGS))
  oli_opt(structure(rep.int(list(list(...)), length(x)), names = x))
}, sealed = SEALED)

setMethod("oli_opt", "list", function(x) {
  add_defaults <- function(x) {
    lacking <- setdiff(names(DEFAULT_PLATE_TYPE_SETTINGS), names(x))
    x[lacking] <- DEFAULT_PLATE_TYPE_SETTINGS[lacking]
    x
  }
  if (is.null(names(x)) || !all(vapply(x, is.list, NA)))
    stop("'x' must be a named list of lists")
  x <- structure(lapply(x, add_defaults), names = make.names(names(x)))
  list2env(x, PLATE_TYPE_SETTINGS)
  invisible(x)
}, sealed = SEALED)

setMethod("oli_opt", "missing", function(x) {
  as.list(PLATE_TYPE_SETTINGS)
}, sealed = SEALED)


################################################################################
################################################################################
#
# Internal helper functions of the package
#


#' Get stored plate-type settings
#'
#' Get the settings that have been stored for a plate type, or determine that
#' plate type using a simple derivation from the class name.
#'
#' @param plate.type Character scalar.
#' @param what Character scalar. The key whose value is of interest.
#' @param alternative Alternative value to return.
#' @param x Any \R object.
#' @return The value associated with this key and plate type, or just this
#'   plate type.
#' @keywords internal
#'
get_for <- function(plate.type, what) {
  get(what, get(plate.type, PLATE_TYPE_SETTINGS))
}

#' @rdname get_for
#'
get_for_relaxedly <- function(plate.type, what, alternative) {
  tryCatch(get_for(plate.type, what), error = function(e) alternative)
}

#' @rdname get_for
#'
class_head <- function(x) {
  sub("[\\W_].*", "", class(x)[1L], FALSE, TRUE)
}


################################################################################


#' Helper methods for indexing etc.
#'
#' Close gaps in indexes by removing all \code{NULL} elements, with a warning.
#' Alternatively, check whether elements of a list can be combined into a
#' \code{\link{FAMES}} object, or combine them.
#'
#' @param x Vector-like \R object.
#' @return \code{x}, its size reduced accordingly, or logical scalar.
#' @keywords internal
#'
close_index_gaps <- function(x) {
  if (any(bad <- vapply(x, is.null, NA))) {
    warning("closing gaps in indexes")
    return(x[!bad])
  }
  x
}

#' @rdname close_index_gaps
#'
joinable <- function(x) {
  if (!all(vapply(x, is, NA, "FAME") | vapply(x, is, NA, "FAMES")))
    return(FALSE)
  pt <- vapply(x, plate_type, "")
  all(duplicated.default(pt[!is.na(pt)])[-1L])
}

#' @rdname close_index_gaps
#'
join_if_possible <- function(x) {
  if (joinable(x))
    new("FAMES", plates = unlist(lapply(x, plates), FALSE, FALSE))
  else
    x
}


################################################################################


#' Helper methods for re-interpreting fatty acids
#'
#' Find unnamed fatty acids and accordingly re-calculate all percentages, or
#' find the closests occurrence of the elements of a vector in another one.
#'
#' @param x Data frame of measurements as contained in a \code{\link{FAME}}
#'   object. For \code{nearest}, a numeric vector.
#' @param table Numeric vector.
#' @return \code{x}, its columns accordingly modified. \code{nearest} returns
#'   the indexes in \code{table} of the value closest to the respective element
#'   of \code{x}.
#' @keywords internal
#'
recalculate_percents <- function(x) {
  interpolate_rfact <- function(x) {
    if (any(x$Interpol <- is.na(x$RFact) & !is.na(x$RT))) {
      m <- lm(log(x$RFact) ~ log(x$RT))
      x$RFact <- ifelse(x$Interpol, exp(predict(m, x)), x$RFact)
    }
    x
  }
  x <- interpolate_rfact(x)
  skip <- grepl("^\\s*(<\\s+min|>\\s+max)\\s+rt\\s*$", x$Comment1, TRUE,
    TRUE) | grepl("^\\s*Summed\\s+Feature\\s+\\d+\\s*$", x[, "Peak Name"],
    TRUE, TRUE)
  val <- ifelse(skip, NA_real_, x$Response * x$RFact)
  val <- 100 * val / sum(val, na.rm = TRUE)
  x$VALUE2 <- val
  is.nn <- is.na(x$VALUE) & !is.na(x$VALUE2)
  nn <- ifelse(nzchar(x$`Peak Name`), x$`Peak Name`,
    sprintf("unknown %0.2f", x$ECL))
  nn[is.nn] <- norm_fa(nn[is.nn], TRUE)
  rownames(x) <- make.unique(ifelse(is.nn, nn, rownames(x)), APPENDIX)
  x
}

#' @rdname recalculate_percents
#'
nearest <- function(x, table) {
  # from http://stackoverflow.com/questions/10160400/r-find-nearest-index
  idx <- findInterval(x, table, FALSE, TRUE)
  idx + (2 * x > table[idx] + table[idx + 1L])
}


################################################################################


#' Parse \acronym{RTF}
#'
#' A simple \acronym{RTF} parser. It removes delimiters from the control words
#' and outcomments percent characters. Afterwards the text is parsed with
#' \code{parseLatex} from the \pkg{tools} package, and an \sQuote{RTF} object is
#' returned (which has \sQuote{LaTeX} as parent class).
#'
#' @param x Character vector.
#' @return Object of class \sQuote{RTF}.
#' @keywords internal
#' @note The way the input is currently parsed implies that \acronym{RTF} files
#'   cannot be regenerated from such objects, and the function is only good for
#'   reading from \acronym{RTF} content, but neither for reading from
#'   \acronym{RTF} format entries nor for manipulating and generating novel
#'   \acronym{RTF}.
#' @seealso tools::parseLatex
#'
parse_rtf <- function(x) UseMethod("parse_rtf")

#' @rdname parse_rtf
#' @method parse_rtf character
#'
parse_rtf.character <- function(x) {
  if (is.null(fromfile <- attr(x, ".file")))
    warning("attribute '.file' is missing")
  x <- gsub("(\\\\[a-z]+)-?\\d+", "\\1", x, FALSE, TRUE)
  x <- parseLatex(gsub("%", "\\%", x, FALSE, FALSE, TRUE))
  class(x) <- c("RTF", oldClass(x))
  attr(x, ".file") <- fromfile
  x
}


################################################################################


#' Internal conversion functions for \acronym{MIDI} data
#'
#' Split an object of class \sQuote{flat_RTF} into sections, or create such an
#' object from an object of class \code{RTF}. Alternatively, create a subset of
#' a \sQuote{midi_entries} object or merge several such objects (or objects that
#' are convertible to such objects) into a single one.
#'
#' @param object Object of class \sQuote{RTF}.
#' @param x Object of class \sQuote{flat_RTF}, \sQuote{RTF} or
#'   \sQuote{midi_entries}. For the \code{merge} method, optionally a
#'   \sQuote{midi_entry} object.
#' @param y Object convertible via \code{\link{fatty_acids}}.
#' @param breaks Character vector. A backslash is prepended if necessary.
#' @param delete Optional character vector determining which
#'   \sQuote{midi_entry} objects to remove from a \sQuote{midi_entries}
#'   object.
#' @param ... Optional arguments. For the \code{merge} methods, other objects
#'   like \code{y}.
#' @return Object of class \sQuote{flat_RTF} or list of such objects, or object
#'   of class \sQuote{midi_entries}.
#' @details The \sQuote{midi_entries} currently supports the following
#'   \code{delete} entries (several ones can be given): \describe{
#'   \item{calib}{Remove those \sQuote{midi_entry} object with \sQuote{Type}
#'   set to \sQuote{Calib}. These are calibration runs that contain no sample
#'   data.} }
#'
#'   For the \sQuote{midi_entries} method of \code{merge} it is an error if
#'   a \sQuote{Method} entry is lacking, and a warning is issued if these
#'   entries are not uniform.
#' @name cut
#' @keywords internal
#'
NULL

#' @rdname cut
#' @method cut flat_RTF
#' @export
#'
cut.flat_RTF <- function(x, breaks, ...) {
  if (!length(breaks))
    stop("'breaks' must not be empty")
  breaks <- sections(x %in% to_macro(breaks) & macro(x), include = FALSE)
  mapply(structure, .Data = split(x, breaks), macro = split(macro(x), breaks),
    MoreArgs = list(class = oldClass(x), .file = attr(x, ".file")),
    SIMPLIFY = FALSE, USE.NAMES = FALSE)
}

#' @rdname cut
#' @method flatten RTF
#' @export
#'
flatten.RTF <- function(object, ...) {
  file <- attr(object, ".file")
  tags <- rapply(object, attr, which = "latex_tag")
  object <- unlist(object)
  if (length(tags) != length(object))
    stop("malformatted RTF object: not as many tags as elements")
  if (length(bad <- setdiff(tags, c("MACRO", "TEXT"))))
    stop("malformatted RTF object: unknown tag ", bad[1L])
  structure(object, macro = tags == "MACRO", class = "flat_RTF", .file = file)
}

#' @rdname cut
#' @method subset midi_entries
#' @export
#'
subset.midi_entries <- function(x, delete = "calib", ...) {
  delete <- match.arg(delete, several.ok = TRUE)
  if ("calib" %in% delete)
    x[vapply(x, `[[`, "", i = "Type") == "Calib"] <- NULL
  x
}

#' @rdname cut
#' @method subset RTF
#' @export
#'
subset.RTF <- function(x, delete, ...) {
  # Newer version, but since flatten() has been improved this is currently the
  # limiting step. Perhaps this can be omitted and replaced by deleting stuff
  # after splitting into paragraphs? But the usual blocks to delete do not
  # necessarily occur at a given level.
  must_delete <- function(x) is.character(x[[1L]]) && any(x[[1L]] %in% delete)
  delete_recursively <- function(x) {
    bad <- is.block <- vapply(x, is.list, NA)
    bad[bad] <- vapply(x[bad], must_delete, NA)
    x[is.block] <- lapply(x[is.block <- is.block & !bad], delete_recursively)
    x[bad] <- NULL
    x
  }
  if (!missing(delete) && length(delete <- to_macro(delete)))
    x <- delete_recursively(x)
  x
}

#' @rdname cut
#' @method merge midi_entry
#' @export
#'
merge.midi_entry <- function(x, y, ...) {
  # convert x to 'midi_entries' object, then move on
  merge(x = fatty_acids(x), y = y, ...)
}

#' @rdname cut
#' @method merge midi_entries
#' @export
#'
merge.midi_entries <- function(x, y, ...) {
  y <- unlist(lapply(list(y, ...), FUN = fatty_acids), recursive = FALSE)
  methods <- vapply(x, `[[`, "", i = "Method")
  if (!all(duplicated.default(methods)[-1L]))
    warning("non-uniform 'Method' entries")
  structure(c(x, y), class = oldClass(x))
}


################################################################################


#' Create or extract LaTex macro strings
#'
#' Prepend a backslash if necessary, or just extract the \sQuote{macro}
#' attribute.
#'
#' @param x Character vector. (Any \R object for \code{macro}.)
#' @return Character vector, or \code{NULL} or content of \sQuote{macro}
#'   attribute.
#' @keywords internal
#'
to_macro <- function(x) UseMethod("to_macro")

#' @rdname to_macro
#' @method to_macro character
#'
to_macro.character <- function(x) {
  bad <- !grepl("^\\\\", x, FALSE, TRUE)
  x[bad] <- sprintf("\\%s", x[bad])
  x
}

#' @rdname to_macro
#'
macro <- function(x) {
  attr(x, "macro")
}


################################################################################


#' Interpret fatty-acid sample \acronym{ID}
#'
#' Interpret sample \acronym{ID} as used at \acronym{DSMZ}. This is hard to
#' generalise and thus internal.
#'
#' @param x Character vector.
#' @return Four-column data frame with character vectors as elements.
#' @keywords internal
#'
interpret_dsmz_sample_id <- function(x) {
  cultivation_components <- function(x) {
    interpret <- function(x) {
      duration <- grepl("^\\d+d$", x, TRUE, TRUE)
      x <- ifelse(duration, tolower(x), toupper(x))
      names(x) <- rep.int("Medium", length(x))
      names(x)[x %in% c("NDK", "VDK")] <- "Timepoint"
      names(x)[duration] <- "Duration"
      x
    }
    x <- sub("^\\s+", "", sub("\\s+$", "", x, FALSE, TRUE), FALSE, TRUE)
    x <- lapply(strsplit(x, "\\s*,\\s*", FALSE, TRUE), interpret)
    collect(x, "values", dataframe = TRUE, stringsAsFactors = FALSE)
  }
  x <- sub("^\\s+", "", x, FALSE, TRUE)
  m <- regexpr("\\([^()]+\\)", x, FALSE, TRUE)
  result <- data.frame(Organism = ifelse(m > 0L, substr(x, 1L, m - 1L), x),
    stringsAsFactors = FALSE)
  result$Organism <- sub("\\s+$", "", result$Organism, FALSE, TRUE)
  x <- substr(x, m + 1L, m + attr(m, "match.length") - 2L)
  cbind(result, cultivation_components(x))
}


################################################################################
