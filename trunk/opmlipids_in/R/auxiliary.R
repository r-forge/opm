

################################################################################
################################################################################
#
# Internal helper functions of the package
#


#' Convert to \acronym{FAME} or \acronym{FAMES}
#'
#' Convert either \code{\link{FAME}} or \code{\link{FAMES}} objects (\code{NULL}
#' if necessary).
#'
#' @param x Any \R object.
#' @return \code{\link{FAME}} or \code{\link{FAMES}} object or \code{NULL},
#'   depending on the input size.
#' @keywords internal
#'
fames <- function(x) UseMethod("fames")

#' @rdname fames
#' @method fames midi_entries
#' @export
#'
fames.midi_entries <- function(x) {
  case(length(x), NULL, as(x[[1]], FAME), as(x, FAMES))
}

#' @rdname fames
#' @method fames midi_entry
#' @export
#'
fames.midi_entry <- function(x) {
  as(x, FAME)
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
  x <- gsub("(\\\\[a-z]+)-?\\d+", "\\1", x, FALSE, TRUE)
  x <- parseLatex(gsub("%", "\\%", x, fixed = TRUE))
  class(x) <- c("RTF", oldClass(x))
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
    MoreArgs = list(class = oldClass(x)), SIMPLIFY = FALSE, USE.NAMES = FALSE)
}

#' @rdname cut
#' @method flatten RTF
#' @export
#'
flatten.RTF <- function(object, ...) {
  tags <- rapply(object, attr, which = "latex_tag")
  object <- unlist(object)
  if (length(tags) != length(object))
    stop("malformatted RTF object: not as many tags as elements")
  if (length(bad <- setdiff(tags, c("MACRO", "TEXT"))))
    stop("malformatted RTF object: unknown tag ", bad[1L])
  structure(object, macro = tags == "MACRO", class = "flat_RTF")
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
