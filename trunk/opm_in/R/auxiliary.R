################################################################################
################################################################################
#
# Miscellaneous helper functions
#


################################################################################


#' Regularly split character vectors if possible
#'
#' From a given set of splitting characters select the ones that split a
#' character vector in a regular way, yielding the same number of parts for all
#' vector elements. Then apply these splitting characters to create a matrix.
#' The data frame method applies this to all character vectors (and optionally
#' also all factors) within a data frame.
#'
#' @param object Character vector to be split, or data frame in which character
#'   vectors (or factors) shall be attempted to be split, or factor.
#' @param split Character vector or \code{TRUE}. \itemize{
#'   \item If a character vector, used as container of the splitting characters
#'   and converted to a vector containing only non-duplicated single-character
#'   strings. For instance, the default \code{split} argument \code{".-_"}
#'   yields \code{c(".", "-", "_")}.
#'   \item If a vector of only empty strings or \code{TRUE}, strings with parts
#'   representing fixed-width fields are assumed, and splitting is done at
#'   whitespace-only columns. Beforehand, equal-length strings are created by
#'   padding with spaces at the right. After splitting in fixed-width mode,
#'   whitespace characters are trimmed from both ends of the resulting strings.
#'   }
#' @param simplify Logical scalar indicating whether a resulting matrix with one
#'   column should be simplified to a vector (or such a data frame to a factor).
#'   If so, at least one matrix column is kept, even if \code{keep.const} is
#'   \code{FALSE}.
#' @param keep.const Logical scalar indicating whether constant columns should
#'   be kept or removed.
#' @param coerce Logical scalar indicating whether factors should be coerced to
#'   \sQuote{character} mode and then also be attempted to be split. The
#'   resulting columns will be coerced back to factors.
#' @param name.sep Character scalar to be inserted in the constructed column
#'   names. If more than one column results from splitting, the names will
#'   contain (i) the original column name, (ii) \code{name.sep} and (iii) their
#'   index, thus creating unique column names (if the original ones were
#'   unique).
#' @param list.wise Logical scalar. Ignored if \code{split} is \code{TRUE}.
#'   Otherwise, \code{object} is assumed to contain word lists separated by
#'   \code{split}. The result is a logical matrix in which the columns represent
#'   these words and the fields indicate whether or not a word was present in a
#'   certain item contained in \code{object}.
#' @param strip.white Logical scalar. Remove whitespace from the ends of each
#'   resulting character scalar after splitting? Has an effect on the removal of
#'   constant columns. Whitespace is always removed if \code{split} is
#'   \code{TRUE}.
#' @param ... Optional arguments passed between the methods.
#' @export
#' @details This function is useful if information coded in the elements of a
#'   character vector is to be converted to a matrix or data frame. For
#'   instance, file names created by a batch export conducted by a some software
#'   are usually more or less regularly structured and contain content at
#'   distinct positions. In such situations, the correct splitting approach can
#'   be recognised by yielding the same number of fields from each vector
#'   element.
#' @return Character matrix, its number of rows being equal to the length of
#'   \code{object}, or data frame with the same number of rows as \code{object}
#'   but potentially more columns. May be character vector of factor with
#'   character or factor input and \code{simplify} set to \code{TRUE}.
#' @family auxiliary-functions
#' @keywords character manip
#' @seealso base::strsplit utils::read.fwf
#' @examples
#'
#' # Splitting by characters
#' x <- c("a-b-cc", "d-ff-g")
#' (y <- separate(x, ".")) # a split character that does not occur
#' stopifnot(is.matrix(y), y[, 1L] == x)
#' (y <- separate(x, "-")) # a split character that does occur
#' stopifnot(is.matrix(y), dim(y) == c(2, 3))
#'
#' # Fixed-with splitting
#' x <- c("  abd  efgh", " ABCD EFGH ", " xyz")
#' (y <- separate(x, TRUE))
#' stopifnot(is.matrix(y), dim(y) == c(3, 2))
#'
#' # Applied to factors
#' xx <- as.factor(x)
#' (yy <- separate(xx, TRUE))
#' stopifnot(identical(yy, as.data.frame(y)))
#'
#' # List-wise splitting
#' x <- c("a,b", "c,b", "a,c")
#' (y <- separate(x, ",", list.wise = TRUE))
#' stopifnot(is.matrix(y), dim(y) == c(3, 3), is.logical(y))
#'
#' # Data-frame method
#' x <- data.frame(a = 1:2, b = c("a-b-cc", "a-ff-g"))
#' (y <- separate(x, coerce = FALSE))
#' stopifnot(identical(x, y))
#' (y <- separate(x)) # only character/factor columns are split
#' stopifnot(is.data.frame(y), dim(y) == c(2, 4))
#' stopifnot(sapply(y, class) == c("integer", "factor", "factor", "factor"))
#' (y <- separate(x, keep.const = FALSE))
#' stopifnot(is.data.frame(y), dim(y) == c(2, 3))
#' stopifnot(sapply(y, class) == c("integer", "factor", "factor"))
#'
setGeneric("separate", function(object, ...) standardGeneric("separate"))

setMethod("separate", "character", function(object, split = opm_opt("split"),
    simplify = FALSE, keep.const = TRUE, list.wise = FALSE,
    strip.white = list.wise) {

  strip_white <- function(x) sub("\\s+$", "", sub("^\\s+", "", x, FALSE, TRUE),
    FALSE, TRUE)

  p0 <- function(x) paste0(x, collapse = "")

  simple_if <- function(x, keep.const, simplify) {
    if (is.matrix(x)) {
      if (!keep.const) {
        if (all(const <- is_constant(x, 2L)) && simplify)
          x <- x[, 1L, drop = FALSE]
        else
          x <- x[, !const, drop = FALSE]
      }
      if (simplify && ncol(x) == 1L)
        x[, 1L]
      else
        x
    } else if (simplify)
      x
    else if (length(x))
      matrix(x)
    else
      matrix(NA_character_, 0L, 0L)
  }

  # create regexp for splitting
  char_group <- function(single, multiple) {
    if (length(single))
      if (length(multiple))
        sprintf("([%s]|[%s]+)", p0(single), p0(multiple))
      else
        sprintf("[%s]", p0(single))
    else if (length(multiple))
      sprintf("[%s]+", p0(multiple))
    else
      NA_character_ # does not split at all
  }

  # splitting at positions that contain whitespace in all strings
  split_fixed <- function(x) {
    ws <- c(" ", "\t", "\v", "\r", "\n", "\b", "\a", "\f")
    x <- strsplit(x, "", TRUE)
    max.len <- max(lengths(x, FALSE))
    x <- lapply(x, function(y) c(y, rep.int(" ", max.len - length(y))))
    x <- do.call(rbind, x)
    groups <- sections(apply(x, 2L, function(y) all(y %in% ws)))
    x <- apply(x, 1L, split.default, groups)
    x <- lapply(x, function(y) strip_white(vapply(y, p0, "")))
    do.call(rbind, x)
  }

  yields_constant <- function(char, x) {
    splits_constant <- function(char, x, ...)
      is_constant(lengths(strsplit(x, char, ...), FALSE))
    if (splits_constant(sprintf("[%s]+", char), x, FALSE, TRUE))
      2L
    else if (splits_constant(char, x, TRUE))
      1L
    else
      0L
  }

  # collect words after splitting and mark their occurrences
  word_occurrences <- function(x, split, strip.white) {
    x <- strsplit(x, sprintf("[%s]", p0(split)), FALSE, TRUE)
    if (strip.white)
      x <- lapply(x, strip_white)
    chars <- unlist(x, FALSE)
    chars <- unique.default(chars[!is.na(chars)])
    result <- matrix(FALSE, length(x), length(chars))
    colnames(result) <- sort.int(chars)
    rownames(result) <- names(x)
    for (i in seq_along(x))
      if (identical(x[[i]], NA_character_))
        result[i, ] <- NA
      else
        result[i, x[[i]]] <- TRUE
    result
  }

  LL(list.wise, strip.white, simplify, keep.const)

  # Fixed-width splitting mode
  if (identical(TRUE, split <- c(split)))
    return(simple_if(split_fixed(object), keep.const, simplify))
  split <- as.character(split)
  if (all(!nzchar(split <- split[!is.na(split)])))
    return(simple_if(split_fixed(object), keep.const, simplify))

  # Prepare split characters
  split <- unique.default(unlist(strsplit(split, "", TRUE), FALSE, FALSE))
  if (!length(split))
    return(simple_if(object, keep.const, simplify))
  split <- c(setdiff(split, "-"), intersect(split, "-"))

  # List-wise splitting
  if (list.wise)
    return(simple_if(word_occurrences(object, split, strip.white),
      keep.const, simplify))

  # Check and apply split characters
  yields.const <- vapply(split, yields_constant, 0L, object)
  split <- char_group(split[yields.const == 1L], split[yields.const == 2L])
  object <- do.call(rbind, strsplit(object, split, FALSE, TRUE))
  if (strip.white)
    object[] <- strip_white(object)
  simple_if(object, keep.const, simplify)

}, sealed = SEALED)

setMethod("separate", "factor", function(object, split = opm_opt("split"),
    simplify = FALSE, keep.const = TRUE, ...) {
  result <- separate(as.character(object), split = split,
    keep.const = keep.const, simplify = FALSE, ...)
  if (L(simplify) && ncol(result) == 1L)
    as.factor(result[, 1L])
  else
    as.data.frame(result, stringsAsFactors = TRUE, optional = TRUE)
}, sealed = SEALED)

setMethod("separate", "data.frame", function(object, split = opm_opt("split"),
    simplify = FALSE, keep.const = TRUE, coerce = TRUE, name.sep = ".", ...) {
  LL(coerce, name.sep, simplify)
  object <- do.call(cbind, mapply(function(x, name) {
    result <- if (is.character(x))
      as.data.frame(separate(x, split = split, keep.const = keep.const,
        simplify = FALSE, ...), stringsAsFactors = FALSE, optional = TRUE)
    else if (coerce && is.factor(x))
      separate(x, split = split, keep.const = keep.const,
        simplify = FALSE, ...)
    else
      as.data.frame(x, stringsAsFactors = FALSE, optional = TRUE)
    case(ncol(result),
      if (keep.const)
        result[, name] <- x,
      names(result) <- name,
      names(result) <- paste(name, seq_len(ncol(result)), sep = name.sep)
    )
    result
  }, object, names(object), SIMPLIFY = FALSE, USE.NAMES = FALSE))
  if (ncol(object) == 1L && simplify)
    object <- object[, 1L]
  object
}, sealed = SEALED)


################################################################################
################################################################################
#
# Global options
#


#' Global \pkg{opm} options
#'
#' Get and set global \pkg{opm} options, or get list of predefined parameter
#' names of interest for the user
#'
#' @param x Character scalar or list or missing. If not given, all current
#'   settings are returned (as a named list). If \code{x} is a character scalar,
#'   it is used for querying for a single value.
#'
#'   If \code{x} is a list, it is expected to contain key-value pairs that can
#'   be set. In that case, it is an error if a key is unknown, i.e. it is an
#'   error to use a name that is not already contained (\pkg{opm} would never
#'   query for it anyway). It is also illegal to attempt set novel values whose
#'   classes are not identical to, or derived from, the classes of the old
#'   value. Further, it is illegal to set a zero-length value.
#' @param ... Optional arguments. If \code{x} is missing, these arguments are
#'   concatenated into a list and used as if \code{x} was given as a list (see
#'   above). That is, the argument names are used as the keys for setting
#'   values. This is usually easier than working with a list.
#' @return List or atomic vector. If one to several values are set, the previous
#'   entries are returned invisibly.
#' @param what Character scalar indicating the parameters of interest. See
#'   \sQuote{Details}.
#' @family auxiliary-functions
#' @details The following keys can be used with the following kinds of values:
#'   \describe{
#'     \item{colors}{Default colour set used by the \code{\link{OPMS}} method
#'       of \code{\link{xy_plot}} and other plotting functions.}
#'     \item{color.borders}{Character vector with default colour borders between
#'       which \code{\link{level_plot}} interpolates to obtain a colour
#'       palette.}
#'     \item{comb.key.join}{Used by functions that support combination of
#'       metadata entries converted to data-frame columns immediately after
#'       their selection. Sets the character string that is used when joining
#'       old names to new name. Should normally only be a single character.}
#'     \item{comb.value.join}{Used by functions that support combination of
#'       metadata entries converted to data-frame columns immediately after
#'       their selection. Sets the character string that is used when joining
#'       old values to new values. Should normally only be a single character;
#'       must be a single character when used by \code{\link{opm_mcp}}.}
#'     \item{contrast.type}{Character scalar indicating the default type of
#'       contrast used by \code{\link{opm_mcp}}.}
#'     \item{css.file}{Character scalar. Default \acronym{CSS} file linked by
#'       \code{\link{phylo_data}} when producing \acronym{HTML} output. Ignored
#'       if empty.}
#'     \item{csv.keys}{Character vector with names of entries of
#'       \code{\link{csv_data}} be used by \code{\link{include_metadata}}.
#'       Should be kept a subset of \code{opm_opt("csv.selection")}.}
#'     \item{csv.selection}{Character vector with names of entries of
#'       \code{\link{csv_data}} (must be a valid \sQuote{keys} argument) to be
#'       extracted by \code{\link{collect_template}}.}
#'     \item{curve.param}{Character scalar. Default \sQuote{subset} argument of
#'       \code{\link{extract}} and the plotting functions.}
#'     \item{disc.param}{Character scalar. Default \sQuote{subset} argument of
#'       \code{\link{do_disc}}. It is usually not advisable to  change it.}
#'     \item{digits}{Integer scalar. Number of digits used by some functions
#'       generating output text.}
#'     \item{file.encoding}{Character scalar. Character encoding in input files
#'       as assumed by \code{\link{read_opm}}.}
#'     \item{file.split.tmpl}{Character scalar. Template used as \sQuote{format}
#'       argument by \code{\link{split_files}}.}
#'     \item{gen.iii}{Character scalar indicating whether \code{\link{read_opm}}
#'       and other IO functions based on it automatically convert to this plate
#'       type. If empty, nothing is changed.}
#'     \item{group.name}{Character scalar used as column name for trivial
#'       groups (either all items in the same group or each item in a group of
#'       its own) created by \code{\link{extract}}.}
#'     \item{heatmap.colors}{Colour palette used by \code{\link{heat_map}}.}
#'     \item{html.attr}{Used by \code{\link{phylo_data}} for automatically
#'       creating \acronym{HTML} \sQuote{title} and \sQuote{class} attributes.}
#'     \item{input.try.order}{Integer vector indicating the preferred order when
#'       trying to read \acronym{CSV} files with \code{\link{read_single_opm}}.
#'       See there for details.}
#'     \item{key.join}{Used by \code{\link{metadata}} and some other functions
#'       that must be in sync with it for joining metadata keys used in nested
#'       queries (because the resulting object is \sQuote{flat}).}
#'     \item{machine.id}{Integer scalar that can be used for identifying an
#'       OmniLog\eqn{\textsuperscript{\textregistered}}{(R)} instrument. Useful
#'       for \code{\link{collect_template}} if several such machines are in
#'       use.}
#'     \item{max.chars}{Integer scalar used when abbreviating full substrate
#'       names. See \code{\link{wells}} for an example.}
#'     \item{md.duration}{Default name of the key to be treated as overall
#'       running time in the metadata. Currently only relevant for easing the
#'       inclusion of the total measurement duration in the metadata.}
#'     \item{md.id.name}{Default name of the key to be treated as \acronym{ID}
#'       in the metadata. Currently only relevant for easing the inclusion of
#'       plate-specific \acronym{ID}s in the metadata.}
#'     \item{md.id.start}{The first value to inserted next as \acronym{ID}
#'       value in the metadata. Currently only relevant for easing the inclusion
#'       of plate-specific \acronym{ID}s in the metadata.}
#'     \item{min.mode}{Used when making discretisation results uniform within a
#'       group. The minimum proportion the most frequent value much reach to be
#'       used for representing all values (if less, frequent, \code{NA} is
#'       used). Must be a numeric scalar between 0 and 1.}
#'     \item{phylo.fmt}{Character scalar indicating the default output format
#'       used by \code{\link{phylo_data}}.}
#'     \item{split}{Character scalar indicating the default splitting characters
#'       used by \code{\link{separate}}.}
#'     \item{strict.OPMD}{Logical scalar indicating whether \code{\link{OPMD}}
#'       objects can only be created if the discretised data are consistent with
#'       the parameter from which they have been estimated.}
#'     \item{threshold}{Numeric scalar indicating the default threshold used by
#'       \code{\link{annotated}}.}
#'     \item{time.zone}{Character scalar indicating the time zone to be used
#'       when parsing \code{setup_time} entries. This is relevant for
#'       \code{\link{merge}}, which by default attempts to sort by parsed setup
#'       times}
#'     \item{time.fmt}{Character vector indicating the time formats used for
#'       parsing the \code{setup_time} entries (in the given order). Also
#'       relevant for \code{\link{merge}} by default. It is advisable to put the
#'       more specific formats to the front because otherwise information such
#'       as an \sQuote{AM} or \sQuote{PM} indication might be lost. A wrong
#'       format might well match a given entry, causing \pkg{opm} to
#'       misinterpret the time or even the date.}
#'     \item{warn.mult}{Issue a warning if \code{\link{read_single_opm}} reads
#'       a multiple-plate old-style or new-style \acronym{CSV} file.}
#'   }
#'
#'   The following kinds of parameter names applied by \pkg{opm} cannot be
#'   modified by the user (but displayed using \code{param_names}):
#'   \describe{
#'     \item{param.names}{Names of the estimated curve parameters used
#'     internally and in the output.}
#'     \item{disc.name}{Alternative name used to select discretised values
#'       instead.}
#'     \item{reserved.md.names}{Names that should not be used in metadata
#'     entries because they are used as predefined column names by functions
#'     such as \code{\link{flatten}}.}
#'     \item{split.at}{The name of the column in data frames generated by
#'     \code{\link{extract}} that separates data from metadata.}
#'     \item{hours}{The name of the column that holds the overall running time
#'     when \code{\link{extract}} is used to obtain it.}
#'   }
#' @keywords utilities
#' @seealso base::options base::getOption
#' @export
#' @examples
#'
#' # fetching a value
#' (digits <- opm_opt("digits"))
#' stopifnot(digits == 4)
#'
#' # setting a value; previous value is returned as list
#' (old.opts <- opm_opt(digits = 5L))
#' stopifnot(is.list(old.opts), length(old.opts) == 1L)
#' stopifnot(old.opts$digits == 4)
#'
#' # fetching the value again: should now be changed
#' (digits <- opm_opt("digits"))
#' stopifnot(digits == 5)
#'
#' # resetting the value
#' (old.opts <- opm_opt(old.opts))
#' stopifnot(is.list(old.opts), length(old.opts) == 1L)
#' stopifnot(old.opts$digits == 5)
#' (digits <- opm_opt("digits"))
#' stopifnot(digits == 4)
#'
#' ## reserved parameter names
#' (x <- param_names())
#' stopifnot(is.character(x), length(x) > 1, identical(unique(x), x))
#' (x <- param_names("reserved"))
#' stopifnot(is.character(x), length(x) > 1, identical(unique(x), x))
#' stopifnot(param_names("split.at") %in% x)
#'
setGeneric("opm_opt", function(x, ...) standardGeneric("opm_opt"))

setMethod("opm_opt", "list", function(x) {
  old <- mget(names(x), OPM_OPTIONS) # fails if names are missing
  for (i in seq_along(x)) {
    if (!length(value <- x[[i]]))
      stop("empty value provided for key '%s'", names(x)[i])
    if (!all(inherits(value, class(old[[i]]), TRUE)))
      stop(sprintf("new and old value have conflicting class(es) for key '%s'",
        names(x)[i]))
  }
  list2env(x, OPM_OPTIONS)
  invisible(old)
}, sealed = SEALED)

setMethod("opm_opt", "missing", function(x, ...) {
  if (nargs())
    opm_opt(list(...))
  else
    as.list(OPM_OPTIONS)
}, sealed = SEALED)

setMethod("opm_opt", "character", function(x) {
  get(x, , OPM_OPTIONS, "any", FALSE)
}, sealed = SEALED)

#= param_names opm_opt

#' @rdname opm_opt
#' @export
#'
setGeneric("param_names", function(what) standardGeneric("param_names"))

setMethod("param_names", "missing", function(what) {
  CURVE_PARAMS
}, sealed = SEALED)

setMethod("param_names", "character", function(what) {
  switch(EXPR = match.arg(what, c("param.names", "disc.name",
      "reserved.md.names", "split.at", "hours")),
    param.names = CURVE_PARAMS,
    disc.name = DISC_PARAM,
    reserved.md.names = unname(RESERVED_NAMES),
    split.at = RESERVED_NAMES[["parameter"]],
    hours = HOUR)
}, sealed = SEALED)


