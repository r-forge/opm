


################################################################################
################################################################################
#
# Miscellaneous methods
#


#' Merge or split plates
#'
#' Combine all plates in a single \code{\link{OPM}} object by treating them as
#' originating from subsequent runs of the same experimental plate. Adjust the
#' times accordingly. Alternatively, split plates according to the contained
#' regular series of substrates, if any. The \code{\link{MOPMX}} method merges
#' according to plate types, optionally including a novel element.
#'
#' @param x \code{\link{OPMX}} or \code{\link{MOPMX}} object.
#' @param y For the \code{\link{OPMS}} method a numeric vector indicating the
#'   time(s) (in hours) between two subsequent plates. Must be positive
#'   throughout, and its length should fit to the number of plates (e.g., either
#'   \code{1} or \code{length(x) - 1} would work). If missing, \code{0.25} is
#'   used.
#'
#'   If \code{x} is an \code{\link{OPM}} object, a missing or numeric \code{y}
#'   argument causes \code{merge} to just return \code{x} because there is
#'   nothing to merge. But \code{y} can be an \code{\link{OPM}} object in that
#'   case, which, if compatible, will be merged with \code{x}.
#'
#'   For the \code{\link{MOPMX}} method, the optional \code{y} can be any object
#'   that can be convert to the class of \code{x} using \code{as}.
#'
#' @param sort.first Logical scalar. Sort the plates according to their setup
#'   times before merging?
#' @param parse Logical scalar. Ignored unless \code{sort.first} is \code{TRUE}.
#'   For sorting, parse the setup times using \code{strptime} from the
#'   \pkg{base} package? It is an error if this does not work, but see
#'   \sQuote{Details}.
#' @param f For the \code{\link{OPMX}} methods, a factor or missing. If missing,
#'   the behaviour is special. Splitting is applied to the plates themselves and
#'   attempted according to the positions of substrates within series as
#'   revealed by \code{\link{substrate_info}} in \sQuote{concentration} mode.
#'
#'   If a factor, \code{f} is used as in the default \code{split} method from
#'   the \pkg{base} package, yielding a list (\code{\link{MOPMX}} object) of
#'   single or multiple plates.
#'
#'   If neither missing nor a factor, \code{f} is used as \code{key} argument
#'   of \code{\link{metadata}}. The resulting entries are pasted together per
#'   plate and converted to a factor used for splitting \code{x}.
#'
#'   For the \code{\link{MOPMX}} methods, f is a factor, a list of factors, or
#'   an object suitable as \code{\link{metadata}} key. If a factor, it is
#'   directly used for splitting \code{x}. If a list of factors, the factor
#'   lengths must correspond to the lengths of the elements of \code{x}, in
#'   turn. Each element of \code{x} is then split separately, and the resulting
#'   \code{\link{MOPMX}} objects are reassigned, yielding a list with one
#'   \code{\link{MOPMX}} object per factor level. Factor levels that do not
#'   occur in some of the elements of \code{x} are dropped, with a warning,
#'   independent of \code{drop} argument.
#'
#'   If \code{f} is neither a factor nor a list of factors, such a list of
#'   factors is generated from the metadata, with \code{NULL} results replaced
#'   by \code{NA}.
#'
#' @param drop Passed to \code{\link{[}}. The default is \code{FALSE}.
#' @export
#' @return The \code{\link{OPMX}} method of \code{merge} yields an
#'   \code{\link{OPM}} object. The \code{\link{metadata}} and
#'   \code{\link{csv_data}} will be taken from the first contained plate, but
#'   aggregated values, if any, will be dropped.
#'
#'   The \code{\link{MOPMX}} method for \code{merge} yields a
#'   \code{\link{MOPMX}} object with a potentially different number of elements.
#'
#'   The \code{split} methods yield either an \code{\link{OPMS}} or an
#'   \code{\link{MOPMX}} object; the \code{\link{MOPMX}} method for \code{split}
#'   yields a list of \code{\link{MOPMX}} objects.
#'
#' @details This \code{\link{OPMS}} method of \code{merge} is intended for
#'   dealing with slowly growing or reacting organisms that need to be analysed
#'   with subsequent runs of the same plate in \acronym{PM} mode. Results
#'   obtained with \emph{Geodermatophilus} strains and Generation-III plates
#'   indicate that this works well in practice. See the references, and see the
#'   documentation of the \code{montero_et_al} data set in the \pkg{opmdata}
#'   package.
#'
#'   See the arguments \code{time.fmt} and \code{time.zone} of
#'   \code{\link{opm_opt}} for modifying the parsing of setup-time entries. If
#'   it does not work, additional time-string templates must be stored.
#'
#'   The \code{CMAT} method of \code{merge} is only for internal use.
#'
#'   The \code{split} methods with missing \code{f} are for splitting plates
#'   that contain series of substrate-usage assays as indicated in the full
#'   substrate names (mostly interpretable as concentrations).
#'   \code{\link{OPMS}} objects are generated that contain each replicate within
#'   the series in a separate plate and the replicate \acronym{ID} indicated in
#'   the metadata entry given by \code{\link{opm_opt}("series.key")}. This
#'   allows for comparisons between within-plate replicates.
#'
#'   Splitting according to substrate series will not work if these are not
#'   regular, i.e. the same substrates occur in each replicate. In such cases
#'   \code{x} will be returned, with a warning. Substrates without a replicate
#'   (\sQuote{concentration}) indicator would silently be skipped, however. The
#'   composition and order of the wells per pseudo-plate must be made uniform.
#'   This is done by enforcing well names and well ordering of the first
#'   replicate in all forthcoming replicates.
#'
#'   After a successful splitting, the numeric suffixes in the full well names
#'   make no sense any more, as each plate contains a constant set of such
#'   suffixes. The \code{no.num} argument of \code{\link{wells}} and the
#'   dependent methods can be used to remove the suffixes before displaying the
#'   full well names.
#'
#'   The \code{\link{MOPMX}} method for \code{merge} will raise an error if
#'   elements occur within \code{x} (and optionally \code{y}) that have the same
#'   plate type but cannot be combined any way because they contain distinct
#'   sets of wells. See the comments on combining plates into a
#'   \code{\link{OPMS}} object.
#'
#' @references Montero-Calasanz, M. d. C., Goeker, M.,  Poetter, G., Rohde, M.,
#'   Sproeer, C., Schumann, P., Gorbushina, A. A., Klenk, H.-P. 2012
#'   \emph{Geodermatophilus arenarius} sp. nov., a xerophilic actinomycete
#'   isolated from Saharan desert sand in Chad. \emph{Extremophiles}
#'   \strong{16}, 903--909.
#' @references Montero-Calasanz, M. d. C., Goeker, M., Rohde, M., Schumann, P.,
#'   Poetter, G., Sproeer, C., Gorbushina, A. A., Klenk, H.-P. 2013
#'   \emph{Geodermatophilus siccatus} sp. nov., isolated from arid sand of the
#'   Saharan desert in Chad. \emph{Antonie van Leeuwenhoek} \strong{103},
#'   449--456.
#'
#' @family conversion-functions
#' @seealso opmdata::montero_et_al
#' @keywords manip
#' @examples
#'
#' ## merge: OPM methods
#' stopifnot(identical(merge(vaas_1, 0.5), vaas_1)) # nothing to merge
#' summary(x <- merge(vaas_1, vaas_1)) # biologically unreasonable!
#' stopifnot(is(x, "OPM"), dim(x) == c(2 * hours(vaas_1, "size"), 96))
#'
#' ## merge: OPMS methods
#' summary(x <- merge(vaas_4)) # biologically unreasonable for these data!
#' stopifnot(is(x, "OPM"), dim(x) == c(sum(hours(vaas_4, "size")), 96))
#'
#' # See opmdata::montero_et_al for an object to which this can be sensibly
#' # applied. An according example is given in the montero_et_al documentation.
#'
#' ## split: OPM methods
#' (x <- split(vaas_1))
#' metadata(x, opm_opt("series.key"))
#' stopifnot(is(x, "OPMS"), dim(x) == c(2, hours(vaas_1, "size"), 1))
#' # only D-Serine is present as series, all other wells are skipped
#' # thus split is more useful when applied to other plate types such as "ECO"
#'
#' (x <- split(vaas_1, "Species"))
#' stopifnot(is(x, "MOPMX"), length(x) == 1)
#'
#' ## split: OPMS methods
#' (x <- split(vaas_4))
#' metadata(x, opm_opt("series.key"))
#' stopifnot(is(x, "OPMS"), dim(x) == c(8, hours(vaas_4, "size")[1], 1))
#'
#' (x <- split(vaas_4, "Species"))
#' stopifnot(is(x, "MOPMX"), length(x) == 2)
#'
#' # Split into list of OPMS objects with the same overall measurement hours
#' x <- split(vaas_4, as.factor(hours(vaas_4)))
#' stopifnot(is(x, "MOPMX"), length(x) == 1, class(x[[1]]) == "OPMS")
#' # ... because the running times were actually already identical, the list
#' # contains only a single element.
#'
setGeneric("merge")

setMethod("merge", c("OPM", "missing"), function(x, y, sort.first = TRUE,
    parse = TRUE) {
  x
}, sealed = SEALED)

setMethod("merge", c("OPM", "numeric"), function(x, y, sort.first = TRUE,
    parse = TRUE) {
  x
}, sealed = SEALED)

setMethod("merge", c("OPM", "OPM"), function(x, y, sort.first = TRUE,
    parse = TRUE) {
  merge(new("OPMS", plates = list(x, y)), 0.25, sort.first, parse)
}, sealed = SEALED)

setMethod("merge", c("OPMS", "numeric"), function(x, y, sort.first = TRUE,
    parse = TRUE) {
  if (any(y <= 0))
    stop("'y' must be positive throughout")
  if (L(sort.first))
    x <- sort(x, by = "setup_time", parse = parse, na.last = TRUE)
  m <- do.call(rbind, measurements(x))
  if (is.matrix(tp <- hours(x, "all"))) {
    to.add <- c(0, must(cumsum(tp[-nrow(tp), ncol(tp), drop = FALSE] + y)))
    m[, 1L] <- as.vector(t(tp + to.add))
  } else if (is.list(tp)) {
    to.add <- c(0, must(cumsum(vapply(tp[-length(tp)], tail, 1, 1L) + y)))
    m[, 1L] <- unlist(mapply(`+`, tp, to.add, SIMPLIFY = FALSE,
      USE.NAMES = FALSE), FALSE, FALSE)
  } else {
    stop(BUG_MSG)
  }
  new("OPM", measurements = m, csv_data = csv_data(x[1L]),
    metadata = metadata(x[1L]))
}, sealed = SEALED)

setMethod("merge", c("OPMS", "missing"), function(x, y, sort.first = TRUE,
    parse = TRUE) {
  merge(x, 0.25, sort.first, parse)
}, sealed = SEALED)

setMethod("merge", c("MOPMX", "missing"), function(x, y) {
  combine <- function(x) if (length(x <- plates(x)) > 1L)
      new("OPMS", plates = x)
    else
      x[[1L]]
  if (!anyDuplicated.default(pt <- plate_type(x)))
    return(x)
  x@.Data <- lapply(split.default(x@.Data, as.factor(pt)), combine)
  x
}, sealed = SEALED)

setMethod("merge", c("MOPMX", "ANY"), function(x, y) {
  merge(x + y)
}, sealed = SEALED)

setMethod("merge", c("CMAT", "logical"), function(x, y) {
  merge(x, if (L(y))
      as.factor(rownames(x))
    else
      as.factor(seq_len(nrow(x))))
}, sealed = SEALED)

setMethod("merge", c("CMAT", "ANY"), function(x, y) {
  merge(x, as.factor(y))
}, sealed = SEALED)

setMethod("merge", c("CMAT", "factor"), function(x, y) {
  if (length(y) != nrow(x)) # this also covers NULL row names
    stop("length of 'y' not equal to number of rows")
  if (anyNA(y))
    stop("'y' must not contain NA values")
  if (length(levels(y)) == length(y))
    return(x)
  cn <- colnames(x) # later put back, avoiding correction of duplicate names
  x <- aggregate(as.data.frame(x, stringsAsFactors = FALSE), by = list(y),
    FUN = c, recursive = TRUE, simplify = FALSE)
  x <- as.matrix(x[, -1L, drop = FALSE])
  x[] <- lapply(x, sort.int, na.last = TRUE)
  rownames(x) <- levels(y)
  colnames(x) <- cn
  new("CMAT", x)
}, sealed = SEALED)

#= split merge

#' @rdname merge
#' @export
#'
setGeneric("split")

setMethod("split", c("OPM", "missing", "missing"), function(x, f, drop) {
  split(x, drop = FALSE)
}, sealed = SEALED)

setMethod("split", c("OPMS", "missing", "missing"), function(x, f, drop) {
  split(x, drop = FALSE)
}, sealed = SEALED)

setMethod("split", c("OPM", "missing", "ANY"), function(x, f, drop) {
  extract_concentration <- function(x) {
    m <- regexpr("(?<=#)\\s*\\d+\\s*$", x, FALSE, TRUE)
    conc <- as.integer(substr(x, m, m + attr(m, "match.length") - 1L))
    regmatches(x, m) <- "1"
    list(Concentration = conc, Standardized = structure(names(x), names = x))
  }
  regular_size <- function(x) {
    counts <- tabulate(x$Concentration)
    length(counts) > 1L || all(duplicated.default(counts)[-1L])
  }
  regular_composition <- function(x) {
    for (i in seq_along(x)[-1L])
      if (!setequal(names(x[[1L]]), names(x[[i]])))
        return(FALSE)
    TRUE
  }
  get_and_rename <- function(x, w1, w2, conc, drop, key) {
    x <- rename_wells(x[, w1, drop = drop], w2)
    x@metadata[[key]] <- conc
    x
  }
  w <- extract_concentration(wells(x, TRUE, FALSE))
  if (!regular_size(w) || !regular_composition(
      w <- split.default(w$Standardized, w$Concentration))) {
    warning("no regular concentration structure found")
    return(x)
  }
  for (i in seq_along(w)[-1L])
    w[[i]] <- w[[i]][names(w[[1L]])]
  new("OPMS", plates = mapply(get_and_rename, conc = as.integer(names(w)),
    w1 = w, SIMPLIFY = FALSE, USE.NAMES = FALSE, MoreArgs = list(x = x,
      w2 = w[[1L]], drop = drop, key = get("series.key", OPM_OPTIONS))))
}, sealed = SEALED)

setMethod("split", c("OPMS", "missing", "ANY"), function(x, f, drop) {
  x@plates <- lapply(x@plates, split, drop = drop)
  x@plates <- unlist(lapply(x@plates, slot, "plates"), FALSE, FALSE)
  x
}, sealed = SEALED)

setMethod("split", c("OPM", "ANY", "missing"), function(x, f, drop) {
  split(x, f, FALSE)
}, sealed = SEALED)

setMethod("split", c("OPMS", "ANY", "missing"), function(x, f, drop) {
  split(x, f, FALSE)
}, sealed = SEALED)

setMethod("split", c("OPM", "factor", "missing"), function(x, f, drop) {
  split(x, f, FALSE)
}, sealed = SEALED)

setMethod("split", c("OPM", "factor", "ANY"), function(x, f, drop) {
  object <- split.default(0L, f, FALSE) # to get the warnings/errors
  object[[1L]] <- x[drop = drop]
  new("MOPMX", object)
}, sealed = SEALED)

setMethod("split", c("OPMS", "factor", "missing"), function(x, f, drop) {
  split(x, f, FALSE)
}, sealed = SEALED)

setMethod("split", c("OPMS", "factor", "ANY"), function(x, f, drop) {
  new("MOPMX", lapply(split.default(x, f, FALSE), `[`, drop = drop))
}, sealed = SEALED)

setMethod("split", c("OPMX", "ANY", "ANY"), function(x, f, drop) {
  split(x, as.factor(extract_columns(x, f, TRUE, " ", "ignore")), drop)
}, sealed = SEALED)

setMethod("split", c("MOPMX", "factor", "missing"), function(x, f, drop) {
  split.default(x, f, FALSE)
}, sealed = SEALED)

setMethod("split", c("MOPMX", "factor", "ANY"), function(x, f, drop) {
  split.default(x, f, drop)
}, sealed = SEALED)

setMethod("split", c("MOPMX", "list", "missing"), function(x, f, drop) {
  split(x, f, FALSE)
}, sealed = SEALED)

setMethod("split", c("MOPMX", "list", "ANY"), function(x, f, drop) {
  if (!all(vapply(f, is.factor, NA)))
    f <- metadata2factorlist(x, f)
  x <- mapply(split, x = x, f = f, MoreArgs = list(drop = drop),
    SIMPLIFY = FALSE)
  f <- sort.int(unique.default(unlist(lapply(f, levels), FALSE, FALSE)))
  result <- structure(vector("list", length(f)), names = f)
  for (level in f)
    result[[level]] <- lapply(x, `[[`, i = level)
  lapply(lapply(result, close_index_gaps), as, "MOPMX")
}, sealed = SEALED)

setMethod("split", c("MOPMX", "ANY", "missing"), function(x, f, drop) {
  split(x, f, FALSE)
}, sealed = SEALED)

setMethod("split", c("MOPMX", "ANY", "ANY"), function(x, f, drop) {
  split(x, metadata2factorlist(x, f), drop)
}, sealed = SEALED)


################################################################################


#' Get available plates or apply function to them
#'
#' Get all \code{plates} contained in an \code{\link{OPMS}} object or a list, or
#' create a list containing a single \code{\link{OPM}} object as element, or
#' apply a function to a collection of \code{\link{OPM}} objects.
#'
#' @param object List, \code{\link{OPM}}, \code{\link{OPMS}} or
#'   \code{\link{MOPMX}} object.
#' @param fun A function. Should accept an \code{\link{OPM}} object as first
#'   argument.
#' @param ... Optional other arguments passed to \code{fun}.
#' @param simplify Logical scalar. If \code{FALSE}, the result is a list. If
#'   \code{TRUE}, it is attempted to simplify this to a vector, matrix or
#'   \code{\link{OPMS}} object (if the result is a list of \code{\link{OPM}} or
#'   \code{\link{OPMA}} objects). If this is impossible, a list is returned.
#'   The \code{\link{MOPMX}} method tries creating a \code{\link{MOPMX}} object
#'   again after removing \code{NULL} values, if any.
#'
#' @return For \code{plates}, a list of \code{\link{OPM}} objects (may be empty
#'   instead if \code{object} is a list). The result of \code{oapply} depends on
#'   \code{fun} and \code{simplify}: a list, vector, matrix or
#'   \code{\link{OPMS}} object are possible outcomes.
#' @export
#' @family conversion-functions
#' @keywords attribute manip
#' @details
#'   The list method of \code{plates} traverses the input recursively and skips
#'   all objects of other classes than \code{\link{OPM}}. See also
#'   \code{\link{opms}}, which is somewhat similar but more flexible.
#'
#'   \code{oapply} applies a function to all \code{\link{OPM}} objects within an
#'   \code{\link{OPMS}} object. Optionally it simplifies the result to an
#'   \code{\link{OPMS}} object if possible, or other structures simpler than a
#'   list. The \code{\link{OPM}} method of simply applies \code{fun} once (to
#'   \code{object}).
#'
#' @seealso base::list base::as.list base::sapply
#' @examples
#' # plates(), 'OPM' method
#' summary(x <- plates(vaas_1)) # => list of OPM objects
#' stopifnot(is.list(x), length(x) == 1L, sapply(x, inherits, what = "OPM"))
#'
#' # plates(), 'OPMS' method
#' summary(x <- plates(vaas_4)) # => list of OPM objects
#' stopifnot(is.list(x), length(x) == 4L, sapply(x, inherits, what = "OPM"))
#'
#' # plates(), list method
#' x <- list(vaas_1, letters, vaas_4, 1:10)
#' summary(x <- plates(x)) # => list of OPM objects
#' stopifnot(is.list(x), length(x) == 5, sapply(x, inherits, what = "OPM"))
#'
#' ## oapply()
#' summary(x <- oapply(vaas_4, identity)) # trivial
#' stopifnot(identical(x, vaas_4))
#' summary(x <- oapply(vaas_4, identity, simplify = FALSE)) # => yields list
#' stopifnot(is.list(x), length(x) == 4, sapply(x, class) == "OPMD")
#'
setGeneric("plates", function(object, ...) standardGeneric("plates"))

setMethod("plates", "WMDS", function(object) {
  object@plates
}, sealed = SEALED)

setMethod("plates", "WMD", function(object) {
  list(object)
}, sealed = SEALED)

setMethod("plates", "list", function(object) {
  to_opm_list.list(object, TRUE, TRUE, FALSE)
}, sealed = SEALED)

setMethod("plates", "MOPMX", function(object) {
  unlist(lapply(object@.Data, plates), FALSE)
}, sealed = SEALED)

## maybe the following method could make sense, too
## but maybe it would be too expensive
# setMethod("plates", "character", function(object) {
#   x <- mget(object, globalenv(), "any", list(NULL), TRUE)
#   to_opm_list.list(x, TRUE, TRUE, FALSE)
# }, sealed = FALSE)

#= oapply plates

#' @rdname plates
#' @export
#'
setGeneric("oapply", function(object, fun, ...) standardGeneric("oapply"))

setMethod("oapply", "OPM", function(object, fun, ...,
    simplify = TRUE) {
  fun(object, ...)
}, sealed = SEALED)

setMethod("oapply", "OPMS", function(object, fun, ...,
    simplify = TRUE) {
  result <- sapply(X = object@plates, FUN = fun, ..., simplify = simplify,
    USE.NAMES = FALSE)
  if (simplify && is.list(result))
    result <- try_opms.list(result)
  result
}, sealed = SEALED)

setMethod("oapply", "MOPMX", function(object, fun, ...,
    simplify = TRUE) {
  result <- sapply(X = object, FUN = fun, ..., simplify = simplify,
    USE.NAMES = TRUE) # using object@.Data would lose the names
  if (simplify && is.list(result))
    tryCatch(new(class(object), result[!vapply(result, is.null, NA)]),
      error = function(e) result)
  else
    result
}, sealed = SEALED)


################################################################################


#' Factor from flattened data
#'
#' Extract all plate-specifying information from a data frame as created by
#' \code{\link{flatten}}. If metadata have been included, these will be joined
#' together; otherwise the plate identifiers (basically numbers) themselves are
#' used.
#'
#' @param object Object as returned by \code{\link{flatten}}.
#' @param sep Character scalar. Separator used for joining the columns together.
#' @return Factor with one entry per plate.
#' @keywords internal
#'
setGeneric("flattened_to_factor",
  function(object, ...) standardGeneric("flattened_to_factor"))

setMethod("flattened_to_factor", "data.frame", function(object, sep = " ") {
  LL(plate.pos <- which(colnames(object) == RESERVED_NAMES[["plate"]]), sep)
  if (plate.pos == 1L)
    return(unique(object[, plate.pos]))
  result <- aggregate(object[, seq_len(plate.pos)],
    by = list(object[, plate.pos]), FUN = `[[`, i = 1L)
  result <- as.list(result[, seq.int(2L, ncol(result) - 1L), drop = FALSE])
  as.factor(do.call(paste, c(result, sep = sep)))
}, sealed = SEALED)


################################################################################
################################################################################
#
# Sorting etc.
#


#' Sort, unify, revert or repeat \acronym{OPMS} objects
#'
#' Sort an \code{\link{OPMS}} object based on one to several metadata or
#' \acronym{CSV} data entries, or sort elements of a \code{\link{MOPMX}} object
#' based on plate type, length, or a metadata entry. Alternatively, remove
#' duplicated elements from a \code{\link{OPMS}} or \code{\link{MOPMX}} object,
#' or revert the order of plates within an \code{\link{OPMS}} object, or, repeat
#' \code{\link{OPMS}} or \code{\link{OPM}} objects zero times, once, or several
#' times.
#'
#' @param x \code{\link{OPM}} or \code{\link{OPMS}} or \code{\link{MOPMX}}
#'   object.
#' @param decreasing Logical scalar. Passed to \code{order} or \code{sort.list}
#'   from the \pkg{base} package.
#' @param by List or character vector. For \code{\link{OPMS}} objects, if a
#'   list, a list of one to several keys passed as \code{key} argument to
#'   \code{\link{metadata}}. If a character vector of length one, \code{by} is
#'   passed as \sQuote{what} argument to \code{\link{csv_data}}. If longer,
#'   passed step-by-step to \code{\link{csv_data}} as \code{keys} argument.
#'
#'   For \code{\link{MOPMX}} objects, either \sQuote{plate.type}, which sorts
#'   according to the plate types, \sQuote{length}, which sorts the elements
#'   according to their lengths (i.e., number of plates), or a metadata query
#'   that yields, for each element of \code{x}, a vector to which \code{max}
#'   can be applied. Sorting \code{x} is then done according to these maxima.
#'
#' @param parse Logical scalar. Convert the \code{setup_time} via
#'   \code{strptime} before ordering? Has only an effect if \code{by} is
#'   \sQuote{setup_time}. It is an error if the time format is not recognised.
#'   See \code{\link{opm_opt}}, arguments \code{time.fmt} and \code{time.zone},
#'   for modifying the parsing of setup-time entries, and \code{\link{csv_data}}
#'   for this kind of entries.
#' @param exact Logical scalar. Passed to \code{\link{metadata}}. Affects only
#'   metadata querying, not directly the sorting.
#' @param strict Logical scalar. Is it an error if metadata keys are not found?
#'   If \code{FALSE}, \code{x} gets ordered according to only the found keys,
#'   and remains in the original order if none of the keys in \code{by} are
#'   found at all. Note that it is always an error if keys are found in the
#'   \code{\link{metadata}} of some of the \code{\link{plates}} but not in
#'   those of others.
#' @param na.last Logical scalar. Also passed to \code{order} or
#'   \code{sort.list}.
#' @param incomparables Vector passed to \code{\link{duplicated}}. The default
#'   is \code{FALSE}.
#' @param ... Optional arguments passed between the methods or to
#'   \code{\link{duplicated}} or to \code{rep} from the \pkg{base} package. See
#'   the examples.
#' @export
#' @return \code{\link{OPMS}} object with not necessarily the same order of
#'   plates than before, or \code{\link{OPM}} object.
#' @details
#'   The \code{sort} \code{\link{OPM}} method just returns the input data to
#'   avoid destructive effects due to the way the default \code{sort} interacts
#'   with \code{\link{OPM}} indexing.
#'
#'   \code{rev} should be slightly more efficient than calling the default
#'   \code{rev} method. There is also an \code{\link{OPM}} method which just
#'   returns the input data (to avoid destructive effects due to the way the
#'   default \code{rev} interacts with \code{\link{OPM}} indexing).
#'
#'   The \code{\link{OPM}} method of \code{unique} also returns
#'   the passed object.
#'
#'   \code{rev} yields an \code{\link{OPMS}} object with another number of
#'   plates, or an \code{\link{OPM}} object, or \code{NULL}.
#'
#' @family conversion-functions
#' @keywords manip
#' @seealso base::order base::sort base::strptime base::unique base::rev
#' @seealso base::rep
#' @examples
#'
#' ## 'OPMS' methods
#'
#' # Existing keys
#' stopifnot(is.unsorted(metadata(vaas_4, "Strain")))
#' x <- sort(vaas_4, by = list("Strain"))
#' stopifnot(is(x, "OPMS"), !is.unsorted(metadata(x, "Strain")))
#' x <- sort(vaas_4, by = list("Strain"), decreasing = TRUE)
#' stopifnot(is(x, "OPMS"), is.unsorted(metadata(x, "Strain")))
#'
#' # Non-existing keys
#' x <- try(sort(vaas_4, by = list("Not there", "Missing"), strict = TRUE))
#' stopifnot(inherits(x, "try-error")) # yields error
#' x <- try(sort(vaas_4, by = list("Not there", "Missing"), strict = FALSE))
#' stopifnot(identical(x, vaas_4)) # no error, but no new order
#'
#' # CSV-data based
#' copy <- sort(vaas_4) # default: by setup time
#' csv_data(vaas_4, what = "setup_time")
#' csv_data(copy, what = "setup_time")
#' stopifnot(!identical(copy, vaas_4))
#' copy <- sort(vaas_4, by = c("Position", "Setup Time"))
#' csv_data(vaas_4, what = "position")
#' csv_data(copy, what = "position")
#' stopifnot(!is.unsorted(csv_data(copy, what = "position")))
#' stopifnot(is.unsorted(csv_data(vaas_4, what = "position")))
#'
#' # making OPMS objects unique
#' dim(x <- unique(vaas_4))
#' stopifnot(identical(x, vaas_4))
#' dim(x <- unique(c(vaas_4, vaas_4)))
#' stopifnot(identical(x, vaas_4))
#' dim(x <- unique(vaas_4, what = "Species")) # species are not unique
#' stopifnot(dim(x)[1L] < dim(vaas_4)[1L])
#' dim(x <- unique(vaas_4, what = list("Species", "Strain")))
#' stopifnot(identical(x, vaas_4)) # organisms are unique
#'
#' # reverting an OPMS object
#' dim(x <- rev(vaas_4))
#' stopifnot(dim(x) == dim(vaas_4), !identical(x, vaas_4))
#' stopifnot(identical(rev(x), vaas_4))
#'
#' # repeating an OPMS object
#' dim(x <- rep(vaas_4))
#' stopifnot(identical(x, vaas_4))
#' dim(x <- rep(vaas_4, times = 2))
#' stopifnot(length(x) == length(vaas_4) * 2)
#' dim(y <- rep(vaas_4, each = 2))
#' stopifnot(length(y) == length(vaas_4) * 2, !identical(x, y))
#' stopifnot(is.null(rep(vaas_4, 0)))
#'
#' ## 'OPM' methods
#' summary(x <- sort(vaas_1))
#' stopifnot(identical(x, vaas_1))
#' dim(x <- unique(vaas_1)) # trivial
#' stopifnot(identical(x, vaas_1))
#' dim(x <- unique(vaas_1, what = list("Species", "Strain")))
#' stopifnot(identical(x, vaas_1))
#' dim(x <- rev(vaas_1)) # trivial
#' stopifnot(identical(x, vaas_1))
#' dim(x <- rep(vaas_1, 1))
#' stopifnot(identical(x, vaas_1))
#' dim(x <- rep(vaas_1, 2)) # conversion to OPMS if > 1 element
#' stopifnot(length(x) == 2, is(x, "OPMS"))
#' stopifnot(is.null(rep(vaas_4, 0)))
#'
setGeneric("sort")

setMethod("sort", c("OPM", "missing"), function(x, decreasing, ...) {
  x
}, sealed = SEALED)

setMethod("sort", c("OPM", "ANY"), function(x, decreasing, ...) {
  x
}, sealed = SEALED)

setMethod("sort", c("OPMS", "missing"), function(x, decreasing, ...) {
  sort(x = x, decreasing = FALSE, ...)
}, sealed = SEALED)

setMethod("sort", c("OPMS", "ANY"), function(x, decreasing, by = "setup_time",
    parse = by == "setup_time", exact = TRUE, strict = TRUE, na.last = TRUE) {
  if (is.list(by)) {
    keys <- lapply(X = by, FUN = metadata, object = x, exact = exact,
      strict = strict)
    if (!strict)
      if (!length(keys <- keys[!vapply(keys, is.null, NA)]))
        return(x)
  } else if (is.character(by))
    case(length(by),
      stop("if a character scalar, 'by' must not be empty"),
      {
        keys <- csv_data(x, what = by)
        if (L(parse))
          keys <- must(parse_time(keys))
        keys <- list(keys)
      },
      keys <- lapply(X = by, FUN = csv_data, object = x)
    )
  else
    stop("'by' must be a list or a character vector")
  keys <- insert(keys, decreasing = decreasing, na.last = na.last,
    .force = TRUE)
  x@plates <- x@plates[do.call(order, keys)]
  x
}, sealed = SEALED)

setMethod("sort", c("MOPMX", "missing"), function(x, decreasing, ...) {
  sort(x = x, decreasing = FALSE, ...)
}, sealed = SEALED)

setMethod("sort", c("MOPMX", "ANY"), function(x, decreasing,
    by = c("plate.type", "length"), exact = TRUE, strict = TRUE,
    na.last = TRUE, ...) {
  if (length(x) < 2L)
    return(x)
  selection <- tryCatch(match.arg(by), error = function(e) "other")
  case(selection,
    length = criterion <- vapply(x, length, 0L),
    plate.type = criterion <- plate_type(x),
    other = {
      m <- metadata(object = x, key = by, exact = exact, strict = strict)
      criterion <- sapply(m, max, na.rm = TRUE, USE.NAMES = FALSE)
    }
  )
  x[sort.list(x = criterion, decreasing = decreasing, na.last = na.last, ...)]
}, sealed = SEALED)

#= unique sort

#' @rdname sort
#' @export
#'
setGeneric("unique")

setMethod("unique", c("OPM", "ANY"), function(x, incomparables, ...) {
  x
}, sealed = SEALED)

setMethod("unique", c("OPM", "missing"), function(x, incomparables, ...) {
  x
}, sealed = SEALED)

setMethod("unique", c("OPMS", "missing"), function(x, incomparables, ...) {
  unique(x = x, incomparables = FALSE, ...)
}, sealed = SEALED)

setMethod("unique", c("OPMS", "ANY"), function(x, incomparables, ...) {
  x[!duplicated(x = x, incomparables = incomparables, ...)]
}, sealed = SEALED)

setMethod("unique", c("MOPMX", "missing"), function(x, incomparables, ...) {
  unique(x = x, incomparables = FALSE, ...)
}, sealed = SEALED)

setMethod("unique", c("MOPMX", "ANY"), function(x, incomparables, ...) {
  x[!duplicated(x = x, incomparables = incomparables, ...)]
}, sealed = SEALED)

#= rev sort

#' @rdname sort
#' @export
#'
setGeneric("rev")

setMethod("rev", "OPM", function(x) {
  x
}, sealed = SEALED)

setMethod("rev", "OPMS", function(x) {
  x@plates <- x@plates[seq.int(length(x), 1L)]
  x
}, sealed = SEALED)

#= rep sort

#' @rdname sort
#' @export
#'
setGeneric("rep")

setMethod("rep", "OPM", function(x, ...) {
  x <- rep(list(x), ...)
  case(length(x), NULL, x[[1L]], new("OPMS", plates = x))
}, sealed = SEALED)

setMethod("rep", "OPMS", function(x, ...) {
  x <- rep(x@plates, ...)
  case(length(x), NULL, x[[1L]], new("OPMS", plates = x))
}, sealed = SEALED)


################################################################################
################################################################################
#
# Extraction of character matrices
#


#' Extract aggregated values and/or metadata
#'
#' Extract selected aggregated and/or discretised values into common matrix or
#' data frame. The \code{extract} data-frame method conducts normalisation
#' and/or computes normalised point-estimates and respective confidence
#' intervals for user-defined experimental groups. It is mainly a helper
#' function for \code{\link{ci_plot}}. \code{extract_columns} extracts only
#' selected metadata entries for use as additional columns in a data frame or
#' (after joining) as character vector with labels.
#'
#' @param object \code{\link{OPMS}} object, \code{\link{MOPMX}} object or data
#'   frame, for \code{extract} with one column named as indicated by
#'   \code{split.at} (default given by \code{\link{param_names}("split.at")}),
#'   columns with factor variables before that column and columns with numeric
#'   vectors after that column. For \code{extract_columns} optionally an
#'   \code{\link{OPM}} object.
#' @param as.labels List, character vector or formula indicating the metadata to
#'   be joined and used as row names (if \code{dataframe} is \code{FALSE}) or
#'   additional columns (if otherwise). Ignored if \code{NULL}.
#'
#'   If a \code{as.labels} is a formula and \code{dataframe} is \code{TRUE}, the
#'   pseudo-function \code{J} within the formula can be used to trigger
#'   combination of factors immediately after selecting them as data-frame
#'   columns, much like \code{as.groups}.
#'
#' @param subset Character vector. The parameter(s) to put in the matrix. One of
#'   the values of \code{\link{param_names}()}. Alternatively, if it is
#'   \code{\link{param_names}("disc.name")}, discretised data are returned, and
#'   \code{ci} is ignored..
#' @param ci Logical scalar. Also return the confidence intervals?
#' @param trim Character scalar. See \code{\link{aggregated}} for details.
#' @param dataframe Logical scalar. Return data frame or matrix?
#'
#' @param as.groups For the \code{\link{OPMS}} method, a list, character vector
#'   or formula indicating the metadata to be joined and either used as
#'   \sQuote{row.groups} attribute of the output matrix or as additional columns
#'   of the output data frame. See \code{\link{heat_map}} for its usage. Ignored
#'   if empty.
#'
#'   If a \code{as.groups} is a formula and \code{dataframe} is \code{TRUE}, the
#'   pseudo-function \code{J} within the formula can be used to trigger
#'   combination of factors immediately after selecting them as data-frame
#'   columns, much like \code{as.labels}.
#'
#'   If \code{as.groups} is a logical scalar, \code{TRUE} yields a trivial group
#'   that contains all elements, \code{FALSE} yields one group per element, and
#'   \code{NA} yields an error. The column name in which this factor is placed
#'   if \code{dataframe} is \code{TRUE} is determined using
#'   \code{opm_opt("group.name")}.
#'
#'   For the data-frame method, a logical, character or numeric vector
#'   indicating according to which columns (before the \code{split.at} column)
#'   the data should be aggregated by calculating means and confidence
#'   intervals. If \code{FALSE}, such an aggregation does not take place. If
#'   \code{TRUE}, all those columns are used for grouping.
#' @param sep Character scalar. Used as separator between the distinct metadata
#'   entries if these are to be pasted together. \code{extract_columns} ignores
#'   this unless \code{join} is \code{TRUE}. The data-frame method always joins
#'   the data unless \code{what} is a list.
#' @param dups Character scalar specifying what to do in the case of duplicate
#'   labels: either \sQuote{warn}, \sQuote{error} or \sQuote{ignore}. Ignored
#'   unless \code{join} is \code{TRUE} and if \code{object} is an
#'   \code{\link{OPM}} object. For the data-frame method of \code{extract}, a
#'   character scalar defining the action to conduct if \code{as.groups}
#'   contains duplicates.
#'
#' @param exact Logical scalar. Passed to \code{\link{metadata}}.
#' @param strict Logical scalar. Also passed to \code{\link{metadata}}.
#'
#' @param full Logical scalar indicating whether full substrate names shall be
#'   used. This is passed to \code{\link{wells}}, but in contrast to what
#'   \code{\link{flatten}} is doing the argument here refers to the generation
#'   of the column names.
#' @param max Numeric scalar. Passed to \code{\link{wells}}.
#' @param ... Optional other arguments passed to \code{\link{wells}}.
#'
#' @param norm.per Character scalar indicating the presence and direction of a
#'   normalisation step.
#'   \describe{
#'   \item{none}{No normalisation.}
#'   \item{row}{Normalisation per row. By default, this would subtract the mean
#'     of each plate from each of its values (over all wells of that plate).}
#'   \item{column}{Normalisation per column. By default, this would subtract the
#'     mean of each well from each of its values (over all plates in which this
#'     well is present).}
#'   }
#'   This step can further by modified by the next three arguments.
#' @param norm.by Vector indicating which wells (columns) or plates (rows) are
#'   used to calculate means used for the normalisation. By default, the mean is
#'   calculated over all rows or columns if normalisation is requested using
#'   \code{norm.per}. But if \code{direct} is \code{TRUE}, \code{norm.by} is
#'   directly interpreted as numeric vector used for normalisation.
#' @param direct Logical scalar. For \code{extract}, indicating how to use
#'   \code{norm.by}. See there for details. For \code{extract_columns},
#'   indicating whether to extract column names directly, or search for columns
#'   of one to several given classes.
#'
#' @param subtract Logical scalar indicating whether normalisation (if any) is
#'   done by subtracting or dividing.
#' @param split.at Character vector defining alternative names of the column at
#'   which the data frame shall be divided. Exactly one must match.
#'
#' @param what For the \code{\link{OPMS}} method, a list of metadata keys to
#'   consider, or single such key; passed to \code{\link{metadata}}. A formula
#'   is also possible; see there for details. A peculiarity of
#'   \code{extract_columns} is that including \code{J} as a pseudo-function call
#'   in the formula triggers the combination of metadata entries to new factors
#'   immediately after selecting them, as long as \code{join} is \code{FALSE}.
#'
#'   For the data-frame method, just the names of the columns to extract, or
#'   their indexes, as vector, if \code{direct} is \code{TRUE}. Alternatively,
#'   the name of the class to extract from the data frame to form the matrix
#'   values.
#'
#'   In the \sQuote{direct} mode, \code{what} can also be a named list of
#'   vectors used for indexing. In that case a data frame is returned that
#'   contains the columns from \code{object} together with new columns that
#'   result from pasting the selected columns together. If \code{what} is
#'   named, its names are used as the new column names. Otherwise each name
#'   is created by joining the respective value within \code{what} with the
#'   \code{"comb.key.join"} entry of \code{\link{opm_opt}} as separator.
#' @param join Logical scalar. Join each row together to yield a character
#'   vector? Otherwise it is just attempted to construct a data frame.
#' @param factors Logical scalar determining whether strings should be converted
#'   to factors. Note that this would only affect newly created data-frame
#'   columns.
#'
#' @export
#' @return Numeric matrix or data frame from \code{extract}; always a data frame
#'   for the data-frame method with the same column structure as \code{object}
#'   and, if grouping was used, a triplet structure of the rows, as indicated in
#'   the new \code{split.at} column: (i) group mean, (ii) lower and (iii) upper
#'   boundary of the group confidence interval. The data could then be
#'   visualised using \code{\link{ci_plot}}. See the examples.
#'
#'   For the \code{OPMS} method of \code{extract_columns}, a data frame or
#'   character vector, depending on the \code{join} argument. The data-frame
#'   method of \code{extract_columns} returns a character vector or a data
#'   frame, too, but depending on the \code{what} argument.
#'
#' @details \code{extract_columns} is not normally directly called by an
#'   \pkg{opm} user because \code{extract} is available, which uses this
#'   function, but can be used for testing the applied metadata selections
#'   beforehand.
#'
#'   The \code{extract_columns} data-frame method is partially trivial (extract
#'   the selected columns and join them to form a character vector or new
#'   data-frame columns), partially more useful (extract columns with data of a
#'   specified class).
#'
#'   Not all \code{\link{MOPMX}} objects are suitable for \code{extract}. The
#'   call will be successful if only \code{\link{OPMS}} objects are contained,
#'   i.e. \code{\link{OPM}} objects are forbidden. But even if successful it
#'   might result in \code{NA} values within the resulting matrix or data frame.
#'   This may cause methods that call \code{extract} to fail. \code{NA} values
#'   will not occur if the set of row names created using \code{as.labels} is
#'   equal between the distinct elements of \code{object}. The also holds if
#'   \code{dataframe} is \code{TRUE}, even though in that case row names are
#'   only temporarily created.
#'
#'   Duplicate combinations of row and columns names currently cause the
#'   \code{\link{MOPMX}} methods to skip all of them except the last one if
#'   \code{dataframe} is \code{FALSE}. This should mainly effect substrates that
#'   occur in plates of distinct plate types.
#'
#'   Similarly, duplicate row names will cause the skipping of all but the
#'   last one. This can be circumvented by using an \code{as.labels} argument
#'   that yields unique row names. If \code{as.labels} is empty, the
#'   \code{\link{MOPMX}} method of \code{extract} will create potentially unique
#'   row names from the names if these are present but from the plate types if
#'   the \sQuote{names} attribute is \code{NULL}. This will not be done, and
#'   rows will neither be skipped nor reordered, if \code{dataframe} is
#'   \code{TRUE}.
#'
#'   Otherwise row names and names of substrate columns will be reordered
#'   (sorted). The created \sQuote{row.groups} attribute, if any, will be
#'   adapted accordingly. If \code{dataframe} is \code{TRUE}, the placement of
#'   the columns created by \code{as.groups} will also be as usual, but
#'   duplicates, if any, will be removed.
#'
#' @family conversion-functions
#' @author Lea A.I. Vaas, Markus Goeker
#' @seealso \code{\link{aggregated}} for the extraction of aggregated values
#'   from a single \code{OPMA} objects.
#'
#'   boot::norm
#'   base::data.frame base::as.data.frame base::matrix base::as.matrix
#'   base::cbind
#' @keywords manip dplot htest
#' @examples
#'
#' ## 'OPMS' method
#' opm_opt("curve.param") # default parameter
#'
#' # generate matrix (containing the parameter given above)
#' (x <- extract(vaas_4, as.labels = list("Species", "Strain")))[, 1:3]
#' stopifnot(is.matrix(x), dim(x) == c(4, 96), is.numeric(x))
#' # using a formula also works
#' (y <- extract(vaas_4, as.labels = ~ Species + Strain))[, 1:3]
#' stopifnot(identical(x, y))
#'
#' # generate data frame
#' (x <- extract(vaas_4, as.labels = list("Species", "Strain"),
#'   dataframe = TRUE))[, 1:3]
#' stopifnot(is.data.frame(x), dim(x) == c(4, 99))
#' # using a formula
#' (y <- extract(vaas_4, as.labels = ~ Species + Strain,
#'   dataframe = TRUE))[, 1:3]
#' stopifnot(identical(x, y))
#' # using a formula, with joining into new columns
#' (y <- extract(vaas_4, as.labels = ~ J(Species + Strain),
#'   dataframe = TRUE))[, 1:3]
#' stopifnot(identical(x, y[, -3]))
#'
#' # put all parameters in a single data frame
#' x <- lapply(param_names(), function(name) extract(vaas_4, subset = name,
#'   as.labels = list("Species", "Strain"), dataframe = TRUE))
#' x <- do.call(rbind, x)
#'
#' # get discretised data
#' (x <- extract(vaas_4, subset = param_names("disc.name"),
#'   as.labels = list("Strain")))[, 1:3]
#' stopifnot(is.matrix(x), identical(dim(x), c(4L, 96L)), is.logical(x))
#'
#' ## data-frame method
#'
#' # extract data from OPMS-object as primary data frame
#' # second call to extract() then applied to this one
#' (x <- extract(vaas_4, as.labels = list("Species", "Strain"),
#'   dataframe = TRUE))[, 1:3]
#'
#' # no normalisation, but grouping for 'Species'
#' y <- extract(x, as.groups = "Species", norm.per = "none")
#' # plotting using ci_plot()
#' ci_plot(y[, c(1:6, 12)], legend.field = NULL, x = 350, y = 1)
#'
#' # normalisation by plate means
#' y <- extract(x, as.groups = "Species", norm.per = "row")
#' # plotting using ci_plot()
#' ci_plot(y[, c(1:6, 12)], legend.field = NULL, x = 130, y = 1)
#'
#' # normalisation by well means
#' y <- extract(x, as.groups = "Species", norm.per = "column")
#' # plotting using ci_plot()
#' ci_plot(y[, c(1:6, 12)], legend.field = NULL, x = 20, y = 1)
#'
#' # normalisation by subtraction of the well means of well A10 only
#' y <- extract(x, as.groups = "Species", norm.per = "row", norm.by = 10,
#'   subtract = TRUE)
#' # plotting using ci_plot()
#' ci_plot(y[, c(1:6, 12)], legend.field = NULL, x = 0, y = 0)
#'
#' ## extract_columns()
#'
#' # 'OPMS' method
#'
#' # Create data frame
#' (x <- extract_columns(vaas_4, what = list("Species", "Strain")))
#' stopifnot(is.data.frame(x), dim(x) == c(4, 2))
#' (y <- extract_columns(vaas_4, what = ~ Species + Strain))
#' stopifnot(identical(x, y)) # same result using a formula
#' (y <- extract_columns(vaas_4, what = ~ J(Species + Strain)))
#' stopifnot(is.data.frame(y), dim(y) == c(4, 3)) # additional column created
#' stopifnot(identical(x, y[, -3]))
#' (x <- extract_columns(vaas_4, what = TRUE)) # use logical scalar
#' stopifnot(is.data.frame(x), dim(x) == c(4, 1))
#' (y <- extract_columns(vaas_4, what = FALSE))
#' stopifnot(is.data.frame(y), dim(y) == c(4, 1), !all(y[, 1] == x[, 1]))
#'
#' # Create a character vector
#' (x <- extract_columns(vaas_4, what = list("Species", "Strain"), join = TRUE))
#' stopifnot(is.character(x), length(x) == 4L)
#' (x <- try(extract_columns(vaas_4, what = list("Species"), join = TRUE,
#'   dups = "error"), silent = TRUE)) # duplicates yield error
#' stopifnot(inherits(x, "try-error"))
#' (x <- try(extract_columns(vaas_4, what = list("Species"), join = TRUE,
#'   dups = "warn"), silent = TRUE)) # duplicates yield warning only
#' stopifnot(is.character(x), length(x) == 4L)
#'
#' # data-frame method, 'direct' running mode
#' x <- data.frame(a = 1:26, b = letters, c = LETTERS)
#' (y <- extract_columns(x, I(c("a", "b")), sep = "-"))
#' stopifnot(grepl("^\\s*\\d+-[a-z]$", y)) # pasted columns 'a' and 'b'
#'
#' # data-frame method, using class name
#' (y <- extract_columns(x, as.labels = "b", what = "integer", as.groups = "c"))
#' stopifnot(is.matrix(y), dim(y) == c(26, 1), rownames(y) == x$b)
#' stopifnot(identical(attr(y, "row.groups"), x$c))
#'
setGeneric("extract", function(object, ...) standardGeneric("extract"))

setMethod("extract", "MOPMX", function(object, as.labels,
    subset = opm_opt("curve.param"), ci = FALSE, trim = "full",
    dataframe = FALSE, as.groups = NULL, ...) {

  convert_row_groups <- function(x) { # for generated matrices only
    result <- unlist(lapply(x, rownames), FALSE, FALSE)
    result <- sort.int(unique.default(result))
    result <- structure(character(length(result)), names = result)
    for (mat in x) # last one wins, as in collect()
      result[rownames(mat)] <- as.character(attr(mat, "row.groups"))
    as.factor(unname(result))
  }

  protected <- function(x) x[seq_len(match(RESERVED_NAMES[["parameter"]], x))]

  group_columns <- function(x, other) { # for generated data frames only
    x <- metadata_key(x)
    setdiff(c(unlist(x, FALSE, FALSE), names(attr(x, "combine"))), other)
  }

  x <- lapply(X = object, FUN = extract, as.labels = as.labels,
    subset = subset, ci = ci, trim = trim, dataframe = dataframe,
    as.groups = as.groups, ...)

  if (!dataframe) {
    if (!length(as.labels)) { # create potentially unique row names
      if (is.null(base <- names(object)))
        base <- plate_type(object)
      for (i in seq_along(x))
        rownames(x[[i]]) <- paste(base[[i]], seq_len(nrow(x[[i]])), sep = ".")
    }
    return(structure(collect(x, "datasets"), row.groups = if (length(as.groups))
        convert_row_groups(x)
      else
        NULL))
  }

  x <- collect_rows(x)
  rownames(x) <- NULL
  if (!length(as.groups))
    return(x)
  p.col <- protected(colnames(x))
  g.col <- group_columns(as.groups, p.col)
  x[, c(p.col, setdiff(colnames(x), c(p.col, g.col)), g.col), drop = FALSE]

}, sealed = SEALED)

setMethod("extract", "OPMS", function(object, as.labels,
    subset = opm_opt("curve.param"), ci = FALSE, trim = "full",
    dataframe = FALSE, as.groups = NULL, sep = " ", dups = "warn",
    exact = TRUE, strict = TRUE, full = TRUE, max = 10000L, ...) {

  do_extract <- function(what, join, dups = "ignore") {
    extract_columns(object, what = what, join = join, sep = sep, dups = dups,
      exact = exact, strict = strict)
  }
  create_groups <- function(x, join, ci) {
    result <- do_extract(x, join)
    if (join) {
      result <- as.factor(result)
      if (ci)
        result <- rep(result, each = 3L)
    } else if (ci)
      result <- result[rep(seq_len(nrow(result)), each = 3L), , drop = FALSE]
    result
  }

  # Collect parameters in a matrix
  subset <- match.arg(subset,
    unlist(map_param_names(plain = TRUE, disc = TRUE)))
  if (subset == DISC_PARAM) {
    ci <- FALSE
    result <- discretized(object, full = full, max = max, ...)
  } else {
    result <- do.call(rbind, lapply(object@plates, FUN = aggregated,
      subset = subset, ci = ci, trim = trim, full = full, max = max, ...))
  }

  if (dataframe) {

    result <- as.data.frame(result)
    if (length(as.labels)) {
      columns <- do_extract(as.labels, join = FALSE)
      if (ci)
        columns <- columns[rep(seq_len(nrow(columns)), each = 3L), ,
          drop = FALSE]
      columns <- cbind(columns, rownames(result))
      colnames(columns)[ncol(columns)] <- RESERVED_NAMES[["parameter"]]
      rownames(result) <- rownames(columns) # otherwise a warning is likely
      result <- cbind(columns, result)
    } else {
      params <- rownames(result)
      rownames(result) <- seq_len(nrow(result))
      result <- cbind(params, result)
      colnames(result)[1L] <- RESERVED_NAMES[["parameter"]]
    }
    if (length(as.groups))
      result <- cbind(result, create_groups(as.groups, FALSE, ci))

  } else {

    if (length(as.labels)) {
      labels <- do_extract(as.labels, join = TRUE, dups = dups)
      rownames(result) <- if (ci)
        paste(rep(labels, each = 3L), rownames(result))
      else
        labels
    } else {
      rownames(result) <- if (ci)
        paste(rownames(result), rep(seq_len(nrow(result) / 3L), each = 3L),
          sep = sep)
      else
        seq_len(nrow(result))
    }
    if (length(as.groups))
      attr(result, "row.groups") <- create_groups(as.groups, TRUE, ci)
  }

  result

}, sealed = SEALED)

setMethod("extract", "data.frame", function(object, as.groups = TRUE,
    norm.per = c("row", "column", "none"), norm.by = TRUE, subtract = TRUE,
    direct = inherits(norm.by, "AsIs"), dups = c("warn", "error", "ignore"),
    split.at = param_names("split.at")) {

  do_norm <- function(x, row, by, direct, subtract) sweep(x, 2L - row,
    if (direct)
      by
    else if (row)
      rowMeans(x[, by, drop = FALSE])
    else
      colMeans(x[by, , drop = FALSE]), if (subtract)
      "-"
    else
      "/"
  )

  LL(subtract, direct)
  param.pos <- assert_splittable_matrix(object, split.at)

  num.pos <- seq.int(param.pos + 1L, ncol(object))
  case(match.arg(norm.per), # compute the normalisation if requested
    none = NULL,
    row = object[, num.pos] <- do_norm(object[, num.pos, drop = FALSE],
      TRUE, norm.by, direct, subtract),
    column = object[, num.pos] <- do_norm(object[, num.pos, drop = FALSE],
      FALSE, norm.by, direct, subtract)
  )

  if (!length(as.groups) || identical(c(as.groups), FALSE))
    return(object)

  # make list or vector from the grouping columns and note its length
  # metadata_key() enables lists to be passed as used for selecting metadata
  as.groups <- metadata_key(as.groups, FALSE)
  if (!is.logical(as.groups) && anyDuplicated(as.groups))
    case(match.arg(dups), ignore = as.null, warn = warning, error = stop)(
      "duplicated grouping values")
  as.groups <- unclass(object[, seq_len(param.pos - 1L), drop = FALSE][,
    as.groups, drop = FALSE])
  gl <- length(as.groups)

  # compute the means and CIs with respect to the stated grouping
  aggr.mean <- aggregate(object[, num.pos, drop = FALSE], by = as.groups,
    FUN = mean)
  aggr.CI <- aggregate(object[, num.pos, drop = FALSE], by = as.groups,
    FUN = var) # first the variances

  # The output has to be organized in a certain structure, three rows per group:
  # first the mean, second the lower CI limit third the upper CI limit. This
  # step creates the factor-data part up to the parameter column.
  result <- as.data.frame(sapply(aggr.mean[, seq_len(gl), drop = FALSE],
    rep, each = 3L))
  colnames(result) <- names(as.groups)
  result[, RESERVED_NAMES[["parameter"]]] <- as.factor(unlist(map_param_names(
    subset = as.character(object[1L, param.pos]), ci = TRUE)))

  # Reduce to numeric part and get CIs from means and variances.
  aggr.mean <- as.matrix(aggr.mean[, seq.int(gl + 1L, ncol(aggr.mean)),
    drop = FALSE])
  aggr.CI <- norm.ci(t0 = aggr.mean,
    var.t0 = aggr.CI[, seq.int(gl + 1L, ncol(aggr.CI)), drop = FALSE])
  aggr.CI <- as.matrix(aggr.CI[, -1L, drop = FALSE]) # remove the 'conf' column

  # Prepare the numerical part of the results.
  output <- matrix(ncol = 3L * nrow(aggr.mean), nrow = ncol(aggr.mean))
  pos.1 <- ncol(aggr.CI)
  pos.2 <- seq.int(pos.1 / 2L + 1L, pos.1)
  pos.1 <- seq_len(pos.1 / 2L)
  for (i in seq_len(nrow(aggr.mean)))
    output[, seq.int(i * 3L - 2L, 3L * i)] <- c(aggr.mean[i, , drop = TRUE],
      aggr.CI[i, pos.1, drop = TRUE], aggr.CI[i, pos.2, drop = TRUE])
  output <- t(output)
  colnames(output) <- colnames(aggr.mean)

  # Done.
  cbind(result, output)
}, sealed = SEALED)

#= extract_columns extract

#' @rdname extract
#' @export
#'
setGeneric("extract_columns",
  function(object, ...) standardGeneric("extract_columns"))

setMethod("extract_columns", "WMD", function(object, what, join = FALSE,
    sep = " ", dups = c("warn", "error", "ignore"), factors = TRUE,
    exact = TRUE, strict = TRUE) {
  what <- metadata_key(what, FALSE, NULL)
  if (is.logical(what)) {
    result <- 1L
    if (!L(join)) {
      result <- as.data.frame(result)
      colnames(result) <- get("group.name", OPM_OPTIONS)
    }
    return(result)
  }
  result <- metadata(object, what, exact, strict)
  result <- if (is.list(result))
    rapply(result, as.character)
  else
    as.character(result)
  if (L(join)) {
    result <- paste0(result, collapse = sep)
  } else {
    result <- as.list(result)
    if (is.null(names(result)))
      names(result) <- paste0(what, collapse = get("key.join", OPM_OPTIONS))
    result <- as.data.frame(result, optional = TRUE, stringsAsFactors = factors)
    if (ncol(result) > length(colnames(result)))
      colnames(result) <- paste0(what, collapse = get("key.join", OPM_OPTIONS))
    if (is.list(attr(what, "combine")))
      result <- extract_columns(result, attr(what, "combine"),
        factors = factors, direct = TRUE)
  }
  result
}, sealed = SEALED)

setMethod("extract_columns", "WMDS", function(object, what, join = FALSE,
    sep = " ", dups = c("warn", "error", "ignore"), factors = TRUE,
    exact = TRUE, strict = TRUE) {
  what <- metadata_key(what, FALSE, NULL)
  if (is.logical(what)) {
    result <- if (L(what))
        rep.int(1L, length(object))
      else
        seq_len(length(object))
    if (!L(join)) {
      result <- as.data.frame(result)
      colnames(result) <- get("group.name", OPM_OPTIONS)
    }
    return(result)
  }
  result <- metadata(object, what, exact, strict)
  result <- if (is.list(result))
    lapply(result, rapply, f = as.character)
  else
    as.list(as.character(result))
  if (L(join)) {
    result <- unlist(lapply(result, FUN = paste0, collapse = sep))
    msg <- if (is.dup <- anyDuplicated(result))
      paste("duplicated label:", result[is.dup])
    else
      NULL
    if (length(msg))
      case(match.arg(dups), ignore = as.null, warn = warning, error = stop)(msg)
  } else {
    result <- must(do.call(rbind, result))
    result <- as.data.frame(result, optional = TRUE, stringsAsFactors = factors)
    if (ncol(result) > length(colnames(result)))
      colnames(result) <- paste0(what, collapse = get("key.join", OPM_OPTIONS))
    if (is.list(attr(what, "combine")))
      result <- extract_columns(result, attr(what, "combine"),
        factors = factors, direct = TRUE)
  }
  result
}, sealed = SEALED)

setMethod("extract_columns", "data.frame", function(object, what,
    as.labels = NULL, as.groups = NULL, sep = opm_opt("comb.value.join"),
    factors = is.list(what), direct = is.list(what) || inherits(what, "AsIs")) {
  join <- function(x, what, sep)
    do.call(paste, c(x[, what, drop = FALSE], list(sep = sep)))
  find_stuff <- function(x, what) {
    x <- x[, vapply(x, inherits, NA, what), drop = FALSE]
    if (!ncol(x))
      stop("no data of class(es) ", paste0(what, collapse = "/"), " found")
    as.matrix(x)
  }
  LL(direct, factors)
  if (direct) {
    if (is.list(what)) {
      if (is.null(names(what)))
        names(what) <- vapply(what, paste0, "",
          collapse = get("comb.key.join", OPM_OPTIONS))
      result <- object
      what <- what[!match(names(what), colnames(result), 0L)]
      if (factors)
        for (i in seq_along(what))
          result[, names(what)[i]] <- as.factor(join(object, what[[i]], sep))
      else
        for (i in seq_along(what))
          result[, names(what)[i]] <- join(object, what[[i]], sep)
      if (length(as.labels))
        rownames(result) <- join(object, as.labels, sep)
      attr(result, "joined.columns") <- c(attr(result, "joined.columns"), what)
    } else {
      result <- join(object, what, sep)
      if (length(as.labels))
        names(result) <- join(object, as.labels, sep)
      if (factors)
        result <- as.factor(result)
    }
  } else {
    result <- find_stuff(object, what)
    if (length(as.labels))
      rownames(result) <- join(object, as.labels, sep)
  }
  if (length(as.groups))
    attr(result, "row.groups") <- as.factor(join(object, as.groups, sep))
  result
}, sealed = SEALED)


################################################################################


#' Create data frame
#'
#' These \code{as.data.frame} methods create a data frame from aggregated and
#' discretised values in a manner distinct from \code{\link{extract}}.
#' \code{flatten} converts into a \sQuote{flat} data frame, including all
#' measurements in a single column (suitable, e.g., for \pkg{lattice}).
#'
#' @param x Object of class \code{\link{OPM}}, its child classes, or
#'   \code{\link{OPMS}} or \code{\link{MOPMX}}. If an \code{\link{OPMS}} object,
#'   for the \code{as.data.frame} method its elements must either all be
#'   \code{\link{OPM}} or all be \code{\link{OPMA}} or all be \code{\link{OPMD}}
#'   objects. If a \code{\link{MOPMX}} object, its elements must be conforming
#'   \code{\link{OPMS}} or either \code{\link{OPM}}, \code{\link{OPMA}} or
#'   \code{\link{OPMS}}  objects.
#'
#'   There are \code{as.data.frame} methods for some of the objects created by
#'   \code{\link{substrate_info}}, too.
#' @param row.names Optional vector for use as row names of the resulting data
#'   frame. Here, it is not recommended to try to set row names explicitly.
#' @param optional Logical scalar passed to the list and matrix methods of
#'   \code{as.data.frame}.
#' @param sep Character scalar used as word separator in column names. Set this
#'   to \code{NULL} or an empty vector to turn off character replacement in
#'   column names.
#' @param csv.data Logical scalar indicating whether the \code{\link{csv_data}}
#'   entries that identify the plate shall be included.
#' @param settings Logical scalar indicating whether the
#'   \code{\link{aggr_settings}} and \code{\link{disc_settings}} entries, if
#'   available, shall be included.
#' @param stringsAsFactors Logical scalar passed to the list and matrix methods
#'   of \code{as.data.frame}.
#'
#' @param object \code{\link{OPM}} or \code{\link{OPMS}} object (or list).
#' @param include For \code{flatten}, either \code{NULL}, character vector, list
#'   or formula. If not empty, include this meta-information in the data frame,
#'   replicated in each row. Otherwise it converted to a list and passed to
#'   \code{\link{metadata}}. See there for details.
#'
#'   For \code{as.data.frame}, if empty or \code{FALSE}, ignored. If
#'   \code{TRUE}, all metadata are included using \code{\link{to_metadata}}. If
#'   otherwise and non-empty, metadata selected using
#'   \code{\link{extract_columns}} are included.
#' @param fixed \code{NULL} or list. If not \code{NULL}, include these items in
#'   the data frame, replicated in each row.
#' @param factors Logical scalar. See the \code{stringsAsFactors} argument of
#'   \code{data.frame} and \code{as.data.frame} from the \pkg{base} package.
#' @param exact Logical scalar. Passed to \code{\link{metadata}}.
#' @param strict Logical scalar. Passed to \code{\link{metadata}}.
#' @param full Logical scalar. Replace well coordinates by full names?
#' @param numbers Logical scalar. Use numbers instead of well names? This is
#'   \emph{not} recommended for must usages.
#' @param ... Optional other arguments passed to \code{\link{wells}}, or from
#'   the \code{\link{OPMS}} to the \code{\link{OPM}} method, or to the list and
#'   matrix methods of \code{as.data.frame}.
#'
#' @return The \code{as.data.frame} methods create a data frame with one row
#'   for each combination of well and plate.
#'
#'   The \code{flatten} methods create a data frame with one row for each
#'   combination of time point, well and plate.
#'
#' @details The \code{as.data.frame} methods for \code{\link{OPMX}} objects
#'   are mainly intended to produce objects that can easily be written to
#'   \acronym{CSV} files, for instance using \code{write.table} from the
#'   \pkg{utils} package. There are no \pkg{opm} methods other than
#'   \code{\link{batch_opm}} (which can write such files) that make use of the
#'   created kind of objects. In particular, they cannot be input again into
#'   \pkg{opm}.
#'
#'   The following entries are contained in the generated data frame:
#'   \itemize{
#'
#'   \item Optionally the \code{\link{csv_data}} entries that identify the
#'   plate.
#'
#'   \item The names of the wells. Always included.
#'
#'   \item For \code{\link{OPMA}} objects (and \code{\link{OPMS}} objects that
#'   contain them as well as \code{\link{MOPMX}} objects that contain such
#'   \code{\link{OPMA}} or \code{\link{OPMS}} objects), always the aggregated
#'   data (curve parameters), one column for each point estimate, upper and
#'   lower confidence interval of each parameter.
#'
#'   \item For \code{\link{OPMA}} objects (and \code{\link{OPMS}} objects that
#'   contain them as well as \code{\link{MOPMX}} objects that contain such
#'   \code{\link{OPMA}} or \code{\link{OPMS}} objects), optionally the used
#'   aggregation settings, one column per entry, except for the \sQuote{options}
#'   entry (which is not a scalar). The column names are prefixed with
#'   \code{"Aggr"} followed by \code{sep}. If \code{sep} is empty,
#'   \code{\link{opm_opt}("comb.key.join")} is used.
#'
#'   \item For \code{\link{OPMD}} objects (and \code{\link{OPMS}} objects that
#'   contain them as well as \code{\link{MOPMX}} objects that contain such
#'   \code{\link{OPMD}} or \code{\link{OPMS}} objects), always one column with
#'   the discretised data.
#'
#'   \item For \code{\link{OPMD}} objects (and \code{\link{OPMS}} objects that
#'   contain them as well as \code{\link{MOPMX}} objects that contain such
#'   \code{\link{OPMD}} or \code{\link{OPMS}} objects), optionally the used
#'   discretisation settings, one column per entry, except for the
#'   \sQuote{options} entry (which is not a scalar). The column names are
#'   prefixed with \code{"Disc"} followed by \code{sep}. If \code{sep} is empty,
#'   \code{\link{opm_opt}("comb.key.join")} is used.
#'   }
#'
#'   The limits of using \acronym{CSV} as output format already show up in this
#'   list, and in general we recommend to generate \acronym{YAML} or
#'   \acronym{JSON} output instead.
#'
#'   For the \code{as.data.frame} methods of the other classes, see
#'   \code{\link{substrate_info}}.
#'
#'   In the data frame returned by \code{flatten}, column names are unchecked
#'   (not converted to variable names). The three last columns are coding for
#'   time, well and value, with the exact spelling of the column names given by
#'   \code{\link{param_names}}.
#'
#'   The \code{\link{OPMS}} method yields an additional column for the plate,
#'   the exact spelling of its name also being available via
#'   \code{\link{param_names}}. This column contains the position of each plate
#'   within \code{object}.
#'
#'   The \code{\link{MOPMX}} method yields a another additional column for the
#'   plate type. There is currently no safeguard against having several
#'   \code{\link{OPMX}} objects of the same plate type within a
#'   \code{\link{MOPMX}} object.
#' @export
#' @family conversion-functions
#' @seealso utils::write.table stats::reshape pkgutils::flatten
#' @keywords manip dplot
#' @examples
#'
#' ## OPMD method of as.data.frame()
#' summary(x <- as.data.frame(vaas_1))
#' stopifnot(is.data.frame(x), nrow(x) == 96)
#'
#' ## OPMS method of as.data.frame()
#' summary(x <- as.data.frame(vaas_4[, , 1:10]))
#' stopifnot(is.data.frame(x), nrow(x) == 10 * 4)
#'
#' ## OPM method of flatten()
#' # distinct numbers of columns due to distinct selection settings
#' head(x <- flatten(vaas_1))
#' stopifnot(is.data.frame(x), identical(dim(x), c(36864L, 3L)))
#' head(x <- flatten(vaas_1, fixed = "TEST", include = "Strain"))
#' stopifnot(is.data.frame(x), identical(dim(x), c(36864L, 5L)))
#'
#' ## OPMS method of flatten()
#' # distinct numbers of columns due to distinct selection settings
#' head(x <- flatten(vaas_4[, , 1:10]))
#' stopifnot(is.data.frame(x), identical(dim(x), c(15360L, 4L)))
#' head(x <- flatten(vaas_4[, , 1:10], fixed = "TEST", include = ~ Strain))
#' stopifnot(is.data.frame(x), identical(dim(x), c(15360L, 6L)))
#'
setGeneric("as.data.frame")

setMethod("as.data.frame", "OPM", function(x, row.names = NULL,
    optional = FALSE, sep = "_", csv.data = TRUE, settings = TRUE,
    include = FALSE, ..., stringsAsFactors = default.stringsAsFactors()) {
  result <- as.data.frame(wells(x), NULL, optional, ...,
    stringsAsFactors = stringsAsFactors)
  colnames(result) <- RESERVED_NAMES[["well"]]
  if (L(csv.data))
    result <- data.frame(as.data.frame(as.list(x@csv_data[CSV_NAMES]), NULL,
      optional, ..., stringsAsFactors = stringsAsFactors), result,
      check.names = FALSE, stringsAsFactors = FALSE)
  if (is.logical(include)) {
    if (L(include))
      result <- data.frame(result, to_metadata(x, stringsAsFactors, optional),
        check.names = FALSE, stringsAsFactors = FALSE)
  } else if (length(include)) {
    result <- data.frame(result, extract_columns(object = x, what = include,
      factors = stringsAsFactors), check.names = FALSE,
      stringsAsFactors = FALSE)
  }
  rownames(result) <- row.names
  if (length(sep))
    colnames(result) <- gsub("\\W+", sep, colnames(result), FALSE, TRUE)
  result
}, sealed = SEALED)

setMethod("as.data.frame", "OPMA", function(x, row.names = NULL,
    optional = FALSE, sep = "_", csv.data = TRUE, settings = TRUE,
    include = FALSE, ..., stringsAsFactors = default.stringsAsFactors()) {
  result <- as.data.frame(t(x@aggregated), NULL, optional, ...,
    stringsAsFactors = stringsAsFactors)
  if (length(sep))
    colnames(result) <- gsub("\\W+", sep, colnames(result), FALSE, TRUE)
  result <- data.frame(callNextMethod(x, row.names, optional, sep, csv.data,
    settings, include, ..., stringsAsFactors = stringsAsFactors), result,
    check.names = FALSE, stringsAsFactors = FALSE)
  if (L(settings)) {
    settings <- x@aggr_settings[c(SOFTWARE, VERSION, METHOD)]
    if (length(sep)) {
      names(settings) <- gsub("\\W+", sep, names(settings), FALSE, TRUE)
      names(settings) <- paste("Aggr", names(settings), sep = sep)
    } else {
      names(settings) <- paste("Aggr", names(settings),
        sep = get("comb.key.join", OPM_OPTIONS))
    }
    result <- data.frame(result, as.data.frame(settings, NULL, optional, ...,
      stringsAsFactors = stringsAsFactors), check.names = FALSE,
      stringsAsFactors = FALSE)
  }
  result
}, sealed = SEALED)

setMethod("as.data.frame", "OPMD", function(x, row.names = NULL,
    optional = FALSE, sep = "_", csv.data = TRUE, settings = TRUE,
    include = FALSE, ..., stringsAsFactors = default.stringsAsFactors()) {
  result <- callNextMethod(x, row.names, optional, sep, csv.data, settings,
    include, ..., stringsAsFactors = stringsAsFactors)
  result$Discretized <- x@discretized
  if (settings) {
    settings <- x@disc_settings[c(SOFTWARE, VERSION, METHOD)]
    if (length(sep)) {
      names(settings) <- gsub("\\W+", sep, names(settings), FALSE, TRUE)
      names(settings) <- paste("Disc", names(settings), sep = sep)
    } else
      names(settings) <- paste("Disc", names(settings),
        sep = get("comb.key.join", OPM_OPTIONS))
    result <- data.frame(result, as.data.frame(settings, NULL, optional, ...,
      stringsAsFactors = stringsAsFactors), check.names = FALSE,
      stringsAsFactors = FALSE)
  }
  result
}, sealed = SEALED)

setMethod("as.data.frame", "OPMS", function(x, row.names = NULL,
    optional = FALSE, sep = "_", csv.data = TRUE, settings = TRUE,
    include = FALSE, ..., stringsAsFactors = default.stringsAsFactors()) {
  if (!length(row.names))
    row.names <- vector("list", length(x@plates))
  do.call(rbind, mapply(as.data.frame, x = x@plates, row.names = row.names,
    MoreArgs = list(optional = optional, sep = sep, csv.data = csv.data,
      settings = settings, include = include, ...,
      stringsAsFactors = stringsAsFactors),
    SIMPLIFY = FALSE, USE.NAMES = FALSE))
}, sealed = SEALED)

setMethod("as.data.frame", "MOPMX", function(x, row.names = NULL,
    optional = FALSE, sep = "_", csv.data = TRUE, settings = TRUE,
    include = FALSE, ..., stringsAsFactors = default.stringsAsFactors()) {
  if (!length(row.names))
    row.names <- vector("list", length(x@.Data))
  do.call(rbind, mapply(as.data.frame, x = x@.Data, row.names = row.names,
    MoreArgs = list(optional = optional, sep = sep, csv.data = csv.data,
      settings = settings, include = include, ...,
      stringsAsFactors = stringsAsFactors),
    SIMPLIFY = FALSE, USE.NAMES = FALSE))
}, sealed = SEALED)

setOldClass("kegg_compounds")

setMethod("as.data.frame", "kegg_compounds", function(x, row.names = NULL,
    optional = TRUE, ..., stringsAsFactors = FALSE) {
  result <- lapply(x, as.data.frame, row.names, optional, ...,
    stringsAsFactors = stringsAsFactors)
  do.call(rbind, structure(result, names = names(x)))
}, sealed = SEALED)

setOldClass("kegg_compound")

setMethod("as.data.frame", "kegg_compound", function(x, row.names = NULL,
    optional = TRUE, ..., stringsAsFactors = FALSE) {
  # store database links for later
  links <- strsplit(as.character(x$DBLINKS), "\\s*:\\s*", FALSE, TRUE)
  links <- do.call(rbind, links)
  links <- structure(links[, 2L], names = links[, 1L])
  # get non-link components
  wanted <- c("ENTRY", "NAME", "FORMULA", "SEEALSO", "BRITE", "ACTIVITY",
    "EXACT_MASS")
  x <- structure(x[wanted], names = wanted)
  x$EXACT_MASS <- must(as.numeric(x$EXACT_MASS))
  # 'ACTIVITY' is actually only present in KEGG 'drug' descriptions
  x$ACTIVITY <- paste0(x$ACTIVITY, collapse = " ")
  x$NAME <- sub("\\s*;\\s*$", "", x$NAME, FALSE, TRUE)
  x$SEEALSO <- grep(pat <- "^Same\\s+as:\\s*", x$SEEALSO, FALSE, TRUE, TRUE)
  x$SEEALSO <- sub(pat, "", x$SEEALSO, FALSE, TRUE)
  x$SEEALSO <- gsub("\\s+", "||", x$SEEALSO, FALSE, TRUE)
  ## Note that several hierarchies may be present.
  ## Maybe we can use YAML to better represent this, either directly or after
  ## conversion to nested list.
  x$BRITE <- paste0(x$BRITE, collapse = "\n")
  x <- lapply(x, paste0, collapse = "||")
  # add database-link components
  x$CAS <- if (pos <- match("CAS", names(links), 0L))
      sub("\\s+", "||", links[[pos]], FALSE, TRUE)
    else
      NA_character_
  x$ChEBI <- if (pos <- match("ChEBI", names(links), 0L))
      sub("\\s+", "||", links[[pos]], FALSE, TRUE)
    else
      NA_character_
  # done
  x[!nzchar(x)] <- NA_character_
  as.data.frame(x, row.names, optional, ...,
    stringsAsFactors = stringsAsFactors)
}, sealed = SEALED)

#= flatten as.data.frame

#' @rdname as.data.frame
#' @export
#'
setGeneric("flatten")

setMethod("flatten", "OPM", function(object, include = NULL, fixed = list(),
    factors = TRUE, exact = TRUE, strict = TRUE, full = TRUE,
    numbers = FALSE, ...) {

  # Convert to flat data frame
  well.names <- if (L(numbers))
      seq_len(ncol(object@measurements) - 1L)
    else
      well.names <- wells(object, full = full, ...)
  ## the home-brewn solution was much faster than reshape():
  # if (factors)
  #   well.names <- as.factor(well.names)
  # result <- reshape(as.data.frame(object@measurements,
  #   stringsAsFactors = factors), direction = "long", idvar = "Hour",
  #   varying = wells(object), v.names = "Value", timevar = "Well",
  #   times = well.names)
  # colnames(result)[1L] <- "Time"
  times <- hours(object, "all")
  rep.times <- rep.int(times, length(well.names))
  rep.wells <- rep(well.names, each = length(times))
  result <- data.frame(time = rep.times, well = rep.wells,
    value = as.vector(object@measurements[, -1L]), check.names = FALSE,
    stringsAsFactors = factors)
  colnames(result) <- RESERVED_NAMES[colnames(result)]

  if (length(fixed)) # Include fixed stuff
    result <- data.frame(as.list(fixed), result, check.names = FALSE,
      stringsAsFactors = factors)

  if (length(include)) # Pick metadata and include them in the data frame
    result <- data.frame(metadata(object, include, exact = exact,
      strict = strict), result, stringsAsFactors = factors,
      check.names = FALSE)

  result

}, sealed = SEALED)

setMethod("flatten", "OPMS", function(object, include = NULL, fixed = list(),
    ...) {
  nums <- paste(RESERVED_NAMES[["plate"]], seq_along(object@plates))
  nums <- lapply(as.list(nums), `names<-`, value = RESERVED_NAMES[["plate"]])
  nums <- lapply(nums, c, fixed, recursive = FALSE)
  do.call(rbind, mapply(flatten, object = object@plates, fixed = nums,
    MoreArgs = list(include = include, ...), SIMPLIFY = FALSE))
}, sealed = SEALED)

setMethod("flatten", "MOPMX", function(object, include = NULL, fixed = list(),
    factors = FALSE, ...) {
  pt <- vapply(object@.Data, plate_type, "")
  pt <- lapply(as.list(pt), `names<-`, value = CSV_NAMES[["PLATE_TYPE"]])
  pt <- lapply(pt, c, fixed, recursive = FALSE)
  x <- mapply(flatten, object = object@.Data, fixed = pt,
    MoreArgs = list(include = include, factors = factors, ...),
    SIMPLIFY = FALSE, USE.NAMES = FALSE)
  nr <- vapply(x, ncol, 0L)
  if (any(bad <- nr < max(nr))) {
    pn <- RESERVED_NAMES[["plate"]]
    pn <- structure(list(paste(pn, 1L)), names = pn)
    for (i in seq_along(which(bad)))
      x[[i]] <- data.frame(x[[i]], pn, stringsAsFactors = factors,
        check.names = FALSE)
  }
  do.call(rbind, x)
}, sealed = SEALED)


################################################################################
################################################################################
#
# YAML functions
#


#' Convert to \acronym{YAML}
#'
#' Convert some \R object to \acronym{YAML} or \acronym{JSON}.
#'
#' @param object Object of one of the classes belonging to
#'   \code{\link{YAML_VIA_LIST}}, or \code{\link{MOPMX}} object.
#' @param sep Logical scalar. Prepend \acronym{YAML} document separator
#'   \sQuote{\verb{---}}?
#' @param line.sep Character scalar used as output line separator.
#' @param json Logical scalar. Create \acronym{JSON} instead of \acronym{YAML}?
#'   If so, \code{sep}, \code{line.sep} and \code{...} are ignored.
#' @param listify Logical scalar indicating whether after conversion to a list
#'   its non-list elements should be converted to lists if they have names.
#'   (Names of named vector are \strong{not} conserved by default in output
#'   \acronym{YAML}).
#' @param nodots Logical scalar indicating whether dots in list names should be
#'   converted to underscores. This is necessary in some situations (we met this
#'   problem when storing \acronym{JSON} documents in a document-oriented
#'   database). Converted names will additionally be marked by prepending an
#'   underscore, which assists in getting the original spelling back but is
#'   anything else than fail-safe.
#' @param ... Optional other arguments passed to \code{as.yaml} from the
#'   \pkg{yaml} package, or arguments passed between the methods.
#' @export
#' @return Character scalar (\acronym{YAML} string).
#' @family conversion-functions
#' @keywords character IO
#' @references \url{http://www.yaml.org/}
#' @references \url{http://www.json.org/}
#' @details \acronym{YAML} is a useful data-serialisation standard that is
#'   understood by many programming languages. It is particularly more human
#'   readable than \acronym{XML}, and vector-like data structures (such as
#'   phenotype microarray measurements) can be much more compactly encoded.
#'
#'   Many \acronym{PM} data sets at once can be batch-converted into
#'   \acronym{YAML} format using \code{\link{batch_opm}}. The output format for
#'   the child classes is described in detail there, as well as other aspects
#'   relevant in practice.
#'
#'   \acronym{JSON} is a subset of \acronym{YAML} and (in most cases) can also
#'   be parsed by a \acronym{YAML} parser. For generating \acronym{JSON}, the
#'   \code{toJSON} function from the \pkg{rjson} package would be used.
#'
#' @seealso yaml::as.yaml yaml::yaml.load_file json::toJSON
#'
#' @examples \dontrun{
#'
#' # Let 'x' be a any convertible object
#' # Store the data in file 'out.yml' in YAML format.
#' write(to_yaml(x), "out.yml")
#' }
#'
setGeneric("to_yaml", function(object, ...) standardGeneric("to_yaml"))

setMethod("to_yaml", "list", function(object, sep = TRUE,
    line.sep = "\n", json = FALSE, listify = nodots, nodots = FALSE, ...) {
  replace_dots <- function(x) {
    if (any(bad <- grepl(".", x, FALSE, FALSE, TRUE)))
      x[bad] <- paste0("_", chartr(".", "_", x[bad]))
    x
  }
  to_map <- function(items) if (is.null(names(items)))
    items
  else
    as.list(items)
  LL(sep, line.sep, json, listify, nodots)
  if (listify)
    object <- rapply(object, to_map, "ANY", NULL, "replace")
  if (nodots)
    object <- map_names(object, replace_dots)
  if (json) {
    result <- toJSON(object, "C")
  } else {
    result <- as.yaml(x = object, line.sep = line.sep, ...)
    if (sep)
      result <- sprintf(sprintf("---%s%%s%s", line.sep, line.sep), result)
  }
  result
}, sealed = SEALED)

setMethod("to_yaml", "YAML_VIA_LIST", function(object, ...) {
  n <- names(object)
  object <- as(object, "list")
  if (is.null(names(object)) && length(object) == length(n))
    names(object) <- n
  to_yaml(object, ...)
}, sealed = SEALED)

setMethod("to_yaml", "MOPMX", function(object, ...) {
  to_yaml(lapply(object, as, "list"), ...)
}, sealed = SEALED)


################################################################################


#' Convert user-defined objects to \acronym{OPMX}
#'
#' Convert data frames with user-defined plate types to \code{\link{OPMX}} or
#' \code{\link{MOPMX}} objects.
#'
#' @rdname opmx.function
#'
#' @param object Data frame containing numeric data with growth or respiration
#'   measurements and optional or mandatory additional columns, depending on
#'   the \code{format} argument.
#' @param format Character scalar indicating the data layout within
#'   \code{object}. See below for examples. In brief, the formats are:
#'   \describe{
#'     \item{horizontal}{One row per well, with additional columns providing the
#'     substrate names, metadata that identify the plate, and optionally other
#'     columns to be used as \code{\link{metadata}} or \code{\link{csv_data}}
#'     entries. The time points must be given in columns that can be identified
#'     with a certain prefix. The part after the prefix must be convertible to
#'     numeric data (the time points, ideally given in hours). Several plates
#'     and even several plate types can be contained within \code{object}.}
#'     \item{rectangular}{Several rows and columns per time point, yielding a
#'     set of (potentially incomplete) rectangles per plate. Empty columns are
#'     skipped; empty rows are skipped or act as separator. Only a single plate,
#'     and only measurements are contained within \code{object}, thus some of
#'     the other arguments cannot be empty. See the \code{interval} argument for
#'     setting time points and see \code{sep} for details of the format.}
#'     \item{vertical}{One column per well. Only a single plate, and only
#'     measurements and time points are contained within \code{object}, thus
#'     some of the other arguments cannot be empty. The time points are either
#'     contained as column or can be read from the row names. Ideally, they are
#'     given in hours.If \code{object} has no column names, the first column or
#'     the row names yield the time points.}
#'   }
#' @param position Character vector. In \sQuote{horizontal} format, the name of
#'   one to several columns to be joined yielding \sQuote{position} indicators.
#'   These will be used to uniquely identify each plate. The columns to be
#'   joined will be kept, too; usually they will end up in the
#'   \code{\link{metadata}}. In this case, the resulting \sQuote{position}
#'   indicators are newly generated rather than literally taken from the input,
#'   but yield the same grouping. An empty \code{position} argument is possible,
#'   but then an accordingly named column must already be present, whose content
#'   is used literally.
#'
#'   The \code{position} argument is mandatory for the \sQuote{rectangular} and
#'   \sQuote{vertical} formats. It should be chosen so as to identify the
#'   resulting \code{\link{OPM}} object again once it is combined with others
#'   into an \code{\link{OPMS}} object. (By default the setup time is
#'   additionally considered but the default for the \code{setup.time} argument
#'   is just the time of the call to \code{opmx}.)
#'
#'   For plate position values to be used literally, integers between 0 and 99
#'   (inclusively) followed by a single letters are recommended, because this
#'   allows \pkg{opm} to normalise this entry. See the eponymous argument of
#'   \code{\link{csv_data}}.
#'
#' @param plate.type Character scalar. In \sQuote{horizontal} mode, the name of
#'   the column containing the plate-type indicators. After normalisation, these
#'   will be used for storing the mapping of well coordinates. The argument is
#'   ignored if empty. But then an accordingly named column must already be
#'   present \strong{OR} \code{full.name} must contain a single named element,
#'   whose name is then inserted as plate name.
#'
#'   The \code{plate.type} argument is mandatory for the \sQuote{rectangular}
#'   and \code{vertical} formats. An according plate type must already have been
#'   stored using \code{\link{register_plate}} and contain the well coordinates
#'   found in \code{object}. Normalisation of the plate-type name is done,
#'   however.
#'
#'   In \sQuote{horizontal} mode, the plate type can be registered beforehand,
#'   too, which is useful to enforce a certain ordering of wells. But then the
#'   registered well-coordinate map must contains all well coordinates found in
#'   \code{object}.
#'
#' @param well Character scalar. In \sQuote{horizontal} format, the name of the
#'   column containing the well indicators. These should be substrate names; an
#'   according mapping from (newly assigned) well coordinates to these substrate
#'   names will then be stored using \code{\link{register_plate}} if it is not
#'   yet present. Ignored if empty (but then an accordingly named column must be
#'   present).
#' @param prefix Character scalar. In \sQuote{horizontal} format, used for
#'   identifying the measurements columns.
#' @param sep Character vector, numeric scalar, or empty. In
#'   \sQuote{rectangular} format, used for identifying the rows and column with
#'   time-points and well coordinates. If empty, \code{object} is split at rows
#'   containing only \code{NA} values or empty strings. If a positive number,
#'   \code{object} is split into section with that many rows. If a non-empty
#'   character vector, rows harbouring values contained in \code{sep} are
#'   regarded as the first row of each section.
#'
#'   Well coordinates can be missing from each rectangle unless \code{sep} is a
#'   non-empty character vector. If present, they must comprise single letters
#'   in one column and values interpretable as integers in the first row, or
#'   otherwise around. If missing, in the case of more rows than columns letters
#'   are assigned to the columns, numbers to the rows, else otherwise around.
#' @param full.name Named character vector indicating the full plate names.
#'   Ignored if empty. Names should be names of the plate types found within
#'   \code{object}, if any, but normalisation will be done. Values should be the
#'   respective full names. Missing ones are silently ignored.
#'
#'   If the plate type is not found within \code{object}, then it is taken from
#'   the name of \code{full.name}, assuming a uniform plate type throughout
#'   \code{object}. In that case, \code{full.name} must contain only a single
#'   element (and a single name).
#' @param setup.time Character scalar to be inserted if missing in the data.
#'   Like the next argument, the value goes into the \code{\link{csv_data}}.
#' @param filename Character scalar to be inserted if missing in the data.
#' @param interval Numeric vector. If of length one, indicating the time
#'   interval between measurements in the \sQuote{rectangular} format. If the
#'   length corresponds to the number of measurements per well in \code{object},
#'   it is interpreted directly as the time points. This is useful if the
#'   intervals are non-unique. Ignored if empty, causing \code{0, 1, 2, ...} to
#'   be used as time points. (This is often acceptable as it only causes a
#'   different scaling; it is not acceptable if the time points were not in
#'   regular intervals.)
#'
#'   In the case of the \code{vertical} format, a non-empty \code{interval}
#'   value causes the time points to not be extracted from \code{object} but
#'   constructed from \code{interval}. Ideally, \code{interval} is given in
#'   hours (because this corresponds to the default axis labelling of some
#'   plotting functions).
#' @param na.strings Character vector passed to \code{type.convert} from the
#'   \pkg{utils} package. Currently only relevant for the \sQuote{rectangular}
#'   format.
#' @param dec Likewise.
#' @export
#' @return \code{\link{OPMX}} or \code{\link{MOPMX}} object or \code{NULL},
#'   depending on how many distinct plate types are encountered within
#'   \code{object}.
#' @family conversion-functions
#' @keywords manip
#' @details The main purpose of this function is to convert objects that hold
#'   non-\acronym{PM} data to \code{\link{OPMX}} objects that can be analysed
#'   with \pkg{opm}. The mechanism for dealing with user-defined plate types is
#'   implemented in \code{\link{register_plate}}, whereas \code{opmx} also takes
#'   care of the necessary changes in format and naming for converting a data
#'   frame to a \code{\link{MOPMX}} object.
#'
#'   In contrast to functions for reading \acronym{PM} data, \code{opmx} allows
#'   for input \code{NA} values by removing, within each resulting pseudo-plate
#'   (\code{\link{OPM}} object) individually, all time points that contain at
#'   least one \code{NA} value. This might lose information, of course, but not
#'   between plates, even though it might yield \code{\link{OPMX}} objects with
#'   non-uniform measurement times. A warning is issued in the case of removal.
#'
#' @examples
#'
#' ## 'horizontal' input format
#'
#' # fake data frame containing growth or respiration measurements
#' x <- data.frame(
#'   Treatment = c(rep("Control", 4), rep("Heat stress", 4)),
#'   Strain = paste0("X", c(1, 2, 2, 1, 2, 1, 1, 2)),
#'   Substrate = c(rep("Glucose", 2), rep("Galactose", 4), rep("Glucose", 2)),
#'   T_0 = c(12, 5, 8, 6, 8, 9, 7, 10),
#'   T_5 = c(23, 7, 7, 18, 30, 10, 8, 9),
#'   T_10 = c(79, 9, 10, 64, 67, 8, 6, 11),
#'   T_15 = c(103, 8, 46, 99, 101, 17, 9, 8),
#'   T_20 = c(105, 9, 77, 112, 103, 44, 8, 12)
#' )
#'
#' # The plate type is not contained and thus taken from 'full.name', but
#' # the wells are obviously within 'Substrate', and each combination of
#' # 'Treatment' and 'Strain' is apparently one group of measurements
#' # (interpreted as 'plate').
#' y <- opmx(x, well = "Substrate", position = c("Treatment", "Strain"),
#'   full.name = c(sugars = "Fake sugar test plate"))
#'
#' # This yields a single OPMX object as there is only one plate type.
#' stopifnot(is(y, "OPMX"), dim(y) == c(4, 5, 2))
#' print(xy_plot(y, include = list("Strain", "Treatment"),
#'   theor.max = FALSE, main = list(in.parens = FALSE), ylab = "Hours"))
#'
#' ## 'rectangular' input format
#'
#' # Get the input file. The rectangular format is hardly suitable for R
#' # but produced by plate readers such as those distributed by the TECAN
#' # company.
#' growth.data.file <- grep("tecan", opm_files("growth"), ignore.case = TRUE,
#'   value = TRUE)
#'
#' if (length(growth.data.file)) { # if the file was found
#'
#'   x <- read.table(growth.data.file)
#'   head(x)
#'
#'   # Creating a fake well map. For really making sense of these data, one
#'   # would need to know the real substrate names.
#'   well.map <- rbind(1:6, 1:6, 1:6, 1:6)
#'   well.map[] <- paste0("Substrate ", LETTERS[1:4], well.map)
#'
#'   # Registering the plate type beforehand is mandatory here because the
#'   # file does not contain the real substrate names.
#'   register_plate(XYZ = well.map)
#'   (y <- opmx(x, "rectangular", plate.type = "XYZ", position = 1,
#'     interval = 0.25))
#'   plate_type(y) # => a custom (user-defined) plate
#'   stopifnot(setequal(wells(y, full = TRUE, in.parens = FALSE), well.map))
#'
#'   register_plate(XYZ = NULL) # tidying up
#'
#' } else {
#'   warning("file with growth data not found")
#' }
#'
#' ## 'vertical' input format
#'
#' # Fake data frame. It is safer to set all column names explicitly.
#' # If none are there, the first column yields the time points unless
#' # there are explicitly set row names.
#' x <- data.frame(
#'   c(0, 5, 10, 15, 20),
#'   c(12, 23, 79, 103, 105),
#'   c(5, 7, 9, 8, 9),
#'   c(8, 7, 10, 46, 77),
#'   c(6, 18, 64, 99, 112),
#'   c(8, 30, 67, 101, 103),
#'   c(9, 10, 8, 17, 44),
#'   c(7, 8, 6, 9, 8),
#'   c(10, 9, 11, 8, 12)
#' )
#' colnames(x) <- NULL # necessary for this example
#'
#' # Creating a fake well map for the fake data frame.
#' well.map <- paste("Substrate", 1:8)
#' names(well.map) <- paste0("A", 1:8)
#'
#' # Registering the plate type beforehand is mandatory here because the
#' # input data frame does not contain the real substrate names.
#' register_plate(XYZ = well.map)
#' wells(plate = "CUSTOM:XYZ")[1:10]
#'
#' (y <- opmx(x, "vertical", plate.type = "XYZ", position = 1))
#'
#' plate_type(y) # => a custom (user-defined) plate
#' stopifnot(setequal(wells(y, full = TRUE, in.parens = FALSE), well.map))
#'
#' register_plate(XYZ = NULL) # tidying up
#'
setGeneric("opmx", function(object, ...) standardGeneric("opmx"))

#= opmx opmx.function

setMethod("opmx", "data.frame", function(object,
    format = c("horizontal", "rectangular", "vertical"), plate.type = NULL,
    position = NULL, well = NULL, prefix = "T_", sep = object[1L, 1L],
    full.name = NULL, setup.time = date(), filename = "", interval = NULL,
    na.strings = "NA", dec = ".") {

  try_numeric <- function(x, na.strings, dec) {
    if (is.numeric(x))
      return(x)
    type.convert(x, na.strings, TRUE, dec)
  }

  convert_interval <- function(x, y) {
    if (length(x) == 1L)
      return(x * (seq_len(y) - 1L))
    if (length(x) != y)
      stop(sprintf("expected 1 or %i as length of non-empty 'interval', got %i",
        y, length(x)))
    if (!is.numeric(x))
      stop("non-empty 'interval' must be numeric")
    x
  }

  # Create a matrix acceptable as 'measurements' entry.
  #
  convert_rectangular_matrix <- function(x, sep, interval, na.strings, dec) {

    empty <- function(x) {
      if (is.character(x))
        !nzchar(x) | is.na(x)
      else
        is.na(x)
    }

    convert_time_point <- function(x, header, na.strings, dec) {
      make_coords <- function(left, right, na.strings, dec) {
        vapply(toupper(left), sprintf, character(length(right)),
          fmt = "%s%02i", try_numeric(right, na.strings, dec))
      }
      if (all(x[-1L, 1L] %in% c(LETTERS, letters))) {
        coords <- make_coords(x[-1L, 1L], unlist(x[1L, -1L], FALSE, FALSE),
          na.strings, dec)
        x <- t(as.matrix(x[-1L, -1L, drop = FALSE]))
      } else if (all(x[1L, -1L] %in% c(LETTERS, letters))) {
        coords <- make_coords(x[1L, -1L], x[-1L, 1L], na.strings, dec)
        x <- as.matrix(x[-1L, -1L, drop = FALSE])
      } else if (header) {
        stop("expected row and column names comprising letters or integers")
      } else if (nrow(x) > ncol(x)) {
        coords <- make_coords(LETTERS[seq_len(ncol(x))], seq_len(nrow(x)),
          na.strings, dec)
        x <- as.matrix(x)
      } else {
        coords <- make_coords(LETTERS[seq_len(nrow(x))], seq_len(ncol(x)),
          na.strings, dec)
        x <- t(as.matrix(x))
      }
      dim(x) <- NULL
      if (!is.numeric(x <- try_numeric(x))) {
        warning("skipping uninterpretable (non-numeric) alleged time point")
        return(NULL)
      }
      names(x) <- coords
      if (is.unsorted(names(x)))
        return(x[order(names(x))])
      x
    }

    if (any(pos <- vapply(x, function(x) all(empty(x)), NA)))
      x <- x[, !pos, drop = FALSE] # remove all-NA columns

    if (!length(sep)) {
      pos <- Reduce(`&`, lapply(x, empty)) # split at empty rows
      pos <- sections(pos, FALSE)
      header <- FALSE
    } else if (is.character(sep)) {
      pos <- logical(nrow(x))
      for (i in seq_along(x))
        if (any(pos <- x[, i] %in% sep)) {
          x <- x[, c(i, setdiff(seq_along(x), i)), drop = FALSE]
          break
        }
      if (!any(pos))
        stop("'sep' found in none of the columns")
      pos <- sections(pos, TRUE)
      header <- TRUE
    } else if (is.numeric(sep) && length(sep) == 1L && !is.na(sep) && sep > 0) {
      if (nrow(x) %% sep > 0L)
        stop("number of rows must be a multiple of any number given as 'sep'")
      pos <- factor(rep(seq.int(nrow(x) / sep), each = sep))
      header <- FALSE
    } else {
      stop("'sep' must be empty or character vector or positive number")
    }

    x <- split.data.frame(x, pos)
    x <- do.call(rbind, lapply(x, convert_time_point, header, na.strings, dec))

    if (any(bad <- apply(is.na(x), 2L, all)))
      x <- x[, !bad, drop = FALSE] # removal of all-NA columns
    if (is.integer(x))
      storage.mode(x) <- "double"

    x <- cbind(if (length(interval))
        convert_interval(interval, nrow(x))
      else
        seq_len(nrow(x)) - 1L, x)
    colnames(x)[1L] <- HOUR
    rownames(x) <- NULL
    x
  }

  # Create a matrix acceptable as 'measurements' entry.
  #
  convert_vertical_matrix <- function(x, interval, na.strings, dec) {
    select_columns <- function(x) {
      n <- clean_coords(colnames(x))
      if (any(ok <- grepl("^[A-H]\\d{2}$", n, FALSE, TRUE))) {
        colnames(x)[ok] <- n[ok]
      } else if (any(ok <- grepl("^\\d{3}$", n, FALSE, TRUE))) {
        colnames(x)[ok] <- rownames(WELL_MAP)[as.integer(colnames(x)[ok])]
      } else if (any(ok <- grepl("^V\\d{2}$", n, FALSE, TRUE))) {
        colnames(x)[ok] <- rownames(WELL_MAP)[
          as.integer(chartr("V", " ", colnames(x)[ok]))]
      } else {
        ok <- !logical(ncol(x))
        if (!length(interval) && is.integer(attr(x, "row.names")))
          ok[1L] <- FALSE # first column contains time points
        colnames(x)[ok] <- rownames(WELL_MAP)[seq_along(which(ok))]
      }
      cbind(if (length(interval))
          convert_interval(interval, nrow(x))
        else if (any(!ok))
          x[, !ok, drop = FALSE][, 1L]
        else
          rownames(x), x[, ok, drop = FALSE])
    }
    x <- try_numeric(as.matrix(select_columns(x)), na.strings, dec)
    if (is.integer(x))
      storage.mode(x) <- "double"
    else if (!is.double(x))
      stop("could not convert mesurements to numeric type")
    rownames(x) <- NULL
    colnames(x)[1L] <- HOUR
    x
  }

  # Where 'x' must be a matrix acceptable as 'measurements' entry, but
  # optionally containing NA values.
  #
  filter_times <- function(x) {
    if (!any(bad <- rowSums(is.na(x)) > 0L))
      return(x)
    warning("removing ", sum(bad), " time points that contain NAs")
    x[!bad, , drop = FALSE]
  }

  # At this stage, 'x' must be a matrix acceptable as 'measurements' entry, the
  # only exception being that rows with NAs are removed. Not used for the
  # 'horizontal' format.
  #
  create_opm_object <- function(x, position, plate.type, full.name, setup.time,
      filename) {
    L(plate.type, .msg = "plate type missing or non-unique")
    L(position, .msg = "'position' missing or non-unique")
    if (inherits(plate.type, "AsIs")) { # undocumented behaviour
      plate.type <- unclass(plate.type)
    } else {
      plate.type <- custom_plate_normalize_all(plate.type)
      custom_plate_assert(plate.type, colnames(x)[-1L])
      if (!is.na(full <- full.name[plate.type]))
        custom_plate_set_full(plate.type, full)
    }
    y <- c(L(filename), plate.type, position, L(setup.time))
    names(y) <- CSV_NAMES
    new("OPM", measurements = filter_times(x), csv_data = y, metadata = list())
  }

  # 'plate.type' and 'full.name' must already be normalized at this stage.
  #
  register_substrates <- function(wells, plate.type, full.name) {
    wn <- unique.default(wells) # already sorted at this stage
    if (all(grepl("^\\s*[A-Za-z]\\s*\\d+\\s*$", wn, FALSE, TRUE))) {
      map <- structure(clean_coords(wn), names = wn)
    } else if (custom_plate_exists(plate.type)) {
      map <- custom_plate_get(plate.type)
      if (any(bad <- !wn %in% map))
        stop("plate type '", plate.type, "' already exists but lacks ",
          "substrate '", wn[bad][1L], "'")
      map <- structure(names(map), names = map)
    } else {
      map <- structure(rownames(WELL_MAP)[seq_along(wn)], names = wn)
      custom_plate_set(plate.type, structure(names(map), names = map))
    }
    if (!is.na(full <- full.name[plate.type]))
      custom_plate_set_full(plate.type, full)
    map_values(wells, map)
  }

  # A mapping of the column names of 'x' must already have been conducted at
  # this stage.
  #
  convert_horizontal_format <- function(x, prefix, full.name, setup.time,
      filename) {
    repair_csv_data <- function(x, full.name, setup.time, filename) {
      map <- c(CSV_NAMES, RESERVED_NAMES[["well"]])
      map <- structure(map, names = chartr(" ", ".", map))
      names(x) <- map_values(names(x), map)
      n <- CSV_NAMES[["PLATE_TYPE"]]
      if (pos <- match(n, colnames(x), 0L))
        x[, pos] <- custom_plate_normalize_all(x[, pos])
      else
        x[, n] <- L(names(full.name),
          .msg = "plate type neither in 'object' nor (uniquely) in 'full.name'")
      n <- CSV_NAMES[["SETUP"]]
      if (!n %in% names(x))
        x[, n] <- setup.time
      n <- CSV_NAMES[["FILE"]]
      if (!n %in% names(x))
        x[, n] <- filename
      x
    }
    csv_positions <- function(x) {
      pos <- get("csv.selection", OPM_OPTIONS)
      pos <- unique.default(c(pos, CSV_NAMES[["PLATE_TYPE"]]))
      match(pos, colnames(x))
    }
    time_point_columns <- function(x, prefix) {
      first <- substring(x, 1L, nchar(prefix))
      x <- substring(x, nchar(prefix) + 1L, nchar(x))
      x <- suppressWarnings(as.numeric(x))
      x[first != prefix] <- NA_real_
      if (all(is.na(x)))
        stop("no columns with time points found -- wrong prefix?")
      x
    }
    per_plate_type <- function(cd, tp, x, md, full.name) {
      pos <- match(RESERVED_NAMES[["well"]], colnames(md))
      colnames(x) <- register_substrates(md[, pos],
        cd[1L, CSV_NAMES[["PLATE_TYPE"]]], full.name)
      md <- md[, -pos, drop = FALSE]
      indexes <- cd[, get("csv.keys", OPM_OPTIONS), drop = FALSE]
      indexes <- apply(indexes, 1L, paste0, collapse = " ")
      indexes <- split.default(seq_len(ncol(x)), indexes)
      result <- vector("list", length(indexes))
      for (i in seq_along(indexes)) {
        val <- x[, idx <- indexes[[i]], drop = FALSE]
        result[[i]] <- new("OPM", csv_data = cd[idx[1L], ],
          metadata = lapply(md[idx, , drop = FALSE], unique.default),
          measurements = filter_times(cbind(tp,
            val[, order(colnames(val)), drop = FALSE])))
      }
      case(length(result), NULL, result[[1L]], new("OPMS", plates = result))
    }
    traverse_plate_types <- function(cd, tp, x, md, full.name) {
      indexes <- split.default(seq_len(ncol(x)),
        cd[, CSV_NAMES[["PLATE_TYPE"]]])
      result <- vector("list", length(indexes))
      for (i in seq_along(indexes)) {
        idx <- indexes[[i]]
        result[[i]] <- per_plate_type(cd[idx, , drop = FALSE], tp,
          x[, idx, drop = FALSE], md[idx, , drop = FALSE], full.name)
      }
      names(result) <- names(indexes)
      result
    }
    x <- x[order(x[, RESERVED_NAMES[["well"]]]), , drop = FALSE]
    x <- repair_csv_data(x, full.name, setup.time, filename)
    pos <- csv_positions(x)
    cd <- as.matrix(x[, pos, drop = FALSE])
    x <- x[, -pos, drop = FALSE]
    tp <- time_point_columns(names(x), prefix)
    md <- x[, is.na(tp), drop = FALSE]
    x <- t(as.matrix(x[, !is.na(tp), drop = FALSE]))
    rownames(x) <- NULL
    tp <- matrix(tp[!is.na(tp)], nrow(x), 1L, FALSE, list(NULL, HOUR))
    result <- traverse_plate_types(cd, tp, x, md, full.name)
    case(length(result), NULL, result[[1L]], as(result, "MOPMX"))
  }

  # Only for the 'horizontal' format.
  #
  map_colnames <- function(x, plate.type, position, well) {
    to_positions <- function(x) {
      if (is.factor(x) || is.double(x))
        x <- as.integer(x)
      else if (!is.integer(x))
        x <- as.integer(as.factor(x))
      clean_plate_positions(paste(x, "A"))
    }
    map <- list()
    map[[CSV_NAMES[["PLATE_TYPE"]]]] <- plate.type
    map[[RESERVED_NAMES[["well"]]]] <- well
    if (length(map)) {
      map <- structure(names(map), names = unlist(map, TRUE, FALSE))
      names(x) <- map_values(names(x), map)
    }
    if (length(position)) {
      if (length(map))
        position <- map_values(position, map)
      wanted <- list(position)
      names(wanted) <- pos <- CSV_NAMES[["POS"]]
      x <- extract_columns(x, wanted)
      x[, pos] <- to_positions(x[, pos])
    }
    x
  }

  prepare_full_name <- function(x) {
    if (!length(x))
      return(structure(character(), names = character()))
    names(x) <- custom_plate_normalize_all(names(x))
    x
  }

  for (i in which(vapply(object, is.factor, NA)))
    object[, i] <- as.character(object[, i])

  full.name <- prepare_full_name(full.name)

  case(match.arg(format),

    horizontal = convert_horizontal_format(map_colnames(object,
      plate.type, position, well), prefix, full.name, setup.time, filename),

    rectangular = create_opm_object(convert_rectangular_matrix(object, sep,
      interval, na.strings, dec), position, plate.type, full.name,
      setup.time, filename),

    vertical = create_opm_object(convert_vertical_matrix(object, interval,
      na.strings, dec), position, plate.type, full.name, setup.time, filename)

  )
}, sealed = SEALED)


################################################################################

