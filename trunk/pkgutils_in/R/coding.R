
################################################################################


#' Modified switch function
#'
#' An altered \code{switch} statement for stricter flow control.
#'
#' @param EXPR A character or numeric scalar based on which a decision is made.
#' @param ... Additional arguments from which to select an alternative.
#' @return Selected value from \code{\dots}.
#' @details If \code{EXPR} is a character scalar, the behaviour is like
#'   the one of \code{switch} with the exception that unmatched values within
#'   \code{\dots} cause an error. If \code{EXPR} is of mode \sQuote{numeric},
#'   the behaviour is like \code{switch} but counting starts at 0 and a value
#'   larger than the number of elements within \code{\dots} selects the last
#'   element. It is an error if \code{EXPR} is negative or if \code{\dots}
#'   contains no arguments at all.
#' @export
#' @seealso base::switch
#' @family coding-functions
#' @keywords utilities
#' @examples
#'
#' # 'numeric' method
#' (x <- case(0, "a", "b", "c"))
#' stopifnot(identical(x, "a"))
#' (x <- case(99, "a", "b", "c"))
#' stopifnot(identical(x, "c"))
#'
#' # 'character' method
#' (x <- case("b", a = "x", b = "y", c = "z"))
#' stopifnot(identical(x, "y"))
#' (x <- try(case("d", a = "x", b = "y", c = "z"), silent = TRUE))
#' stopifnot(inherits(x, "try-error"))
#'
case <- function(EXPR, ...) UseMethod("case")

#' @rdname case
#' @method case numeric
#' @export
#'
case.numeric <- function(EXPR, ...) {
  stopifnot(EXPR >= 0L, nargs() > 1L)
  switch(EXPR = pmin(EXPR, nargs() - 2L) + 1L, ...)
}

#' @rdname case
#' @method case character
#' @export
#'
case.character <- function(EXPR, ...) {
  switch(EXPR = EXPR, ..., stop("unmatched 'EXPR' value"))
}


################################################################################


#' Convert warnings to errors
#'
#' Raise an error if a warning occurs. Useful for making certain tests more
#' strict. It is a bit easier to use than changing the \sQuote{warn} entry of
#' \code{options} from the \pkg{base} package (because the entry would usually
#' need to be set back).
#'
#' @param expr \R expression to evaluate.
#' @param msg Character vector to be used as error message. If empty or
#'   \code{NULL}, the \code{conditionMessage} of the issued warning is used.
#' @param ... Optional further arguments to \code{tryCatch}.
#' @param domain Passed to \code{stop} (if a warning occurs).
#' @return The result of \code{expr} (if no error occurs).
#' @export
#' @seealso base::tryCatch base::stop base::options
#' @family coding-functions
#' @keywords utilities
#' @examples
#' (x <- try(must(as.numeric(c("1", "2", "3"))), silent = TRUE))
#' stopifnot(identical(x, c(1, 2, 3)))
#' (x <- try(must(as.numeric(c("1", "x", "3"))), silent = TRUE))
#' stopifnot(inherits(x, "try-error"))
#'
must <- function(expr, msg = NULL, ..., domain = NULL) {
  # For some reason, using stop() directly results in errors that cannot be
  # catched with tryCatch() any more.
  tryCatch(expr = expr, warning = function(w) stop(if (length(msg))
    msg
  else
    conditionMessage(w), call. = FALSE, domain = domain), ...)
}


################################################################################


#' Assert a length
#'
#' Raise an error if one to several given \R objects do not have the specified
#' length. This is mainly used to easily generate meaningful error messages
#' related to function arguments.
#'
#' @param x \R object to test.
#' @param ... Any \R objects to test.
#' @param .wanted Integer scalar giving the desired length. Note that this can
#'   \strong{not} be a scalar with \sQuote{double} as \code{storage.mode}.
#' @param .msg Error message passed to \code{sprintf} with the name of \code{x}
#'   and the value of \code{wanted} as the two additional arguments.
#' @param .domain Passed to \code{stop} from the \pkg{base} package as argument
#'   \sQuote{domain}.
#' @return If successful, \code{L} returns \code{x}, but an error message is
#'   raised if \code{length(x)} is not identical to \code{wanted}. \code{LL}
#'   yields the names of the arguments contained in \code{\dots}, returned
#'   invisibly, if successful. Otherwise an error is raised.
#' @seealso base::stop
#' @export
#' @family coding-functions
#' @keywords utilities
#' @examples
#' (x <- L(letters, 26L))
#' stopifnot(identical(x, letters))
#' (x <- try(L(letters, 25L), silent = TRUE))
#' stopifnot(inherits(x, "try-error"))
#' (x <- LL(letters, LETTERS, .wanted = 26L))
#' stopifnot(x == c("letters", "LETTERS"))
#'
L <- function(x, .wanted = 1L, .msg = "need object '%s' of length %i",
    .domain = NULL) {
  if (identical(length(x), .wanted))
    return(x)
  stop(sprintf(.msg, deparse(match.call()$x), .wanted), call. = FALSE,
    domain = .domain)
}

#' @rdname L
#' @export
#'
LL <- function(..., .wanted = 1L, .msg = "need object '%s' of length %i",
    .domain = NULL) {
  arg.names <- as.character(match.call())[-1L][seq_along(items <- list(...))]
  invisible(mapply(function(item, name) {
    if (!identical(length(item), .wanted))
      stop(sprintf(.msg, name, .wanted), call. = FALSE, domain = .domain)
    name
  }, items, arg.names, SIMPLIFY = TRUE, USE.NAMES = FALSE))
}


################################################################################


#' Nicer message listings and flattening of objects
#'
#' Create some kind of listing, used, e.g., in (error) messages or warnings.
#' Alternatively, make an object \sQuote{flat}, such as by creating a non-nested
#' list from a list.
#'
#' @inheritParams pack_desc
#' @param object Usually a list. The default method just returns \code{object}
#'   if it is atomic but raises an error otherwise.
#' @param use.names Logical scalar passed to \code{unlist} from the \pkg{base}
#'   package.
#' @param x For the default method, an object convertible via \code{unclass} to
#'   one of the object classes that have explicit methods. For the
#'   character-vector method, in the default style mode, its \sQuote{names}
#'   attribute is used as the first column of the resulting listing; if it is
#'   \code{NULL} or if \code{force.numbers} is \code{TRUE}, numbers are
#'   inserted. The \sQuote{double} method is controlled by the \sQuote{digits}
#'   entry of \code{options} from the \pkg{base} package.
#' @param header \code{NULL} or character vector. Prepended to the result.
#' @param footer \code{NULL} or character vector. Appended to the result.
#' @param prepend Logical, numeric or character scalar. The two main uses are:
#'   \describe{
#'   \item{Default mode:}{The value is prepended to each line except
#'     \code{header} and \code{footer}. If numeric, the number of spaces.
#'     \code{TRUE} causes tabs to be used, \code{FALSE} turns prepending off. If
#'     in \sQuote{character} mode, used directly.}
#'   \item{If \code{style} is \sQuote{sentence}:}{ In that case, a logical
#'     scalar decides about whether names are prepended before joining the
#'     vector elements. A character scalar is used as template for
#'     \code{sprintf}, which gets \code{names(x)} passed as second and \code{x}
#'     as third argument. (This order can be inverted, see next argument.)}
#'   }
#' @param style Character scalar. The main options are:
#'   \describe{
#'   \item{\sQuote{table} or \sQuote{list}:}{Passed to \code{formatDL}.}
#'   \item{\sQuote{sentence}:}{A comma-separated list is created from \code{x},
#'   the last separator according to \code{last.sep}.}
#'   \item{\sQuote{m4} or \sQuote{M4}}{acronym{GNU} \command{m4} macro
#'   definitions using double or single quoting, respectively, for the
#'   expansions are created. A warning is issued if the macro strings are
#'   invalid, which is always the case of \code{x} has no names; \code{prepend}
#'   is ignored.}
#'   \item{Otherwise:}{A template for \code{sprintf} is assumed taking two
#'   additional arguments, first \code{names(x)} and then \code{x}.}
#'   }
#'   Note that names and values of \code{x} are exchanged beforehand if
#'   \code{style} is run through \code{I} from the \pkg{base} package.
#' @param collapse Character scalar used to join the resulting vector elements.
#'   It is by default also applied for joining \code{header} and \code{footer}
#'   footer with them (if provided). This can be turned off using hf.collapse.
#'   By default this is an empty string for \sQuote{sentence} style, the newline
#'   character otherwise.
#' @param force.numbers Logical scalar. Always use numbers instead of the
#'   \sQuote{names} attribute?
#' @param last.sep Character scalar indicating what should be used as last
#'   separator if \code{style} is \sQuote{sentence}. \sQuote{both} means
#'   \sQuote{and} and comma, \sQuote{two} means \sQuote{or} and comma.
#' @param hf.collapse Character scalar or empty. If distinct from
#'   \code{collapse}, used for separately for joining \code{header} and
#'   \code{footer} (if provided).
#' @param ... Optional other arguments passed to \code{formatDL}.
#' @return Character scalar.
#' @export
#' @seealso base::message base::warning base::stop base::formatDL
#' @family coding-functions
#' @keywords utilities
#' @examples
#'
#' ## listing()
#'
#' # default style
#' x <- structure(letters[1:5], names = LETTERS[1:5])
#' (y <- listing(x, "Five letters:", "...end here", 1))
#' stopifnot(length(y) == 1, y ==
#'   "Five letters:\n A: a\n B: b\n C: c\n D: d\n E: e\n...end here")
#'
#' # 'sentence' style
#' (y <- listing(letters[1:3], style = "sentence", last.sep = "both"))
#' stopifnot(y == "a, b, and c", length(y) == 1)
#' (y <- listing(letters[1:3], style = I("sentence"), last.sep = "both"))
#' stopifnot(y == "1, 2, and 3", length(y) == 1)
#' (y <- listing(letters[1:3], style = "sentence", prepend = TRUE))
#' stopifnot(y == "1: a, 2: b and 3: c", length(y) == 1)
#' (y <- listing(letters[1:3], style = I("sentence"), prepend = TRUE))
#' stopifnot(y == "a: 1, b: 2 and c: 3", length(y) == 1)
#' (y <- listing(letters[1:3], style = "sentence", prepend = "%s=>%s"))
#' stopifnot(y == "1=>a, 2=>b and 3=>c", length(y) == 1)
#' (y <- listing(letters[1:3], style = "sentence", last.sep = "two"))
#' stopifnot(y == "a, b, or c", length(y) == 1)
#'
#' # with explicit sprintf template
#' (y <- listing(x, style = "%s, %s", collapse = "; ", prepend = "!"))
#' stopifnot(y == "!A, a; !B, b; !C, c; !D, d; !E, e", length(y) == 1)
#' (y <- listing(x, style = I("%s, %s"), collapse = "; ", prepend = "!"))
#' stopifnot(y == "!a, A; !b, B; !c, C; !d, D; !e, E", length(y) == 1)
#'
#' # create m4 macro definitions
#' (y <- listing(x, style = "m4"))
#' stopifnot(grepl("^(define\\([^)]+\\)dnl\n?)+$", y), length(y) == 1)
#'
#' # other 'x' arguments
#' stopifnot(listing(x) == listing(as.list(x)))
#' old.opt <- options(digits = 3)
#' stopifnot(listing(pi) == "1: 3.14") # controlled by getOption("digits")
#' options(old.opt)
#'
#' ## flatten()
#' x <- list(a = list(b = 1:5, c = letters[1:5]), d = LETTERS[1:3],
#'   e = list(pi))
#' (y <- flatten(x)) # sublists removed, non-list elements kept
#' stopifnot(is.list(y), length(y) == 4, !sapply(y, is.list))
#' # atomic objects are not modified by default
#' stopifnot(identical(letters, flatten(letters)))
#'
setGeneric("listing", function(x, ...) standardGeneric("listing"))

setMethod("listing", "numeric", function(x, ...) {
  x <- signif(x, getOption("digits"))
  storage.mode(x) <- "character"
  listing(x, ...)
}, sealed = SEALED)

setMethod("listing", "factor", function(x, ...) {
  y <- as.character(x)
  names(y) <- names(x)
  listing(y, ...)
}, sealed = SEALED)

setMethod("listing", "ANY", function(x, ...) {
  listing(c(unclass(x)), ...)
}, sealed = SEALED)

setMethod("listing", "list", function(x, ...) {
  listing(unlist(x), ...)
}, sealed = SEALED)

setMethod("listing", "character", function(x, header = NULL, footer = NULL,
    prepend = FALSE, style = "list", collapse = if (style == "sentence")
      ""
    else
      "\n", force.numbers = FALSE,
    last.sep = c("and", "both", "comma", "or", "two"), hf.collapse = collapse,
    ...) {

  spaces <- function(x) {
    if (is.character(x))
      x
    else if (is.numeric(x))
      sprintf(sprintf("%%%is", x), "")
    else if (x)
      "\t"
    else
      ""
  }

  do_prepend <- function(x, prepend) paste0(spaces(L(prepend)), x)

  sentence <- function(x, last, prepend) {
    get_last_sep <- function(last) case(last, and = " and ", comma = ", ",
      both = ", and ", or = " or ", two = ", or ")
    if (is.character(prepend))
      x <- sprintf(prepend, names(x), x)
    else if (prepend)
      x <- paste(names(x), x, sep = ": ")
    case(n <- length(x),
      stop("empty 'x' argument in 'sentence' mode"),
      x,
      paste0(x, collapse = get_last_sep(last)),
      paste(paste0(x[-n], collapse = ", "), x[n], sep = get_last_sep(last))
    )
  }

  to_m4 <- function(x, single) {
    is_macro <- function(x) grepl("^[A-Za-z_]\\w*$", x, FALSE, TRUE)
    do_quote <- function(x, single) {
      x <- chartr("`", "'", x)
      if (single)
        sprintf("`%s'", gsub("'", "''`", x, FALSE, FALSE, TRUE))
      else
        sprintf("``%s''", gsub("'", "'''``", x, FALSE, FALSE, TRUE))
    }
    if (any(bad <- !is_macro(y <- names(x))))
      warning(sprintf("not a valid m4 macro string: '%s'", y[bad][1L]))
    sprintf("define(%s, %s)dnl", do_quote(y, TRUE), do_quote(x, single))
  }

  LL(style, collapse, force.numbers, hf.collapse)
  if ((is.null(names(x)) && style != "insert") || force.numbers)
    names(x) <- seq_along(x)
  if (inherits(style, "AsIs"))
    x <- structure(names(x), names = x)
  switch(style,
    table =,
    list = x <- do_prepend(formatDL(x = x, style = style, ...), prepend),
    sentence = x <- sentence(x, match.arg(last.sep), prepend),
    m4 = x <- to_m4(x, FALSE),
    M4 = x <- to_m4(x, TRUE),
    x <- do_prepend(sprintf(style, names(x), x), prepend)
  )
  if (identical(collapse, hf.collapse))
    paste0(c(header, x, footer), collapse = collapse)
  else
    paste0(c(header, paste0(x, collapse = collapse), footer),
      collapse = hf.collapse)
}, sealed = SEALED)

#= flatten listing

#' @rdname listing
#' @export
#'
setGeneric("flatten", function(object, ...) standardGeneric("flatten"))

setMethod("flatten", "ANY", function(object, ...) {
  if (is.atomic(object))
    return(object)
  stop("need atomic 'object' (or specific 'flatten' method)")
}, sealed = SEALED)

setMethod("flatten", "list", function(object, use.names = TRUE, ...) {
  while (any(is.a.list <- vapply(object, is.list, NA))) {
    object[!is.a.list] <- lapply(object[!is.a.list], list)
    object <- unlist(object, FALSE, use.names)
  }
  object
}, sealed = SEALED)


################################################################################


#' Collect information
#'
#' Methods for collecting information from list-like objects into a matrix or
#' data frame or for re-assigning values to columns in a matrix.
#'
#' @param x List or matrix.
#' @param what Character scalar indicating how to collect information. The
#'   following values are supported by the list method: \describe{
#'   \item{counts}{For all non-list elements of \code{x}, count their
#'   occurrences.}
#'   \item{occurrences}{Like \sQuote{counts}, but only indicate presence or
#'   absence.}
#'   \item{values}{Simplify all direct elements of \code{x}, irrespective of
#'   whether or not they are lists, for including them as rows in a data frame.
#'   Their names determine the columns. See \code{keep.unnamed} for the action
#'   in the case of missing names.}
#'   \item{elements}{Like \sQuote{elements}, but collect only the non-list
#'   elements of \code{x}, i.e. flatten \code{x} in the first step.}
#'   \item{datasets}{Convert all elements to data frames or matrices, then merge
#'   them using rows and column names. In case of conflict, the last ones win.
#'   Here, the behaviour of other arguments is special if all elements of
#'   \code{x} are atomic. See below.}
#'   }
#'   The matrix method currently only supports \code{columns}, which means
#'   assorting the values to the columns anew based on the majority of their
#'   occurrences, and \code{rows}, which does the same for the rows.
#' @param min.cov Numeric scalar indicating the minimal coverage required in the
#'   resulting presence-absence matrix. Columns with a fewer number of non-zero
#'   entries are removed.
#' @param keep.unnamed Logical scalar indicating whether names should be
#'   inserted for elements of \code{x} that miss them. If \code{NA}, they are
#'   skipped, but with a warning; if \code{FALSE}, they are skipped silently.
#'   This only has an effect in conjunction with the last three values of
#'   \code{what}. If \kbd{datasets} are chosen, it usually has only an
#'   effect if all elements of \code{x} are atomic.
#' @param dataframe Logical scalar indicating whether a data frame should be
#'   produced instead of a matrix.
#' @param optional See \code{as.data.frame} from the \pkg{base} package.
#' @param stringsAsFactors See \code{as.data.frame} from the \pkg{base} package.
#' @param empty Character scalar used as intermediary placeholder for empty and
#'   missing values.
#' @param ... Optional arguments passed to and from other methods (if requested
#'   to \code{as.data.frame}).
#' @export
#' @return The list method of \code{flatten} returns a non-nested list. The
#'   \code{collect} methods yield a data frame or a matrix.
#' @family coding-functions
#' @details The list method of \code{flatten} is based on
#'   \url{http://stackoverflow.com/questions/8139677/} with some slight
#'   improvements.
#' @seealso base::unlist base::as.data.frame
#' @keywords manip
#' @examples
#'
#' ## collect()
#' x <- list(X = list(A = 1:3, B = 7L, C = list('c1', 1:3)),
#'   Y = list(A = 1:3, 11, B = -1L, D = "?"))
#'
#' ## collect values into a data frame or matrix
#' (got <- collect(x, "values", dataframe = TRUE))
#' stopifnot(LETTERS[1:4] == colnames(got))
#' stopifnot(names(x) == rownames(got))
#' stopifnot(is.list(got$A), is.integer(got$B), is.list(got$C),
#'   is.factor(got$D))
#' stopifnot(!is.na(got$A), !is.na(got$B), anyNA(got$C), anyNA(got$D))
#' # include the unnamed ones
#' got <- collect(x, "values", dataframe = TRUE, keep.unnamed = TRUE)
#' stopifnot(dim(got) == c(2, 5))
#' # simplify to matrix
#' (got <- collect(x, "values", dataframe = FALSE))
#' stopifnot(is.matrix(got), mode(got) == "list")
#'
#' ## collect elements into a data frame or matrix
#' (got <- collect(x, "elements", dataframe = TRUE))
#' stopifnot(dim(got) == c(2, 9), colnames(x) == rownames(got),
#'   is.data.frame(got))
#' (got <- collect(x, "elements", dataframe = FALSE))
#' stopifnot(dim(got) == c(2, 9), colnames(x) == rownames(got),
#'   !is.data.frame(got))
#'
#' ## count or just note occurrences
#' (got <- collect(x, "counts", dataframe = FALSE))
#' stopifnot(dim(got) == c(2, 8), rownames(got) == names(x),
#'   setequal(colnames(got), unlist(x)), any(got > 1))
#' (got <- collect(x, "occurrences", dataframe = FALSE))
#' stopifnot(dim(got) == c(2, 8), rownames(got) == names(x),
#'   setequal(colnames(got), unlist(x)), !any(got > 1))
#'
#' ## convert to data frames and insert everything in a single one
#' (got <- collect(x, "datasets", optional = FALSE, dataframe = TRUE))
#' stopifnot(dim(got) == c(3, 6), is.data.frame(got))
#'
#' ## a more useful application is to merge matrices
#' m1 <- matrix(1:4, ncol = 2, dimnames = list(c("A", "B"), c("x", "y")))
#' m2 <- matrix(1:4, ncol = 2, dimnames = list(c("C", "B"), c("x", "z")))
#' (got <- collect(list(m1, m2), "datasets"))
#' # values missing in some matrix yield NA
#' stopifnot(dim(got) == c(3, 3), anyNA(got))
#'
collect <- function(x, what, ...) UseMethod("collect")

#' @rdname collect
#' @method collect list
#' @export
#'
collect.list <- function(x,
    what = c("counts", "occurrences", "values", "elements", "datasets"),
    min.cov = 1L, keep.unnamed = FALSE, dataframe = FALSE, optional = TRUE,
    stringsAsFactors = default.stringsAsFactors(), ...) {

  # collecting 'counts' or 'occurrences'
  note_in_matrix <- function(x, count, min.cov, dataframe, optional,
      stringsAsFactors, ...) {
    x <- lapply(lapply(x, unlist), as.character)
    n <- sort.int(unique.default(unlist(x, FALSE, FALSE)))
    result <- matrix(0L, length(x), length(n), FALSE, list(names(x), n))
    if (count)
      for (i in seq_along(x)) {
        value <- table(x[[i]], useNA = "ifany")
        result[i, names(value)] <- unclass(value)
      }
    else
      for (i in seq_along(x))
        result[i, x[[i]]] <- 1L
    if (min.cov > 0L)
      result <- result[, colSums(result) >= min.cov, drop = FALSE]
    if (dataframe)
      as.data.frame(x = result, optional = optional,
        stringsAsFactors = stringsAsFactors, ...)
    else
      result
  }

  # collecting 'values' or 'elements'
  insert_into_matrix <- function(x, flat, keep.unnamed, dataframe, optional,
      stringsAsFactors, verbose, ...) {
    oneify <- function(x) {
      if (is.atomic(x))
        return(x)
      size <- vapply(x, length, 0L)
      x[!size] <- NA
      x[size > 1L] <- lapply(x[size > 1L], list)
      x
    }
    keep_validly_named_only <- function(x) {
      if (is.null(names(x)))
        NULL
      else
        x[nzchar(names(x))]
    }
    keep_validly_named_only_but_complain <- function(x) {
      if (is.null(names(x))) {
        warning("removing unnamed vector")
        NULL
      } else if (!all(ok <- nzchar(names(x)))) {
        warning("removing elements with empty names")
        x[ok]
      } else
        x
    }
    enforce_names <- function(x) {
      names(x) <- if (is.null(n <- names(x)))
          seq_along(x)
        else
          ifelse(nzchar(n), n, seq_along(x))
      x
    }
    if (flat)
      x <- lapply(x, unlist)
    else
      x <- lapply(x, oneify)
    if (keep.unnamed)
      x <- lapply(x, enforce_names)
    else if (verbose)
      x <- lapply(x, keep_validly_named_only_but_complain)
    else
      x <- lapply(x, keep_validly_named_only)
    n <- unique.default(unlist(lapply(x, names), FALSE))
    result <- matrix(NA, length(x), length(n), FALSE, list(names(x), n))
    result <- as.data.frame(x = result, stringsAsFactors = FALSE,
      optional = TRUE, ...)
    for (i in seq_along(x))
      result[i, names(x[[i]])] <- x[[i]]
    if (dataframe && stringsAsFactors)
      for (i in which(vapply(result, is.character, NA)))
        result[, i] <- as.factor(result[, i])
    if (!optional)
      names(result) <- make.names(names(result))
    if (dataframe)
      result
    else
      as.matrix(result)
  }

  # collecting 'datasets'
  collect_matrices <- function(x, keep.unnamed, dataframe, optional,
      stringsAsFactors, verbose, ...) {
    keep_validly_named_only <- function(x) {
      if (is.null(colnames(x)) || is.null(rownames(x)))
        NULL
      else
        x[nzchar(rownames(x)), nzchar(colnames(x)), drop = FALSE]
    }
    keep_validly_named_only_but_complain <- function(x) {
      if (is.null(colnames(x)) || is.null(rownames(x))) {
        warning("removing unnamed matrix or data frame")
        NULL
      } else {
        row.ok <- nzchar(rownames(x))
        col.ok <- nzchar(colnames(x))
        if (!all(row.ok) || !all(col.ok)) {
          warning("removing rows and/or columns with empty names")
          x[row.ok, col.ok, drop = FALSE]
        } else
          x
      }
    }
    enforce_names <- function(x) {
      enforce <- function(x, n) if (is.null(x))
          seq_len(n)
        else
          ifelse(nzchar(x), x, seq_len(n))
      rownames(x) <- enforce(rownames(x), nrow(x))
      colnames(x) <- enforce(colnames(x), ncol(x))
      x
    }
    if (all.atomic <- all(vapply(x, is.atomic, NA))) {
      x <- lapply(x, as.matrix)
      if (keep.unnamed)
        x <- lapply(x, enforce_names)
      else if (verbose)
        x <- lapply(x, keep_validly_named_only_but_complain)
      else
        x <- lapply(x, keep_validly_named_only)
    } else
      x <- lapply(x, data.frame, stringsAsFactors = FALSE,
        check.names = !optional)
    rn <- sort.int(unique.default(unlist(lapply(x, rownames))))
    cn <- sort.int(unique.default(unlist(lapply(x, colnames))))
    result <- matrix(NA, length(rn), length(cn), FALSE, list(rn, cn))
    if (!all.atomic)
      result <- as.data.frame(result, stringsAsFactors = FALSE,
        optional = optional)
    for (mat in x)
      result[rownames(mat), colnames(mat)] <- mat
    if (dataframe) {
      if (all.atomic)
        result <- as.data.frame(result)
      if (stringsAsFactors)
        for (i in which(vapply(result, is.character, NA)))
          result[, i] <- as.factor(result[, i])
    } else if (!all.atomic)
      result <- as.matrix(result)
    if (!optional)
      colnames(result) <- make.names(colnames(result))
    result
  }

  LL(min.cov, dataframe, optional, stringsAsFactors, keep.unnamed)
  if (verbose <- is.na(keep.unnamed))
    keep.unnamed <- FALSE
  case(match.arg(what),
    counts = note_in_matrix(x, TRUE, min.cov, dataframe, optional,
      stringsAsFactors, ...),
    occurrences = note_in_matrix(x, FALSE, min.cov, dataframe, optional,
      stringsAsFactors, ...),
    elements = insert_into_matrix(x, TRUE, keep.unnamed, dataframe, optional,
      stringsAsFactors, verbose, ...),
    values = insert_into_matrix(x, FALSE, keep.unnamed, dataframe, optional,
      stringsAsFactors, verbose, ...),
    datasets = collect_matrices(x, keep.unnamed, dataframe, optional,
      stringsAsFactors, verbose, ...)
  )
}

#' @rdname collect
#' @method collect matrix
#' @export
#'
collect.matrix <- function(x, what = c("columns", "rows"), empty = "?", ...) {

  assort_columns <- function(x, empty) {
    assort <- function(x) {
      result <- integer(nrow(x))
      repeat {
        if (all(result) || !(m <- max(x)))
          break
        pos <- which(x == m, TRUE)[1L, ]
        result[pos[[1L]]] <- pos[[2L]]
        x[pos[[1L]], ] <- x[, pos[[2L]]] <- 0
      }
      if (any(pos <- !result))
        result[pos] <- setdiff(seq_len(ncol(x)), result)[seq_along(which(pos))]
      result
    }
    stopifnot(is.matrix(x), is.character(x))
    x[!nzchar(x) | is.na(x)] <- empty
    n <- unique.default(x)
    n <- matrix(0L, length(n), ncol(x), FALSE, list(n, colnames(x)))
    for (i in seq_len(ncol(x))) {
      cnt <- table(x[, i])
      n[names(cnt), i] <- cnt[]
    }
    n <- sweep(n, 2L, colSums(n), "/")
    n[match(empty, rownames(n), 0L), ] <- 0
    for (i in seq_len(nrow(x)))
      x[i, assort(n[x[i, ], , drop = FALSE])] <- x[i, ]
    x[x == empty] <- ""
    x
  }

  LL(empty)
  case(match.arg(what),
    columns = assort_columns(x, empty),
    rows = t(assort_columns(t(x), empty))
  )
}


################################################################################


#' Prepare class names
#'
#' Ensure that a vector of class names contains only unique values and
#' \sQuote{character}. Reduce it to \sQuote{ANY} if \sQuote{ANY} is contained.
#' See \code{\link{map_values}} for a use.
#'
#' @param x Character vector.
#' @return Character vector.
#' @keywords internal
#'
prepare_class_names <- function(x) UseMethod("prepare_class_names")

#' @rdname prepare_class_names
#' @method prepare_class_names character
#' @export
#'
prepare_class_names.character <- function(x) {
  x <- unique.default(c("character", x))
  if ("ANY" %in% x)
    "ANY"
  else
    x
}


################################################################################


#' Map values or names
#'
#' Map values using a character vector, a function or an expression, or use a
#' character vector or a function for recursively mapping list names, or mapping
#' the \code{colnames} and \code{rownames} attributes of a data frame or matrix.
#'
#' @param object When mapping values, a list (may be nested), data frame or
#'   character vector. If it has names, they are preserved. \code{NULL} can also
#'   be given and yields \code{NULL} or an empty named character vector (if
#'   \code{mapping} is missing). \code{object} may also be an array (which
#'   includes matrices).
#'
#'   When mapping names, \code{object} can be any \R object. The default method
#'   applies the mapping to the \sQuote{names} attribute. The behaviour is
#'   special for lists, which are traversed recursively to also consider
#'   contained lists with names. Data frames and array objects (that is,
#'   including matrices) are also treated specially because the \code{dimnames}
#'   attribute, not the \sQuote{names} attribute is considered.
#'
#' @param mapping When mapping values, a character vector, function, expression,
#'   numeric scalar, \code{NULL} or missing.
#'   \itemize{
#'   \item If a character vector, used as a mapping from its names to its
#'   values. Values from \code{object} are searched for in the \code{names}
#'   attribute of \code{mapping}; those found are replaced by the corresponding
#'   values of \code{mapping}.
#'   \item If \code{mapping} is missing, a character vector is returned (sorted
#'   and with duplicates removed) whose names are identical to the values. This
#'   eases the construction of mapping vectors specific for \code{object}. If
#'   \code{mapping} is missing, the \code{coerce} argument must be named.
#'   \code{mapping} changes its usage if \code{coerce} is \code{TRUE}.
#'   \item For array objects, if \code{mapping} was a function, it would be
#'   applied to \code{object} after conversion with \code{as.vector}, and it
#'   would be attempted to add the original attributes (particularly important
#'   are \sQuote{dim} and \code{dimnames} back to the result.
#'   \item For array objects, if \code{mapping} is the usual character
#'   vector, it then is used for mapping the \code{storage.mode}, not the
#'   \code{class} of \code{object}.
#'   \item If \code{mapping} is an expression, all sub-expressions will be
#'   evaluated in \code{object} represented as an environment, which after
#'   conversion back to a list, is returned.
#'   \item If \code{mapping} is \code{NULL} and \code{object} is a list, all
#'   contained objects of zero length are removed recursively.
#'   \item If \code{mapping} is a numeric scalar and \code{object} is a
#'   character vector or factor, a mapping (named character vector) is created
#'   that translates groups of similar strings to their most frequent member.
#'   }
#'
#'   When mapping names, a mapping function that takes a character vector as
#'   first argument, or character vector used for mapping from its names to its
#'   values, or missing. It is guaranteed that \code{NULL} input remains
#'   \code{NULL}, irrespective of the value of \code{mapping}.
#'
#' @param coerce The usage of this argument depends on \code{object}.
#'   \itemize{
#'   \item A character vector with the names of classes that are coerced to
#'   \sQuote{character} to allow the mapping. Other classes are returned
#'   unchanged. Note that the coerced data are \strong{not} converted back to
#'   their original data type. \sQuote{ANY} can be used to indicate that all
#'   classes will be considered.
#'   \item Alternatively, \code{coerce} can be \code{TRUE}. \code{mapping} is
#'   then interpreted as a mapping between the names of classes, and \code{as}
#'   from the \pkg{methods} package is used for conducting the requested
#'   coercions. Attempting an undefined coercion will result in an error.
#'   \item For the expression method, an enclosing environment to look up
#'   objects that are not found in \code{mapping}.
#'   }
#' @param ... Optional further arguments to \code{mapping} (\strong{if} it is a
#'   function).
#' @export
#' @return \code{map_values} returns a list, data frame, a character or logical
#'   vector or \code{NULL}.
#'
#'   \code{map_names} yields a character vector if \code{mapping} is missing,
#'   otherwise an \R object of the same class than \code{object}.
#'
#' @seealso base::rapply base::list base::as.list methods::as base::class
#'   base::storage.mode base::as.vector
#' @family coding-functions
#' @keywords manip list
#'
#' @details Mapping of \sQuote{character} data using another \sQuote{character}
#'   vector is possible, as well as recursively applying a mapping function to
#'   all \sQuote{character} values within a list, or non-recursively to a data
#'   frame. Optionally other data types are coerced to \sQuote{character}; the
#'   remaining ones are returned unchanged. It is also possible to map between
#'   classes using coercion functions. For convenience in programming, methods
#'   for the \sQuote{NULL} class are also available.
#'
#'   Mapping of logical vectors using another vector expects (at least) three
#'   elements within the mapping vector, i.e. the values to be used for
#'   \code{FALSE}, \code{NA} and \code{TRUE} elements in \code{object}. Nothing
#'   is modified if the mapping is \code{NULL}. The default mapping vector
#'   \code{c(1L, 2L, 3L)} is used if \code{mapping} is missing.
#'
#'   In the case of lists, the function passed to \code{map_names} is not
#'   applied to list elements which are not themselves lists, even if they have
#'   a \sQuote{names} attribute. Such elements and their names, if any, are
#'   returned unchanged. If a \sQuote{names}, \code{colnames} or
#'   \code{rownames} attribute is \code{NULL}, it is ignored.
#'
#'   Alternatively, instead of mapping the names, collect them and return them
#'   as a single character vector, sorted and with duplicates removed. The
#'   collected names are added as their own \code{names} attribute; this might
#'   be useful if the result is later on used for some mapping (using this
#'   function or \code{\link{map_values}}).
#'
#'   The method for a numeric \code{mapping} argument and strings or factors as
#'   \code{object} argument can be used to correct misspellings. It is based on
#'   \code{adist} from the base package, to which the \code{...} arguments are
#'   passed. \code{mapping} indicates the maximum string distance allowed when
#'   placing strings into the same group. Distances are calculated as output of
#'   \code{adist} divided by the larger of the two strings length for
#'   \code{partial = FALSE} (the default) and the smaller one otherwise.
#'   \code{ignore.case} is \code{TRUE} per default, \code{useBytes} is
#'   \code{FALSE}. Clustering is done by single linkage for \code{partial =
#'   FALSE}, by complete linkage for \code{partial = TRUE}. An additional
#'   argument \code{exclude}, if a non-empty string, is used as a
#'   \acronym{PERL}-compatible regular expression to remove strings prior to
#'   determining misspellings.
#' @examples
#'
#' ## map_values()
#'
#' # Character/character method
#' map <- letters
#' names(map) <- rev(LETTERS)
#' (x <- map_values(LETTERS, map))
#' stopifnot(rev(x) == letters)
#'
#' # Character/missing method
#' (x <- map_values(letters))
#' stopifnot(x == letters, names(x) == letters)
#'
#' # Character/function method
#' x <- letters[1:4]
#' names(x) <- LETTERS[1:4]
#' (y <- map_values(x, function(z) sprintf("%s%s", z, z)))
#' stopifnot(names(y) == names(x), y != x)
#'
#' # Character/numeric method
#' x <- c("car", "cars", "car", "care", " Car")
#' (y <- map_values(x, 0.25))
#' stopifnot(length(y) == 3, y == "car")
#'
#' # List/character method
#' x <- list(a = 1:8, c = 9, d = 'x')
#' map <- c(a = "b", e = "f", x = "y")
#' (y <- map_values(x, map))
#' stopifnot(identical(x[1:2], y[1:2]), !identical(x[3], y[3]))
#' (y <- map_values(x, map, coerce = "integer"))
#' stopifnot(identical(x[2], y[2]), !identical(x[1], y[1]),
#'   !identical(x[3], y[3]))
#' (y <- map_values(x, map, coerce = TRUE))
#' stopifnot(identical(x, y))
#' (y <- map_values(x, c(numeric = "character"), coerce = TRUE))
#' stopifnot(identical(x[1], y[1]), !identical(x[2], y[2]),
#'   identical(x[3], y[3]))
#'
#' # List/function method
#' (y <- map_values(x, identity, coerce = "ANY"))
#' stopifnot(identical(x, y))
#' (y <- map_values(x, class, coerce = "ANY"))
#' stopifnot(sapply(y, class) == "character", names(y) == names(x))
#'
#' # List/missing method
#' (y <- map_values(x))
#' stopifnot(y == "x", names(y) == y)
#' (y <- map_values(x, coerce = "integer"))
#' stopifnot(length(y) == 9, names(y) == y)
#' (y <- map_values(x, coerce = c("integer", "numeric")))
#' stopifnot(length(y) == 10, names(y) == y)
#' (y <- map_values(x, coerce = "ANY")) # same effect
#' stopifnot(length(y) == 10, names(y) == y)
#' (y <- map_values(x, coerce = TRUE))
#' stopifnot(y == c("character", "integer", "numeric"), names(y) == y)
#'
#' # List/expression method
#' (y <- map_values(x, expression(b <- a + c)))
#' stopifnot(is.list(y), y$b == c(10:17))
#'
#' # List/expression method applied to a data frame
#' x <- data.frame(a = 1:5, b = 6:10)
#' (y <- map_values(x, expression(c <- a + b)))
#' stopifnot(is.data.frame(y), dim(y) == c(5, 3))
#'
#' # Data frame/character method
#' x <- data.frame(a = 1:3, b = letters[1:3])
#' stopifnot(sapply(x, class) == c("integer", "factor"))
#' map <- c(a = "A", b = "B", c = "C", `1` = "5")
#' (y <- map_values(x, map))
#' stopifnot(identical(x, y))
#' (y <- map_values(x, map, coerce = "factor"))
#' stopifnot(!identical(x, y), y[[2]] == c("A", "B", "C"))
#' (y <- map_values(x, map, coerce = "ANY"))
#' stopifnot(y[[1]] == c("5", "2", "3"), y[[2]] == c("A", "B", "C"))
#' (y <- map_values(x, map, coerce = TRUE))
#' stopifnot(identical(x, y))
#' map <- c(factor = "character", integer = "complex")
#' (y <- map_values(x, map, coerce = TRUE))
#' stopifnot(sapply(y, class) == c("complex", "character"))
#'
#' # Data frame/function method
#' (y <- map_values(x, `+`, coerce = "integer", y = 1L))
#' stopifnot(y$a == x$a + 1L)
#' (y <- map_values(x, as.character, coerce = "factor"))
#' stopifnot(sapply(y, class) == c("integer", "character"))
#'
#' # Data frame/missing method
#' (y <- map_values(x))
#' stopifnot(is.character(y), length(y) == 0)
#' (y <- map_values(x, coerce = "factor"))
#' stopifnot(is.character(y), y == letters[1:3], names(y) == y)
#' (y <- map_values(x, coerce = "ANY"))
#' stopifnot(is.character(y), length(y) == 6, names(y) == y)
#' (y <- map_values(x, coerce = TRUE))
#' stopifnot(is.character(y), y == c("factor", "integer"), names(y) == y)
#'
#' # Matrix/character method
#' (x <- matrix(1:6, nrow = 2))
#' (y <- map_values(x, c(integer = "numeric"), coerce = TRUE))
#' stopifnot(storage.mode(x) != storage.mode(y))
#' (y <- map_values(x, c(`1` = "7"), coerce = "integer"))
#' stopifnot(is.character(y), y[-1] == x[-1])
#'
#' # Matrix/function method
#' (y <- map_values(x, identity))
#' stopifnot(identical(x, y))
#' (y <- map_values(x, `+`, y = 1)) # useless because '+' is directly available
#' stopifnot(dim(y) == dim(x), y == x + 1)
#'
#' # Matrix/missing method
#' (y <- map_values(x))
#' stopifnot(y == "integer", names(y) == y)
#' (y <- map_values(x, coerce = "ANY"))
#' stopifnot(is.character(y), y == 1:6, names(y) == y)
#'
#' # Factor/function method
#' x <- as.factor(c("a", "b", "a"))
#' (y <- map_values(x, toupper))
#' stopifnot(is.factor(y), y == toupper(x))
#'
#' # Factor/character method
#' (y <- map_values(x, c(b = "c", k = "h")))
#' stopifnot(is.factor(y), levels(y) == c("a", "c"))
#'
#' # Factor/missing method
#' (y <- map_values(x))
#' stopifnot(levels(x) == y, names(y) == y)
#'
#' ## map_names()
#'
#' # List/function method
#' x <- list(a = 1:8, c = 9, d = 'x')
#' map <- function(x) sprintf("%s%s", x, x)
#' (y <- map_names(x, map))
#' stopifnot(identical(as.character(x), as.character(y)))
#' stopifnot(!identical(names(x), names(y)))
#'
#' # List/character method
#' x <- list(a = 1:8, c = 9, d = 'x')
#' map <- c(a = "b", e = "f", x = "y")
#' (y <- map_names(x, map))
#' stopifnot(identical(as.character(x), as.character(y)))
#' stopifnot(!identical(names(x), names(y)))
#' # compare with the map_values() example
#'
#' # List/missing method
#' x <- list(a = 1:8, c = 9, d = 'x')
#' (y <- map_names(x))
#' stopifnot(identical(as.vector(y), names(x)))
#' stopifnot(identical(names(y), names(x)))
#' # Now a recursive list
#' x <- list(a = 1:8, c = 9, d = list(d1 = 'x', d2 = 'y'))
#' (y <- map_names(x))
#' stopifnot(length(y) > length(names(x)))
#'
#' # Data frame/function method
#' x <- data.frame(a = 1:3, b = letters[1:3])
#' (y <- map_names(x, toupper))
#' stopifnot(identical(y[[1]], x[[1]]), identical(y[[2]], x[[2]]))
#' stopifnot(identical(names(y), c("A", "B")))
#'
#' # Data frame/character method
#' (y <- map_names(x, c(a = "b", b = "a")))
#' stopifnot(x == y, names(y) == c("b", "a"))
#'
#' # Data frame/missing method
#' (y <- map_names(x))
#' stopifnot(is.character(y), y == names(y), length(y) == 5)
#'
#' # Matrix/function method
#' x <- as.matrix(x)
#' (y <- map_names(x, toupper))
#' stopifnot(x == y, toupper(colnames(x)) == colnames(y))
#'
#' # Matrix/character method
#' (y <- map_names(x, c(a = "b", b = "a")))
#' stopifnot(x == y, colnames(y) == c("b", "a"))
#'
#' # Matrix/missing method
#' (y <- map_names(x))
#' stopifnot(y == c("a", "b"), names(y) == y)
#'
setGeneric("map_values",
  function(object, mapping, ...) standardGeneric("map_values"))

#-------------------------------------------------------------------------------

setMethod("map_values", c("list", "character"), function(object, mapping,
    coerce = character()) {
  if (isTRUE(coerce)) {
    if (is.null(coerce <- names(mapping)))
      return(object)
    mapfun <- function(item) as(item, map_values(class(item), mapping))
  } else
    mapfun <- if (length(coerce) == 0L || all(coerce == "character"))
        function(item) map_values(item, mapping)
      else
        function(item) {
          result <- map_values(as.character(item), mapping)
          mostattributes(result) <- attributes(item)
          result
        }
  map_values(object, mapping = mapfun, coerce = coerce)
}, sealed = SEALED)

setMethod("map_values", c("list", "function"), function(object, mapping,
    coerce = character(), ...) {
  rapply(object = object, f = mapping, classes = prepare_class_names(coerce),
    how = "replace", ...)
}, sealed = SEALED)

setMethod("map_values", c("list", "NULL"), function(object, mapping,
    coerce = character()) {
  clean_recursively <- function(x) {
    if (!is.list(x))
      return(x)
    x <- lapply(x, clean_recursively)
    x[vapply(x, length, 0L) > 0L]
  }
  if (length(coerce))
    object <- rapply(object, as.character, prepare_class_names(coerce), NULL,
      "replace")
  clean_recursively(object)
}, sealed = SEALED)

setMethod("map_values", c("list", "missing"), function(object, mapping,
    coerce = character()) {
  if (isTRUE(coerce)) {
    classes <- "ANY"
    mapfun <- class
  } else {
    classes <- prepare_class_names(coerce)
    mapfun <- as.character
  }
  map_values(rapply(object, mapfun, classes = classes))
}, sealed = SEALED)

setMethod("map_values", c("list", "expression"), function(object, mapping,
    coerce = parent.frame()) {
  e <- list2env(object, NULL, coerce)
  for (subexpr in mapping)
    eval(subexpr, e)
  e <- as.list(e) # return 'e' if the order of list elements doesn't matter
  novel <- setdiff(names(e), names(object))
  for (name in setdiff(names(object), names(e)))
    object[[name]] <- NULL
  object[novel] <- e[novel]
  object
}, sealed = SEALED)

#-------------------------------------------------------------------------------

setMethod("map_values", c("data.frame", "function"), function(object, mapping,
    coerce = character(), ...) {
  if (identical("ANY", coerce <- prepare_class_names(coerce)))
    coerce <- unique(unlist((lapply(object, class))))
  for (i in which(vapply(object, inherits, NA, coerce)))
    object[[i]] <- mapping(object[[i]], ...)
  object
}, sealed = SEALED)

setMethod("map_values", c("data.frame", "character"), function(object, mapping,
    coerce = character()) {
  if (isTRUE(coerce)) {
    if (is.null(coerce <- names(mapping)))
      return(object)
    mapfun <- function(item) as(item, map_values(class(item), mapping))
  } else {
    mapfun <- function(item) map_values(as.character(item), mapping)
  }
  map_values(object, mapping = mapfun, coerce = coerce)
}, sealed = SEALED)

setMethod("map_values", c("data.frame", "NULL"), function(object, mapping,
    coerce = character(), ...) {
  if (identical("ANY", coerce <- prepare_class_names(coerce)))
    coerce <- unique(unlist((lapply(object, class))))
  for (i in which(vapply(object, inherits, NA, coerce)))
    object[[i]] <- as.character(object[[i]])
  object
}, sealed = SEALED)

setMethod("map_values", c("data.frame", "missing"), function(object,
    coerce = character()) {
  if (isTRUE(coerce)) {
    result <- unlist(lapply(object, class))
  } else {
    coerce <- prepare_class_names(coerce)
    if (!"ANY" %in% coerce)
      object <- object[, vapply(object, inherits, NA, coerce),
        drop = FALSE]
    result <- unlist(lapply(object, as.character))
  }
  map_values(result)
}, sealed = SEALED)

#-------------------------------------------------------------------------------

setMethod("map_values", c("array", "character"), function(object, mapping,
    coerce = TRUE) {
  if (isTRUE(coerce)) {
    storage.mode(object) <- map_values(storage.mode(object), mapping)
    object
  } else {
    coerce <- prepare_class_names(coerce)
    if (!identical("ANY", coerce) && !storage.mode(object) %in% coerce)
      stop("storage mode of 'object' not contained in 'coerce'")
    result <- map_values(as.character(object), mapping)
    attributes(result) <- attributes(object)
    result
  }
}, sealed = SEALED)

setMethod("map_values", c("array", "missing"), function(object, coerce = TRUE) {
  if (isTRUE(coerce)) {
    result <- storage.mode(object)
  } else {
    coerce <- prepare_class_names(coerce)
    if (!identical("ANY", coerce) && !storage.mode(object) %in% coerce)
      stop("storage mode of 'object' not contained in 'coerce'")
    result <- as.character(object)
  }
  map_values(result)
}, sealed = SEALED)

setMethod("map_values", c("array", "function"), function(object, mapping, ...) {
  result <- mapping(as.vector(object), ...)
  mostattributes(result) <- c(attributes(result), attributes(object))
  result
}, sealed = SEALED)

#-------------------------------------------------------------------------------

setMethod("map_values", c("character", "function"), function(object, mapping,
    ...) {
  result <- mapping(object, ...)
  mostattributes(result) <- attributes(object)
  result
}, sealed = SEALED)

setMethod("map_values", c("character", "character"), function(object, mapping) {
  mapped <- match(object, names(mapping), 0L)
  object[found] <- mapping[mapped[found <- mapped > 0L]]
  object
}, sealed = SEALED)

setMethod("map_values", c("character", "missing"), function(object) {
  object <- sort.int(unique.default(object))
  structure(object, names = object)
}, sealed = SEALED)

setMethod("map_values", c("character", "NULL"), function(object, mapping) {
  object
}, sealed = SEALED)

setMethod("map_values", c("character", "numeric"), function(object, mapping,
    ...) {
  adist2map(x = object, max.distance = mapping, ...)
}, sealed = SEALED)

#-------------------------------------------------------------------------------

setMethod("map_values", c("factor", "function"), function(object, mapping,
    ...) {
  levels(object) <- map_values(levels(object), mapping, ...)
  object
}, sealed = SEALED)

setMethod("map_values", c("factor", "character"), function(object, mapping) {
  levels(object) <- map_values(levels(object), mapping)
  object
}, sealed = SEALED)

setMethod("map_values", c("factor", "missing"), function(object) {
  map_values(levels(object))
}, sealed = SEALED)

setMethod("map_values", c("factor", "numeric"), function(object, mapping,
    ...) {
  adist2map(x = as.character(object), max.distance = mapping, ...)
}, sealed = SEALED)

#-------------------------------------------------------------------------------

setMethod("map_values", c("logical", "function"), function(object, mapping,
    ...) {
  result <- mapping(object, ...)
  mostattributes(result) <- attributes(object)
  result
}, sealed = SEALED)

setMethod("map_values", c("logical", "vector"), function(object, mapping) {
  result <- ifelse(object, mapping[[3L]], mapping[[1L]])
  result[is.na(result)] <- mapping[[2L]]
  attributes(result) <- attributes(object)
  result
}, sealed = SEALED)

setMethod("map_values", c("logical", "NULL"), function(object, mapping) {
  object
}, sealed = SEALED)

setMethod("map_values", c("logical", "missing"), function(object) {
  result <- object * 2L + 1L
  result[is.na(result)] <- 2L
  attributes(result) <- attributes(object)
  result
}, sealed = SEALED)

#-------------------------------------------------------------------------------

setMethod("map_values", c("NULL", "function"), function(object, mapping, ...) {
  NULL
}, sealed = SEALED)

setMethod("map_values", c("NULL", "character"), function(object, mapping) {
  NULL
}, sealed = SEALED)

setMethod("map_values", c("NULL", "missing"), function(object, mapping) {
  map_values(character())
}, sealed = SEALED)


#-------------------------------------------------------------------------------

#= map_names map_values

#' @rdname map_values
#' @export
#'
setGeneric("map_names",
  function(object, mapping, ...) standardGeneric("map_names"))

#-------------------------------------------------------------------------------

setMethod("map_names", c("list", "function"), function(object, mapping, ...) {
  map_names_recursively <- function(item) {
    if (is.list(item)) {
      names(item) <- map_values(names(item), mapping, ...)
      return(lapply(item, FUN = map_names_recursively))
    }
    item
  }
  map_names_recursively(object)
}, sealed = SEALED)

setMethod("map_names", c("list", "character"), function(object, mapping) {
  map_names_recursively <- function(item) {
    if (is.list(item)) {
      names(item) <- map_values(names(item), mapping)
      return(lapply(item, FUN = map_names_recursively))
    }
    item
  }
  map_names_recursively(object)
}, sealed = SEALED)

setMethod("map_names", c("list", "missing"), function(object) {
  get_names_recursively <- function(item) {
    if (is.list(item))
      c(names(item), unlist(lapply(item, FUN = get_names_recursively)))
    else
      character()
  }
  map_values(get_names_recursively(object))
}, sealed = SEALED)

#-------------------------------------------------------------------------------

setMethod("map_names", c("data.frame", "function"), function(object, mapping,
    ...) {
  dimnames(object) <- map_values(dimnames(object), mapping, ...)
  object
}, sealed = SEALED)

setMethod("map_names", c("data.frame", "character"), function(object, mapping) {
  dimnames(object) <- map_values(dimnames(object), mapping)
  object
}, sealed = SEALED)

setMethod("map_names", c("data.frame", "missing"), function(object) {
  map_values(dimnames(object))
}, sealed = SEALED)

#-------------------------------------------------------------------------------

setMethod("map_names", c("array", "function"), function(object, mapping, ...) {
  dimnames(object) <- map_values(dimnames(object), mapping, ...)
  object
}, sealed = SEALED)

setMethod("map_names", c("array", "character"), function(object, mapping) {
  dimnames(object) <- map_values(dimnames(object), mapping)
  object
}, sealed = SEALED)

setMethod("map_names", c("array", "missing"), function(object) {
  map_values(dimnames(object))
}, sealed = SEALED)

#-------------------------------------------------------------------------------

setMethod("map_names", c("ANY", "function"), function(object, mapping, ...) {
  names(object) <- map_values(names(object), mapping, ...)
  object
}, sealed = SEALED)

setMethod("map_names", c("ANY", "character"), function(object, mapping) {
  names(object) <- map_values(names(object), mapping)
  object
}, sealed = SEALED)

setMethod("map_names", c("ANY", "missing"), function(object) {
  map_values(names(object))
}, sealed = SEALED)


################################################################################


#' Query an object with another object
#'
#' One use is to test whether all names of a query list occur as names in a data
#' list and optionally also whether they point to the same elements; this
#' principle is applied recursively to all contained lists.
#'
#' @param object List containing the data,.
#' @param other List used as query.
#' @param values Logical scalar. Compare also the values or only the keys? If
#'   \code{FALSE}, \code{exact} is ignored.
#' @param exact Logical scalar. If \code{FALSE}, the data value(s) might by any
#'   of the query value(s), and some coercion is done before comparing (see
#'   \code{match} for details.
#'
#'   If \code{TRUE}, the data value(s) must exactly correspond to the query
#'   value(s), and no coercion is done (see \code{identical}) for details). This
#'   might be too strict for most applications.
#' @param ... Optional arguments passed to \code{identical} from the \pkg{base}
#'   package, allowing for fine-control of identity. Has no effect unless
#'   \code{exact} is \code{TRUE}.
#' @export
#' @return Logical scalar.
#' @details  Non-list elements are ignored if \code{values} is \code{FALSE}.
#'   Otherwise the comparison is done using \code{identical} if \code{exact} is
#'   \code{TRUE}. If \code{exact} is \code{FALSE}, the value(s) in the data list
#'   can be any of the values at the corresponding position in the query list,
#'   and the comparison is done by coercion to character vectors. An empty query
#'   list results in \code{TRUE}. Missing names in a non-empty query list result
#'   in \code{FALSE}.
#' @family coding-functions
#' @seealso base::list base::as.list base::`[` base::`[[` base::match
#' @seealso base::identity
#' @keywords attribute list
#' @examples
#'
#' # List/list method
#' x <- list(a = 1:8, c = 9, d = list(d1 = 'x', d2 = 'y'))
#' y <- list(a = 1:10, c = "9", d = list(d1 = "x"))
#' stopifnot(contains(x, y))
#' stopifnot(!contains(x, y, exact = TRUE))
#' stopifnot(contains(x, y, exact = TRUE, values = FALSE))
#'
setGeneric("contains",
  function(object, other, ...) standardGeneric("contains"))

setMethod("contains", c("list", "list"), function(object, other,
    values = TRUE, exact = FALSE, ...) {
  query.keys <- names(other)
  if (length(query.keys) == 0L && length(other) > 0L)
    return(FALSE)
  found <- match(query.keys, names(object), incomparables = "")
  if (anyNA(found))
    return(FALSE)
  for (idx in seq_along(query.keys)) {
    query.subset <- other[[idx]]
    data.subset <- object[[found[idx]]]
    result <- if (is.list(query.subset)) {
      if (is.list(data.subset))
        Recall(object = data.subset, other = query.subset, values = values,
          exact = exact, ...)
      else if (values)
        FALSE
      else
        is.null(names(query.subset))
    } else if (values) {
      if (exact)
        identical(x = data.subset, y = query.subset, ...)
      else
        all(data.subset %in% query.subset)
    } else
      TRUE
    if (!result)
      return(FALSE)
  }
  TRUE
}, sealed = SEALED)


################################################################################


# Used by map_values, character/numeric method and factor/numeric method.
adist2map <- function(x, max.distance = 0.1, ignore.case = TRUE,
    exclude = "", partial = FALSE, useBytes = FALSE, ...) {
  single_linkage <- function(x) {
    result <- seq_len(nrow(x))
    for (i in result) {
      j <- result %in% result[x[i, ]]
      result[j] <- max(result[j])
    }
    result
  }
  complete_linkage <- function(x) {
    result <- seq_len(nrow(x))
    for (i in rev.default(result[-1L])) {
      group <- result == result[[i]]
      for (j in rev.default(which(x[i, seq_len(i - 1L)])))
        if (all(x[j, group])) {
          result[[j]] <- result[[i]]
          group[[j]] <- TRUE
        }
    }
    result
  }
  if (nzchar(exclude))
    x <- grep(exclude, x, FALSE, TRUE, TRUE, FALSE, useBytes, TRUE)
  if (!length(x))
    return(structure(character(), names = character()))
  s <- table(x[!is.na(x) & nzchar(x)])
  s <- s[order(s, nchar(names(s)))]
  d <- adist(x = names(s), y = NULL, ignore.case = ignore.case,
    useBytes = useBytes, partial = partial, ...)
  n <- nchar(names(s), if (useBytes) "bytes" else "chars")
  f <- if (partial) pmin else pmax
  for (i in seq_len(nrow(d)))
    d[i, ] <- d[i, ] / f(n[[i]], n)
  f <- if (partial) complete_linkage else single_linkage
  d <- f(d <= max.distance)
  n <- names(s)
  n[seq_along(d)] <- n[d]
  names(n) <- names(s)
  n[names(n) != n]
}


