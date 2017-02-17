################################################################################


#' Make assertions
#'
#' A function similar to \code{stopifnot} reporting more details on vector
#' elements.
#'
#' @param cond A logical vector, character scalar or function. If a character
#'   scalar, converted to a function with \code{match.fun}. If a function,
#'   \code{orig} is passed to it as its first argument. The function should
#'   return a logical vector of the length of \code{orig}. Contained \code{NA}
#'   values, if any, are replaced by \code{FALSE}.
#' @param orig Mandatory if \code{cond} is (the name of) a function, otherwise
#'   ignored when empty or missing. Otherwise a vector of the length of
#'   \code{cond} if \code{cond} is a logical vector, or the single argument of
#'   \code{cond} if it is (the name of) a function.
#' @param msg When empty or missing, an error message is constructed
#'   automatically in the case of failure. Otherwise either a \code{sprintf}
#'   template to be applied to the failing elements of \code{orig}, or a
#'   character scalar directly providing the error message.
#' @param quiet Logical scalar determining the type of output in case of success
#'   (i.e., all values of \code{cond} are \code{TRUE}) or failure. \describe{
#'   \item{FALSE}{\code{TRUE} is returned when successful, an error is raised
#'   otherwise.}
#'   \item{NA}{\code{cond} is returned or the resulting logical vector when
#'   \code{cond} is (the name of) a function. In case of failure, a warning is
#'   issued. This can be used to drop parts of objects such as data frames,
#'   with a warning.}
#'   \item{TRUE}{A character vector with the descriptions of the problems is
#'   returned. This vector is empty in case of success.}
#'   }
#' @param ... Optional arguments passed to \code{cond} when it is (the name of)
#'   a function.
#' @return The type of return value depends on the values of \code{quiet} and
#'   \code{cond}.
#' @details Compared to \code{stopifnot} this function can only conduct a test
#'   on a single object but can report element-specific details of failures.
#' @export
#' @seealso base::stopifnot base::match.fun base::sprintf
#' @family coding-functions
#' @keywords utilities
#' @examples
#' stopifnot(assert(function(x) x > 0, 1:10))
#' (x <- try(assert(function(x) x > 0, -1:8), TRUE))
#' stopifnot(inherits(x, "try-error"))
#'
assert <- function(cond, orig, msg, quiet = FALSE, ...) {
  if (is.character(cond)) {
    if (missing(msg) || !length(msg))
      msg <- sprintf("assertion '%s' failed for '%%s'", cond)
    cond <- match.fun(cond)(orig, ...)
  } else if (is.function(cond)) {
    if (missing(msg) || !length(msg))
      msg <- sprintf("assertion '%s' failed for '%%s'",
        deparse(match.call()$cond))
    cond <- cond(orig, ...)
  }
  if (!anyNA(cond) && all(cond))
    return(if (is.na(quiet))
        cond
      else if (quiet)
        character()
      else
        TRUE)
  cond[is.na(cond)] <- FALSE
  if (missing(msg) || !length(msg)) {
    msg <- paste0("assertion '", deparse(match.call()$cond), "' failed")
    msg <- if (missing(orig) || !length(orig))
        sprintf(paste0(msg, " in %i of %i cases"), sum(!cond), length(cond))
      else
        paste0(msg, " for ", orig[!cond])
  } else if (missing(orig) || !length(orig)) {
    msg <- sprintf(paste0(msg, " in %i of %i cases"), sum(!cond), length(cond))
  } else {
    msg <- sprintf(msg, orig[!cond])
  }
  if (is.na(quiet)) {
    warning(paste0(msg, collapse = "\n"), call. = FALSE)
    cond
  } else if (quiet) {
    msg
  } else {
    stop(paste0(msg, collapse = "\n"), call. = FALSE)
  }
}


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
#' @method case double
#' @export
#'
case.double <- function(EXPR, ...) {
  case.integer(EXPR = as.integer(EXPR), ...)
}

#' @rdname case
#' @method case integer
#' @export
#'
case.integer <- function(EXPR, ...) {
  switch(EXPR = min(EXPR, nargs() - 2L) + 1L, ...)
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
  # caught with tryCatch() any more.
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
  invisible(mapply(FUN = function(item, name) {
    if (!identical(length(item), .wanted))
      stop(sprintf(.msg, name, .wanted), call. = FALSE, domain = .domain)
    name
  }, item = items, name = arg.names, USE.NAMES = FALSE))
}


################################################################################


#' Nicer message listings and flattening or expansion of objects
#'
#' Create some kind of listing, used, e.g., in (error) messages or warnings.
#' Alternatively, make an object \sQuote{flat}, such as by creating a non-nested
#' list from a list, or expand it after splitting certain components.
#'
#' @inheritParams pack_desc
#' @param object Usually a list. The default method just returns \code{object}
#'   if it is atomic but raises an error otherwise. For \code{unnest}, a data
#'   frame, list or character vector.
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
#' @param ... Optional other arguments passed to \code{formatDL}. For
#'   \code{unnest}, optional arguments passed to \code{data.frame}.
#' @param sep Character scalar passed as \code{split} argument to
#'   \code{strsplit}.
#' @param col Character vector with the names of the columns to be passed to
#'   \code{strsplit} for splitting.
#' @param fixed Logical scalar passed to \code{strsplit}.
#' @param stringsAsFactors Logical scalar passed to \code{data.frame}.
#' @return Character scalar in the case of \code{listing}, data frame in the
#'   case of \code{unnest}.
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
    x <- structure(.Data = names(x), names = x)
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

#= unnest listing

#' @rdname listing
#' @export
#'
setGeneric("unnest", function(object, ...) standardGeneric("unnest"))

setMethod("unnest", "data.frame", function(object, sep, col = names(object),
    fixed = TRUE, ..., stringsAsFactors = FALSE) {
  x <- lapply(object[, col, drop = FALSE], strsplit, sep, fixed, !fixed)
  x <- as.data.frame(do.call(cbind, x)) # yields columns of type 'list'
  x <- cbind(object[, setdiff(names(object), col), drop = FALSE], x)
  col <- names(x)
  args <- list(check.names = FALSE, stringsAsFactors = stringsAsFactors, ...)
  x <- lapply(seq.int(nrow(x)),
    function(i) do.call(data.frame, c(x[i, , drop = FALSE], args)))
  for (i in seq_along(x))
    colnames(x[[i]]) <- col
  do.call(rbind, x)
}, sealed = SEALED)

setMethod("unnest", "list", function(object, ..., stringsAsFactors = FALSE) {
  id <- mapply(FUN = rep.int, x = seq_along(object), times = lengths(object),
    SIMPLIFY = FALSE, USE.NAMES = FALSE)
  x <- unlist(object, FALSE, FALSE)
  x <- data.frame(ID = unlist(id, FALSE, FALSE),
    X = if (is.list(x)) I(x) else x, ..., stringsAsFactors = stringsAsFactors)
  attr(x, "total") <- length(object)
  x
}, sealed = SEALED)

setMethod("unnest", "character", function(object, sep, fixed = TRUE, ...,
    stringsAsFactors = FALSE) {
  unnest(object = strsplit(object, sep, fixed, !fixed), ...,
    stringsAsFactors = stringsAsFactors)
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
#'   \item{rows}{Like \code{datasets}, but all rows are kept. This is like
#'   \code{rbind} from the \pkg{base} package but it also augments missing
#'   columns where necessary.}
#'   }
#'   The matrix method currently only supports \code{columns}, which means
#'   assorting the values to the columns anew based on the majority of their
#'   occurrences, and \code{rows}, which does the same for the rows. This can
#'   be used to clean up messy data.
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
#' @seealso base::unlist base::as.data.frame base::rbind
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
    what = c("counts", "occurrences", "values", "elements", "datasets", "rows"),
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
      size <- lengths(x, FALSE)
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
      stringsAsFactors, verbose, all.rows, ...) {
    unfactor <- function(x) {
      for (i in which(vapply(x, is.factor, NA)))
        x[, i] <- as.character(x[, i])
      x
    }
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
    add_columns <- function(x, wanted) {
      if (length(n <- setdiff(wanted, colnames(x))))
        x <- cbind(x, matrix(NA, nrow(x), length(n), FALSE, list(NULL, n)))
      x[, wanted, drop = FALSE]
    }
    if (all.atomic <- all(vapply(x, is.atomic, NA))) {
      x <- lapply(x, as.matrix)
      if (keep.unnamed)
        x <- lapply(x, enforce_names)
      else if (verbose)
        x <- lapply(x, keep_validly_named_only_but_complain)
      else
        x <- lapply(x, keep_validly_named_only)
    } else {
      x <- lapply(lapply(X = x, FUN = data.frame, stringsAsFactors = FALSE,
        check.names = !optional), unfactor)
    }
    if (all.rows) {
      cn <- unique.default(unlist(lapply(x, colnames), FALSE, FALSE))
      result <- do.call(rbind, lapply(x, add_columns, cn))
      if (is.null(result))
        result <- matrix(NA, 0L, length(cn), FALSE, list(NULL, cn))
    } else {
      rn <- sort.int(unique.default(unlist(lapply(x, rownames), FALSE, FALSE)))
      cn <- sort.int(unique.default(unlist(lapply(x, colnames), FALSE, FALSE)))
      result <- matrix(NA, length(rn), length(cn), FALSE, list(rn, cn))
      if (!all.atomic)
        result <- as.data.frame(result, stringsAsFactors = FALSE,
          optional = optional)
      for (mat in x)
        result[rownames(mat), colnames(mat)] <- mat
    }
    if (dataframe) {
      if (all.atomic || !is.data.frame(result))
        result <- as.data.frame(result)
      if (stringsAsFactors)
        for (i in which(vapply(result, is.character, NA)))
          result[, i] <- as.factor(result[, i])
    } else if (!all.atomic || is.data.frame(result)) {
      result <- as.matrix(result)
    }
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
      stringsAsFactors, verbose, FALSE, ...),
    rows = collect_matrices(x, TRUE, dataframe, optional,
      stringsAsFactors, verbose, TRUE, ...)
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
#'   numeric scalar, list, \code{NULL} or missing.
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
#'   \item If \code{mapping} is a list and \code{object} is a data frame, the
#'   names of \code{mapping} specify the columns of \code{object} to map. Only
#'   factors and character vectors within \code{object} are modified, and it is
#'   an error if \code{mapping} does not list all of them, or has no names at
#'   all. Three potential names of \code{mapping} are special. \code{.} must
#'   refer to a named character vector and is used for mapping the column names
#'   of \code{object}. \code{_}, containing a named list, is used for adding
#'   columns. \code{/} pointing to a character vector provides a set of columns
#'   to delete. \code{-} provides a named list for deleting rows; the names are
#'   the names of the columns and the values the values within that column that
#'   indicate the rows to delete. Behaviour is special when \code{mapping} is
#'   an empty list; in that case, a template for a real mapping list is
#'   generated.
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
    x[lengths(x, FALSE) > 0L]
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
  map_values(rapply(object, mapfun, classes))
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

setMethod("map_values", c("data.frame", "list"), function(object, mapping) {

  can.map <- vapply(object, is.character, NA) | vapply(object, is.factor, NA)

  if (!length(mapping)) { # template-ceation mode
    mapping <- vector("list", sum(can.map))
    mapping[] <- list(structure(.Data = character(), names = character()))
    names(mapping) <- names(object)[can.map]
    return(mapping)
  }

  if (is.null(names(mapping)))
    stop("unnamed list used as 'mapping' argument")

  if (pos <- match(".", names(mapping), 0L)) { # mapping of column names
    names(object) <- map_values(names(object), unlist(mapping[[pos]]))
    mapping <- mapping[-pos]
  }

  if (pos <- match("_", names(mapping), 0L)) { # addition of columns
    add <- mapping[[pos]]
    mapping <- mapping[-pos]
  } else {
    add <- NULL
  }

  if (pos <- match("-", names(mapping), 0L)) { # deletion of rows
    delete <- mapping[[pos]] # must refer to new column names
    mapping <- mapping[-pos]
  } else {
    delete <- NULL
  }

  if (pos <- match("/", names(mapping), 0L)) { # deletion of columns
    remove <- mapping[[pos]] # must refer to new column names
    mapping <- mapping[-pos]
  } else {
    remove <- NULL
  }
  if (length(remove)) { # actually delete columns
    pos <- match(remove, colnames(object), 0L)
    pos <- -pos[pos > 0L]
    can.map <- can.map[pos]
    object <- object[, pos, drop = FALSE]
  }

  wanted <- names(object)[can.map]
  pos <- match(wanted, names(mapping), 0L)
  assert(pos > 0L, wanted, "column name '%s' missing in mapping", FALSE)

  for (i in seq_along(wanted)) # map remaining columns
    object[, wanted[[i]]] <- map_values(object[, wanted[[i]]],
      unlist(mapping[[pos[[i]]]]))

  if (length(delete)) # actually delete rows
    for (name in names(delete))
      if (any(pos <- object[, name] %in% delete[[name]]))
        object <- object[!pos, , drop = FALSE]
  if (length(add)) # add columns with constant default value
    object <- cbind(object, as.data.frame(add))

  object

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
  structure(.Data = object, names = object)
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
      return(lapply(item, map_names_recursively))
    }
    item
  }
  map_names_recursively(object)
}, sealed = SEALED)

setMethod("map_names", c("list", "character"), function(object, mapping) {
  map_names_recursively <- function(item) {
    if (is.list(item)) {
      names(item) <- map_values(names(item), mapping)
      return(lapply(item, map_names_recursively))
    }
    item
  }
  map_names_recursively(object)
}, sealed = SEALED)

setMethod("map_names", c("list", "missing"), function(object) {
  get_names_recursively <- function(item) {
    if (is.list(item))
      c(names(item), unlist(lapply(item, get_names_recursively)))
    else
      character()
  }
  map_values(get_names_recursively(object))
}, sealed = SEALED)

#-------------------------------------------------------------------------------

setMethod("map_names", c("data.frame", "function"), function(object, mapping,
    ...) {
  names(object) <- map_values(names(object), mapping, ...)
  object
}, sealed = SEALED)

setMethod("map_names", c("data.frame", "character"), function(object, mapping) {
  names(object) <- map_values(names(object), mapping)
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
#' One use of \code{contains} is to test whether all names of a query list occur
#' as names in a data list and optionally also whether they point to the same
#' elements; this principle is applied recursively to all contained lists. The
#' \code{check} methods apply various tests to objects.
#'
#' @param object List or data frame containing the data, or character vector
#'   describing problems, if any.
#' @param against Character vector whose names indicate column names of
#'   \code{object} and whose values indicate types or classes to assert.
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
#' @return \code{contains} yields a logical scalar, \code{check} a (potentially
#'   empty) character vector describing each failed assertion when \code{object}
#'   is a list.
#' @details  Non-list elements are ignored by \code{contains} if \code{values}
#'   is \code{FALSE}. Otherwise the comparison is done using \code{identical} if
#'   \code{exact} is \code{TRUE}. If \code{exact} is \code{FALSE}, the value(s)
#'   in the data list can be any of the values at the corresponding position in
#'   the query list, and the comparison is done by coercion to character
#'   vectors. An empty query list results in \code{TRUE}. Missing names in a
#'   non-empty query list result in \code{FALSE}.
#'
#'   The \code{check} method for data frames tests for the presence of each
#'   column listed by \code{against} in \code{object}. For the columns found,
#'   it checks whether \code{is.<name>} returns \code{TRUE}, which \code{<name>}
#'   given by the according element of \code{against}. It is an error if the
#'   function \code{is.<name>} does not exist.
#'
#'   The \code{check} method for character vectors is a simple helper method
#'   that raises an error unless the vector is empty.
#' @family coding-functions
#' @seealso base::list base::as.list base::`[` base::`[[` base::match
#' @seealso base::identity
#' @keywords attribute list
#' @examples
#'
#' # contains() list/list method
#' x <- list(a = 1:8, c = 9, d = list(d1 = 'x', d2 = 'y'))
#' y <- list(a = 1:10, c = "9", d = list(d1 = "x"))
#' stopifnot(contains(x, y))
#' stopifnot(!contains(x, y, exact = TRUE))
#' stopifnot(contains(x, y, exact = TRUE, values = FALSE))
#'
#' # check() data.frame/character method
#' (x <- check(Puromycin,
#'   c(conc = "numeric", rate = "numeric", state = "factor")))
#' (y <- check(Puromycin,
#'   c(missing = "numeric", rate = "numeric", state = "character")))
#' stopifnot(is.character(x), is.character(y))
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

#= check contains

#' @rdname contains
#' @export
#'
setGeneric("check", function(object, against, ...) standardGeneric("check"))

setMethod("check", c("list", "character"), function(object, against) {
  # additional tests
  is.available <- function(x) !anyNA(x)
  is.nonempty <- function(x) !is.character(x) ||
    all(nzchar(x, TRUE), na.rm = TRUE)
  is.unique <- function(x) !anyDuplicated.default(x[!is.na(x)])
  is.positive <- function(x) is.numeric(x) && all(x > 0, na.rm = TRUE)
  is.natural <- function(x) is.numeric(x) && all(x >= 0, na.rm = TRUE)
  # main part
  element_is <- function(x, name, checkfun) checkfun(x[[name]])
  ok <- names(against) %in% names(object)
  result <- sprintf("element '%s' is missing", names(against)[!ok])
  against <- against[ok]
  ok <- mapply(checkfun = lapply(sprintf("is.%s", against), match.fun),
    FUN = element_is, name = names(against), MoreArgs = list(x = object))
  c(result, sprintf("element '%s' fails test 'is.%s'",
    names(against)[!ok], against[!ok]))
}, sealed = SEALED)

setMethod("check", c("character", "missing"), function(object, against) {
  if (length(object))
    stop(paste0(object, collapse = "\n"))
  invisible(TRUE)
}, sealed = SEALED)


################################################################################


#' Partial matches
#'
#' Helper function to collect partial (or complete) matches of (Perl-compatible)
#' regular expressions.
#'
#' @param x Character vector. If it has names, these names will be used as row
#'   names or vector names in the output.
#' @param pattern Character scalar with regular expression that defines one to
#'   several capturing groups (partial matches) in parentheses. If they are
#'   named, these names will be used as column names in the output.
#'   Alternatively, if no capturing groups are present and thus no partial
#'   matches occur, the complete matches are returned as vector.
#' @param ignore.case Logical scalar passed to \code{regexpr}.
#' @return If capturing groups are used, a matrix of mode \sQuote{character}
#'   with the number of rows equal to the length of \code{x} and one column per
#'   capturing group. If these were named, their names appear as column names.
#'   If no capturing groups were present, a character vector of the same length
#'   as \code{x} is returned, containing the complete matches, if any.
#'   Non-matches are always represented as \code{NA}.
#' @details For discarding regular expression groups in the output, use
#'   non-capturing groups such as \code{(?:left|right)}.
#' @family coding
#' @seealso base::regmatches
#' @export
#' @keywords character
#' @examples
#' x <- structure(letters, names = LETTERS)
#' (m <- match_parts(x, "(?<pos1>.)"))
#' stopifnot(m == letters,
#'   colnames(m) == "pos1", rownames(m) == LETTERS)
#'
match_parts <- function(x, pattern, ignore.case = FALSE) {
  m <- regexpr(pattern, x, ignore.case, TRUE)
  f <- m > 0L
  if (is.null(attr(m, "capture.start"))) {
    result <- rep.int(NA_character_, length(x))
    result[f] <- substr(x[f], m[f], m[f] + attr(m, "match.length")[f] - 1L)
    names(result) <- names(x)
    return(result)
  }
  cs <- attr(m, "capture.start")[f, , drop = FALSE]
  cl <- attr(m, "capture.length")[f, , drop = FALSE]
  result <- matrix(NA_character_, length(x), ncol(cs), FALSE,
    list(names(x), attr(m, "capture.names")))
  for (i in seq_len(ncol(result)))
    result[f, i] <- substr(x[f], cs[, i], cs[, i] + cl[, i] - 1L)
  result
}


################################################################################


#' Assign values only when needed
#'
#' A helper function for avoiding long-running computations if the object to be
#' generated is either already present or can be read from an \code{Rda} file.
#'
#' @param name Character scalar indicating the name of an object to be assigned.
#' @param expr Expression that will only be evaluated if \code{name} does not
#'   yet exist and, optionally, cannot be read from an \code{Rda} file.
#' @param template Character scalar with a template as used by \code{sprintf}
#'   from the base package, with the placeholder, if any, referring to
#'   \code{name}. Set this to an empty object to turn of the \code{Rda} file
#'   mechanism.
#' @param env Passed as \code{envir} argument to \code{assign}.
#' @param inherits Passed as \code{inherits} argument to \code{assign}.
#' @return An invisible returned integer code indicating what has been done.
#' \describe{
#' \item{0}{The object already existed, nothing was done.}
#' \item{1}{The result of \code{expr} was assigned to the object, the \code{Rda}
#' file mechanism was not used.}
#' \item{2}{The given \code{Rda} file was found and its content assigned to the
#' object.}
#' \item{3}{The given \code{Rda} file was not found. The result of \code{expr}
#' was assigned to the object and was also stored in the given \code{Rda} file.}
#' }
#' @details If the \code{Rda} file name has no directory component, it is
#'   assumed to be located in the directory given by
#'   \code{getOption("rda_store")} and, if this does not exist, in the directory
#'   given by \code{getwd()}.
#' @export
#' @family coding-functions
#' @keywords IO utilities
#' @seealso base::assign base::readRDS base::saveRDS
#' @examples
#' the_answer <- function() {
#'   print("answer requested")
#'   42L
#' }
#' set("answer", the_answer(), NULL) # prints 'answer requested'
#' set("answer", the_answer(), NULL) # does not print
#' answer # 42
#'
set <- function(name, expr, template = "%s.Rda", env = parent.frame(),
    inherits = TRUE) {
  if (exists(name, NULL, env, NULL, "any", inherits))
    return(invisible(0L))
  if (!length(template)) {
    assign(name, expr, NULL, env, inherits)
    return(invisible(1L))
  }
  if (dirname(rda <- sprintf(template, name)) == ".")
    rda <- file.path(getOption("rda_store", getwd()), rda)
  if (file.exists(rda)) {
    assign(name, readRDS(rda), NULL, env, inherits)
    return(invisible(2L))
  }
  result <- expr
  saveRDS(result, rda)
  assign(name, result, NULL, env, inherits)
  invisible(3L)
}


################################################################################


#' Create \acronym{SQL} \code{SELECT} and \code{UPDATE} statements
#'
#' Helper function converting \R code into \acronym{SQL} \code{SELECT}
#' statements and data frames into \code{UPDATE} statements.
#'
#' @param x Data frame or formula.
#' @param where Character vector giving the name of the data frame and database
#'   table columns used to select rows.
#' @param table Character scalar indicating the name of the database table
#'   to be updated.
#' @param set Character vector giving the name of the data frame and database
#'   table columns to be updated.
#' @param ... Optional arguments passed between methods.
#' @return Character vector.
#' @details The formula method saves some typing, particularly in the case of
#'   complex queries, but it does not support joins. \R operators are mostly
#'   directly translated except for those with the highest precedence. Infix
#'   operators are translated literally. The control structures \code{if} and
#'   \code{function} yield \code{CASE} constructs.
#'
#'   To use the data frame method to update a column, say, \code{"x"} that is
#'   also used to select rows, include \code{"x"} in the \code{where} argument
#'   and \code{"new.x"} in the \code{update} argument.
#' @export
#' @family coding-functions
#' @keywords character database
#' @examples
#'
#' ## formula method
#' x <- mytable(a, b, if (c1 > 15 | c2 == NULL) c1 else c2) ~
#'   b < 69 & a %in% {"x"
#'     "y"}
#' (y <- sql(x))
#' stopifnot(is.character(y), length(y) == 1L)
#'
sql <- function(x, ...) UseMethod("sql")

#' @rdname sql
#' @method sql data.frame
#' @export
#'
sql.data.frame <- function(x, where, table, set = setdiff(colnames(x), where),
    ...) {

  dquote <- function(x) {
    ifelse(grepl("^[a-z][a-z_0-9]+$", x, FALSE, TRUE), x,
      sprintf('"%s"', gsub('"', '""', x, FALSE, FALSE, TRUE)))
  }

  sql_array <- function(x) {
    if (is.list(x))
      x <- vapply(x, sql_array, "")
    else if (is.character(x))
      x <- ifelse(is.na(x), "NULL",
        sprintf('"%s"', gsub('"', '\"', x, FALSE, FALSE, TRUE)))
    else if (is.atomic(x))
      x <- ifelse(is.na(x), "NULL", as.character(x))
    else
      stop("conversion of mode '", typeof(x), "' is not implemented")
    sprintf("{%s}", paste0(x, collapse = ","))
  }

  squote <- function(x, modify) {
    if (is.character(x))
      return(ifelse(is.na(x), "NULL", sprintf("'%s'",
        gsub("'", "''", x, FALSE, FALSE, TRUE))))
    if (is.atomic(x))
      return(ifelse(is.na(x), "NULL", x))
    if (is.list(x)) {
      if (modify)
        return(sprintf("'%s'", vapply(x, sql_array, "")))
      x <- lapply(x, squote)
      x <- vapply(X = x, FUN = paste0, FUN.VALUE = "", collapse = ", ")
      x <- sprintf("(%s)", ifelse(nzchar(x), x, "NULL"))
      attr(x, "list") <- TRUE
      return(x)
    }
    stop("conversion of mode '", typeof(x), "' is not implemented")
  }

  equals <- function(what, value, modify) {
    value <- squote(value, modify)
    sep <- if (modify)
        "="
      else if (isTRUE(attr(value, "list")))
        "IN"
      else
        ifelse(value == "NULL", "IS", "=")
    paste(what, sep, value)
  }

  join <- function(x, modify) {
    if (!ncol(x))
      stop("no columns chosen for ", if (modify) "update" else "selection")
    x <- mapply(FUN = equals, what = dquote(colnames(x)), value = x,
      MoreArgs = list(modify = modify), SIMPLIFY = FALSE, USE.NAMES = FALSE)
    do.call(paste, c(x, list(sep = if (modify) ", " else " AND ")))
  }

  create_map <- function(x) {
    x <- x[grepl("^new\\.(?<=.)", x, FALSE, TRUE) &
      match(substr(x, 5L, nchar(x)), x, 0L)]
    structure(.Data = substr(x, 5L, nchar(x)), names = x)
  }

  for (i in which(vapply(x, is.factor, NA)))
    x[, i] <- as.character(x[, i])

  map <- create_map(colnames(x))

  sprintf("UPDATE %s SET %s WHERE %s;", dquote(table),
    join(x[, set, drop = FALSE], TRUE),
    join(map_names(x[, where, drop = FALSE], map), FALSE))

}

#' @rdname sql
#' @method sql formula
#' @export
#'
sql.formula <- function(x, ...) {

  double_quote <- function(x) {
    if (any(bad <- !grepl("^[a-z][a-z_0-9]*$", x, FALSE, TRUE)))
      x[bad] <- sprintf('"%s"', gsub('"', '""', x[bad], FALSE, FALSE, TRUE))
    x
  }

  identifier_or_literal <- function(x) {
    if (is.character(x))
      return(sprintf("'%s'", gsub("'", "''", x, FALSE, FALSE, TRUE)))
    if (is.null(x))
      return("NULL")
    if (is.atomic(x))
      return(as.character(x))
    if (!is.name(x))
      stop("expected symbol or atomic vector, got ", typeof(x))
    double_quote(as.character(x))
  }

  operator <- function(x) {
    infix_operator <- function(x)
      if (grepl("^[a-z]+(\\s+[a-z]+)*$", x, TRUE, TRUE))
        toupper(x)
      else if (grepl("^[!#%&*+/<=>?@^|~`-]+$", x, FALSE, TRUE) &&
          !grepl("--|/[*]", x, FALSE, TRUE) &&
          !grepl("[^~!@#%^&|`?][+-]$", x, FALSE, TRUE))
        x
      else
        stop("invalid infix operator ", x)
    switch(x,
      # high-precedence operators that get another meaning
      `:::` =, `::` =, `@` =, `$` = x,
      # high-precedence operators with similar meaning in PostgreSQL
      `:` =, `(` =, `{` =, `[` =, `[[` =, `^` =, `-` =, `+` =, `*` =, `/` =,
        `<` =, `>` =, `<=` =, `>=` =, `!=` =, `||` =, `&&` = x,
      # equality, as it is likely to be used in R code
      `==` = "=",
      # logical operators
      `!` = "NOT", `&` = "AND", `|` = "OR",
      # low-precedence operators unlikely to be used but possible
      `~` =, `->` =, `<-` =, `->>` =, `<<-` =, `=` =, `?` = x,
      # reserved words that are kept
      `function` =, `if` = x,
      # infix operators enclosed in '%'
      `%%` = "%",
      if (grepl("^%.+%$", x, FALSE, TRUE))
        infix_operator(substr(x, 2L, nchar(x) - 1L))
      else
        NULL
    )
  }

  rec_sql <- function(x) {

    convert_pairlist <- function(x) {
      if (bad <- match("...", names(x), 0L)) {
        warning("removing '...' argument")
        x <- x[-bad]
      }
      if (!length(x))
        return(NULL)
      keys <- double_quote(names(x))
      if (any(present <- vapply(x, typeof, "") != "symbol" | nzchar(x))) {
        groups <- unclass(rev.default(pkgutils::sections(rev.default(present))))
        groups[is.na(groups)] <- seq_along(which(is.na(groups))) +
          max(groups, na.rm = TRUE)
        keys <- split.default(keys, match(groups, groups))
        keys <- vapply(keys, paste0, "", collapse = " OR ")
        values <- unlist(lapply(x[present], rec_sql), FALSE, FALSE)
        if (length(keys) > length(values))
          values <- c(values, "NULL")
      } else {
        keys <- paste0(keys, collapse = " OR ")
        values <- "NULL"
      }
      paste0(sprintf("WHEN %s THEN %s", keys, values), collapse = " ")
    }

    join <- function(x) paste0(unlist(x, FALSE, FALSE), collapse = ", ")

    named_join <- function(x, n) paste0(ifelse(nzchar(n), sprintf("%s := ",
      double_quote(n)), n), unlist(x, FALSE, FALSE), collapse = ", ")

    if (!is.call(x))
      return(identifier_or_literal(x))

    if (is.null(op <- operator(as.character(x[[1L]]))))
      return(sprintf("%s(%s)", rec_sql(x[[1L]]),
        if (is.null(names(x)))
          join(lapply(x[-1L], rec_sql))
        else
          named_join(lapply(x[-1L], rec_sql), names(x)[-1L])))

    switch(op,
      `if` = if (length(x) > 3L)
        sprintf("CASE WHEN %s THEN %s ELSE %s END", rec_sql(x[[2L]]),
          rec_sql(x[[3L]]), rec_sql(x[[4L]]))
      else
        sprintf("CASE WHEN %s THEN %s END", rec_sql(x[[2L]]), rec_sql(x[[3L]])),
      `function` = if (is.null(args <- convert_pairlist(x[[2L]])))
          rec_sql(x[[3L]])
        else
          sprintf("CASE %s ELSE %s END", args, rec_sql(x[[3L]])),
      `::` = sprintf("%s.%s", rec_sql(x[[2L]]), rec_sql(x[[3L]])),
      `:::` = sprintf("(%s).%s", rec_sql(x[[2L]]), rec_sql(x[[3L]])),
      `$` = sprintf("%s :: %s", rec_sql(x[[2L]]), rec_sql(x[[3L]])),
      `@` = sprintf("CAST(%s AS %s)", rec_sql(x[[2L]]), rec_sql(x[[3L]])),
      `(` = sprintf("(%s)", rec_sql(x[[2L]])), # always only one argument
      `{` = if (length(x) > 1L)
        sprintf("(%s)", join(lapply(x[-1L], rec_sql)))
      else
        "NULL",
      `[` = sprintf("%s[%s]", rec_sql(x[[2L]]),
        join(lapply(x[-c(1L, 2L)], rec_sql))),
      `[[` = sprintf("%s[[%s]]", rec_sql(x[[2L]]),
        join(lapply(x[-c(1L, 2L)], rec_sql))),
      if (length(x) > 2L) {
        if (identical(right <- rec_sql(x[[3L]]), "NULL"))
          switch(op, `=` = op <- "IS", `!=` = op <- "IS NOT",
            warning("NULL on right side of operator ", op))
        sprintf("%s %s %s", rec_sql(x[[2L]]), op, right)
      } else {
        sprintf("%s %s", op, rec_sql(x[[2L]]))
      }
    )

  }

  selection <- function(x) {
    if (is.call(x)) {
      tablename <- identifier_or_literal(x[[1L]])
      if (length(x) > 1L) {
        columns <- unlist(lapply(x[-1L], rec_sql), FALSE, FALSE)
        map <- allNames(x)[-1L]
        map <- ifelse(nzchar(map), paste0(" AS ", double_quote(map)), map)
        columns <- paste0(columns, map, collapse = ", ")
      } else {
        columns <- "*"
      }
    } else {
      tablename <- identifier_or_literal(x)
      columns <- "*"
    }
    sprintf("SELECT %s FROM %s", columns, tablename)
  }

  if (length(x) > 2L)
    sprintf("%s WHERE %s;", selection(x[[2L]]), rec_sql(x[[3L]]))
  else
    sprintf("%s;", selection(x[[2L]]))
}


################################################################################

