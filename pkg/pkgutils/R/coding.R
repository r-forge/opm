case <- function(EXPR, ...) UseMethod("case")

case.numeric <- function(EXPR, ...) {
  stopifnot(EXPR >= 0L, nargs() > 1L)
  switch(EXPR = pmin(EXPR, nargs() - 2L) + 1L, ...)
}

case.character <- function(EXPR, ...) {
  switch(EXPR = EXPR, ..., stop("unmatched 'EXPR' value"))
}

must <- function(expr, msg = NULL, ..., domain = NULL) {
  # For some reason, using stop() directly results in errors that cannot be
  # catched with tryCatch() any more.
  tryCatch(expr = expr, warning = function(w) stop(if (length(msg))
    msg
  else
    conditionMessage(w), call. = FALSE, domain = domain), ...)
}

L <- function(x, .wanted = 1L, .msg = "need object '%s' of length %i",
    .domain = NULL) {
  if (identical(length(x), .wanted))
    return(x)
  stop(sprintf(.msg, deparse(match.call()$x), .wanted), call. = FALSE,
    domain = .domain)
}

LL <- function(..., .wanted = 1L, .msg = "need object '%s' of length %i",
    .domain = NULL) {
  arg.names <- as.character(match.call())[-1L][seq_along(items <- list(...))]
  invisible(mapply(function(item, name) {
    if (!identical(length(item), .wanted))
      stop(sprintf(.msg, name, .wanted), call. = FALSE, domain = .domain)
    name
  }, items, arg.names, SIMPLIFY = TRUE, USE.NAMES = FALSE))
}

listing <- function(x, ...) UseMethod("listing")

listing.double <- function(x, ...) {
  x <- signif(x, getOption("digits"))
  mode(x) <- "character"
  listing.character(x, ...)
}

listing.factor <- function(x, ...) {
  y <- as.character(x)
  names(y) <- names(x)
  listing.character(y, ...)
}

listing.default <- function(x, ...) {
  listing(c(unclass(x)), ...)
}

listing.list <- function(x, ...) {
  listing(unlist(x), ...)
}

listing.character <- function(x, header = NULL, footer = NULL, prepend = FALSE,
    style = "list", collapse = if (style == "sentence")
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
    is_macro <- function(x) grepl("^[A-Za-z_]\\w*$", x, perl = TRUE)
    do_quote <- function(x, single) {
      x <- chartr("`", "'", x)
      if (single)
        sprintf("`%s'", gsub("'", "''`", x, fixed = TRUE))
      else
        sprintf("``%s''", gsub("'", "'''``", x, fixed = TRUE))
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
}

flatten <- function(object, ...) UseMethod("flatten")

flatten.default <- function(object, ...) {
  if (is.atomic(object))
    return(object)
  stop("need atomic 'object' (or specific 'flatten' method)")
}

flatten.list <- function(object, use.names = TRUE, ...) {
  while (any(is.a.list <- vapply(object, is.list, NA))) {
    object[!is.a.list] <- lapply(object[!is.a.list], list)
    object <- unlist(object, FALSE, use.names)
  }
  object
}

collect <- function(x, what, ...) UseMethod("collect")

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

