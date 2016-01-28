

################################################################################
################################################################################
#
# Miscellaneous internal helper functions
#


#' Get data and memoize them
#'
#' Using queries and a function, search for information that is not already
#' stored, and store it in the hash table for memoization.
#'
#' @param x Character vector of query IDs. Empty and \code{NA} elements will
#'   not be used as query but yield \code{default}. All other elements are
#'   pased to \code{getfun} either together or element-by-element.
#' @param prefix Character scalar to prepend to the IDs for storing them in the
#'   hash table. Should be specific for this kind of data.
#' @param default R object to insert for each value of \code{x} that is either
#'   \code{NA} or empty or contains information but yields \code{NULL} values
#'   in the query result.
#' @param getfun Function used for obtaining results. If \code{single} is
#'   \code{TRUE} it should accept an element of \code{x} as first argument.
#'   Otherwise the entire vector \code{x} is the first argument, and the result
#'   must be a list of the same length than the passed subset of \code{x}. If
#'   it has names, they must be set-equal to the passed subset of \code{x}, as
#'   they will be used to order the query results using this subset of \code{x}.
#'   Irrespective of \code{single}, \code{NULL} elements must be used in the
#'   return values of \code{getfun} to indicate failure.
#' @param single Logical scalar modifying the use of \code{getfun}.
#' @param ... Optional arguments passed to \code{getfun}.
#' @return A list of the same length than \code{x}, with \code{x} as names.
#' @keywords internal
#'
get_and_remember <- function(x, prefix, default, getfun, single = FALSE, ...) {
  do_get <- function(x, envir, prefix, default, getfun, single, ...) {
    do_query <- function(x, single, getfun, ...) {
      if (single)
        return(lapply(X = x, FUN = getfun, ...))
      if (!is.list(result <- getfun(x, ...)))
        stop("'getfun' did not return a list")
      if (length(result) != length(x))
        stop("length discrepancy between 'getfun' result and query")
      if (is.null(names(result)))
        result
      else if (all(names(result) %in% x))
        result[x]
      else
        stop("naming discrepancy between 'getfun' result and query")
    }
    result <- vector("list", length(x))
    need <- !vapply(keys <- paste0(prefix, x), exists, NA, envir)
    result[!need] <- mget(keys[!need], envir)
    if (!any(need))
      return(result)
    result[need] <- do_query(x[need], single, getfun, ...)
    if (any(bad <- vapply(result[need], is.null, NA))) {
      warning(listing(x[need][bad], "could not find ", style = "sentence"))
      result[need][bad] <- rep.int(list(default), sum(bad))
    }
    list2env(structure(.Data = result[need][!bad], names = keys[need][!bad]),
      envir)
    result
  }
  if (!is.character(x))
    stop("'x' must be a character vector (of query IDs)")
  result <- vector("list", length(x))
  ok <- !is.na(x) & nzchar(x)
  result[!ok] <- rep.int(list(default), sum(!ok))
  result[ok] <- do_get(x[ok], MEMOIZED, prefix, default, getfun, single, ...)
  #result[ok] <- reassign_duplicates(x[ok], do_get, MEMOIZED, prefix,
  #  default, getfun, single, ...)
  names(result) <- x
  result
}


################################################################################


#' Pick rows
#'
#' Pick rows from a data frame if selected columns are identical to keys.
#'
#' @param object Data frame. At least two rows are needed.
#' @param selection Named list, keys should correspond to column names of
#'   \code{object}, values to one to several alternative values that should
#'   occur in the respective data-frame column.
#' @return Data frame.
#' @keywords internal
#'
setGeneric("pick_from", function(object, ...) standardGeneric("pick_from"))

setMethod("pick_from", "data.frame", function(object, selection) {
  matches <- lapply(names(selection), function(name) {
    m <- lapply(selection[[name]], `==`, object[, name])
    apply(do.call(cbind, m), 1L, any)
  })
  matches <- apply(do.call(cbind, matches), 1L, all)
  matches[is.na(matches)] <- FALSE # we get NA from all-NA rows
  object[matches, , drop = FALSE]
}, sealed = SEALED)


################################################################################


#' Helper methods for subsetting.
#'
#' These are helper methods for subsetting any kinds of \code{\link{OPMX}}
#' objects.
#'
#' @param x \code{\link{OPMX}} object.
#' @param query Query passed to the infix operators.
#' @param invert.1 Logical scalar.
#' @param invert.2 Logical scalar.
#' @param comb.fun Function applied to logical vectors.
#' @return \code{\link{OPMX}} object or \code{NULL}.
#' @keywords internal
#'
setGeneric("common_times", function(x) standardGeneric("common_times"))

setMethod("common_times", "OPM", function(x) {
  x
}, sealed = SEALED)

setMethod("common_times", "OPMS", function(x) {
  tp <- hours(x, what = "all")
  if (is.matrix(tp))
    tp <- lapply(seq_len(nrow(tp)), function(i) tp[i, ])
  if (length(maxs <- unique.default(vapply(tp, max, 1))) < 2L)
    return(x)
  min.max <- min(maxs)
  tp <- lapply(tp, function(x) which(x <= min.max))
  x[, tp]
}, sealed = SEALED)

#= select_by_disc common_times

setGeneric("select_by_disc", function(x, ...) standardGeneric("select_by_disc"))

setMethod("select_by_disc", "OPMD", function(x, invert.1, invert.2, comb.fun) {
  y <- discretized(x)
  if (invert.1)
    y <- !y
  y[is.na(y)] <- FALSE
  if (invert.2)
    y <- !y
  x[, y]
}, sealed = SEALED)

setMethod("select_by_disc", "OPMS", function(x, invert.1, invert.2, comb.fun) {
  y <- discretized(x)
  if (invert.1)
    y <- !y
  y[is.na(y)] <- FALSE
  y <- apply(y, 2L, comb.fun)
  if (invert.2)
    y <- !y
  x[, , y]
}, sealed = SEALED)

#= do_select common_times

setGeneric("do_select", function(x, query) standardGeneric("do_select"))

setMethod("do_select", "OPM", function(x, query) {
  if (query)
    x
  else
    NULL
}, sealed = SEALED)

setMethod("do_select", "OPMS", function(x, query) {
  x[query]
}, sealed = SEALED)


################################################################################


#' Reduce an object
#'
#' Reduce a countable object to the most frequent element(s). Alternatively,
#' join list to a matrix or data frame. \code{metadata2factorlist} is a helper
#' function for \code{link{split}}.
#'
#' @param x For \code{reduce_to_mode}, an \R object to which \code{table} can be
#'   applied. The matrix method reduces the columns. For
#'   \code{metadata2factorlist}, a \code{link{MOPMX}} object.
#' @param cutoff Numeric scalar. Relative frequency below which elements are
#'   discarded.
#' @param use.na Logical scalar indicating whether ambiguous results should be
#'   converted to \code{NA}.
#' @param how Character scalar indicating how to join the list. See
#'   \code{\link{aggr_settings}} for the values.
#' @return Vector of the same storage mode than \code{x}.
#' @keywords internal
#'
reduce_to_mode <- function(x, cutoff, use.na) UseMethod("reduce_to_mode")

#' @rdname reduce_to_mode
#' @method reduce_to_mode default
#' @export
#'
reduce_to_mode.default <- function(x, cutoff, use.na = TRUE) {
  counts <- table(x, useNA = "always")
  counts <- counts[counts >= length(x) * cutoff]
  result <- case(length(counts), NA_character_, names(counts), if (use.na)
    NA_character_
  else
    names(counts))
  storage.mode(result) <- storage.mode(x)
  result
}

#' @rdname reduce_to_mode
#' @method reduce_to_mode matrix
#' @export
#'
reduce_to_mode.matrix <- function(x, cutoff, use.na = TRUE) {
  apply(x, 2L, reduce_to_mode.default, cutoff, use.na)
}

#' @rdname reduce_to_mode
#'
list2matrix <- function(x, how = c("yaml", "json", "rcode")) {
  unlist_matrix <- function(x, fun, ...) {
    x <- do.call(rbind, x)
    if (typeof(x) != "list")
      return(x)
    if (!missing(fun)) {
      max.len <- apply(x, 2L, lengths, FALSE)
      if (is.matrix(max.len))
        max.len <- apply(max.len, 2L, max)
      for (i in which(max.len > 1L))
        x[, i] <- vapply(X = x[, i], FUN = fun, FUN.VALUE = "", ...)
    }
    storage.mode(x) <- "character"
    x
  }
  how <- tryCatch(expr = match.arg(how), error = function(e) how)
  switch(how,
    yaml = unlist_matrix(x, to_yaml, json = FALSE, listify = TRUE),
    json = unlist_matrix(x, to_yaml, json = TRUE, listify = TRUE),
    rcode = unlist_matrix(x),
    collect(x = x, what = how, dataframe = TRUE, stringsAsFactors = FALSE,
      optional = TRUE, keep.unnamed = TRUE, min.cov = 1L)
  )
}

#' @rdname reduce_to_mode
#'
sub_indexes <- function(x) {
  x <- lengths(x, TRUE)
  add <- c(0L, cumsum(x))
  x <- lapply(x, seq_len)
  for (i in seq_along(x)[-1L])
    x[[i]] <- x[[i]] + add[[i]]
  attr(x, "total") <- add[[length(add)]]
  x
}

#' @rdname reduce_to_mode
#'
simplify_conditionally <- function(x) {
  if (!length(x))
    return(NULL)
  if (any(vapply(x, is.list, NA)) || any(vapply(x, is.matrix, NA)))
    return(x)
  if (length(n <- unique.default(lengths(x, FALSE))) > 1L)
    return(x)
  if (n > 1L)
    do.call(rbind, x)
  else
    unlist(x, FALSE, TRUE)
}

#' @rdname reduce_to_mode
#'
close_index_gaps <- function(x) {
  if (any(bad <- vapply(x, is.null, NA))) {
    warning("closing gaps in indexes", call. = FALSE)
    return(x[!bad])
  }
  x
}

#' @rdname reduce_to_mode
#'
metadata2factorlist <- function(x, f) {
  replace_null <- function(x) {
    x[vapply(x, is.null, NA)] <- NA
    x
  }
  f <- metadata(x, f)
  f[simple] <- lapply(f[simple <- vapply(x, is, NA, "OPM")], list)
  f <- lapply(lapply(f, replace_null), lapply, replace_null)
  lapply(lapply(f, vapply, paste0, "", collapse = " "), as.factor)
}


################################################################################


## NOTE: not an S4 method because applicable to any objects

#' Check for uniformity
#'
#' Assess whether all elements in a collection are identical, or skip
#' calculations for duplicated elements but re-assign the results.
#'
#' @param x An \R object to which \code{duplicated} can be applied.
#' @param na.rm Logical scalar. Remove \code{NA} elements before determining
#'   uniformity?
#' @param FUN Function to be applied to the non-duplicated part of \code{x}.
#' @return Either \code{TRUE} or a vector of the class of \code{x} containing
#'   all deviating elements.
#' @keywords internal
#'
is_uniform <- function(x, na.rm = FALSE) {
  if (na.rm)
    x <- na.exclude(x)
  if (length(x) < 2L || all((dup <- duplicated(x))[-1L]))
    return(TRUE)
  x[!dup]
}

#' @rdname is_uniform
#'
reassign_duplicates <- function(x, FUN, ...) {
  # this requires non-NA values (and non-empty values in the case of strings)
  if (!any(dup <- duplicated.default(x)))
    return(FUN(x, ...))
  FUN(x[!dup], ...)[match(x, x[!dup])]
}


################################################################################


#' Check for constantness
#'
#' Assess whether all elements in a collection are identical. This uses
#' \code{duplicated} by default, but there is also an \sQuote{extended} mode for
#' list-like objects.
#'
#' @param x Vector, matrix, array or \sQuote{CMAT} object.
#' @param strict Logical scalar. Has no effect unless \code{x} is a list. If its
#'   elements are integers, \code{FALSE} implies that objects are considered as
#'   identical if their intersection is non-empty. If the elements are floats,
#'   they are compared using \code{mean} and \code{sd}. They are identical if
#'   there ranges, defined as \code{n} standard deviations around the mean,
#'   overlap, with \code{n} being either \code{1} or \code{2}, depending on
#'   \code{strict}.
#' @param na.rm Logical scalar. Remove \code{NA} elements before determining
#'   constantness?
#' @return Logical scalar.
#' @keywords internal
#'
setGeneric("is_constant", function(x, ...) standardGeneric("is_constant"))

setMethod("is_constant", "vector", function(x, na.rm = TRUE) {
  if (na.rm)
    x <- x[!is.na(x)]
  length(x) < 2L || all(duplicated.default(x)[-1L])
}, sealed = SEALED)

setMethod("is_constant", "list", function(x, na.rm = TRUE) {
  if (length(x) < 2L)
    return(TRUE)
  if (na.rm)
    x <- lapply(x, na.exclude)
  all(duplicated.default(x)[-1L])
}, sealed = SEALED)

setMethod("is_constant", "array", function(x, margin = 1L, na.rm = TRUE) {
  if (!margin)
    return(is_constant(as.vector(x), na.rm = na.rm))
  apply(X = x, MARGIN = margin, FUN = is_constant, na.rm = na.rm)
}, sealed = SEALED)

setMethod("is_constant", "OPM", function(x, na.rm = FALSE) {
  is_constant(x@measurements, 2L, na.rm)
}, sealed = SEALED)

setMethod("is_constant", "CMAT", function(x, strict, digits = opm_opt("digits"),
    na.rm = TRUE) {
  no_dup <- function(y) all(duplicated(if (na.rm)
    y[!is.na(y)]
  else
    y)[-1L])
  zero_sd <- function(y) !identical(!sd(y, na.rm = na.rm), FALSE)
  list_remove_na <- function(y) {
    y <- lapply(y, na.exclude)
    y[!!lengths(y, FALSE)]
  }
  uniq_list_const <- function(y) {
    if (na.rm)
      y <- list_remove_na(y)
    all(duplicated(lapply(y, unique.default))[-1L])
  }
  no_set_overlap <- function(y) {
    if (na.rm)
      y <- list_remove_na(y)
    for (i in seq_along(y)[-1L]) {
      v1 <- y[[i]]
      for (j in seq_len(i - 1L))
        if (!length(intersect(v1, y[[j]])))
          return(FALSE)
    }
    TRUE
  }
  all_distrib_overlap <- function(x, fac) {
    x <- cbind(vapply(x, mean, 0, na.rm = na.rm),
      vapply(x, sd, 0, na.rm = na.rm))
    x[, 2L] <- fac * x[, 2L]
    x <- cbind(x[, 1L] - x[, 2L], x[, 1L] + x[, 2L])
    for (i in seq_len(nrow(x)))
      if (any(x[i, 2L] < x[-i, 1L] | x[i, 1L] > x[-i, 2L], na.rm = TRUE))
        return(FALSE)
    TRUE
  }
  if (!length(x))
    return(logical(0L))
  if (nrow(x) < 2L)
    return(!logical(ncol(x)))
  case(typeof(x),
    integer = apply(x, 2L, no_dup),
    double = if (strict)
      apply(x, 2L, no_dup)
    else
      apply(round(x, digits), 2L, zero_sd),
    list = case(typeof(x[[1L]]),
      integer = apply(x, 2L, if (strict)
        uniq_list_const
      else
        no_set_overlap),
      double = apply(x, 2L, all_distrib_overlap, 2L - strict)
    )
  )
}, sealed = SEALED)


################################################################################


#' Check presence of split column, strip whitespace or make 1-row matrix
#'
#' Check whether a certain column is present and not at the end of a data frame
#' or matrix. Alternatively, strip any character or factor content from
#' whitespace at the end of the strings, or convert a vector to a matrix with
#' one row.
#'
#' @param x Data frame, matrix, array or vector.
#' @param split.at Names of columns at which \code{x} should be split.
#' @return Integer scalar indicating the split position. An error is raised
#'   if this is missing or non-unique.
#' @keywords internal
#'
assert_splittable_matrix <- function(x, split.at) {
  pos <- which(colnames(x) == split.at)
  LL(pos, .msg = listing(sprintf("'%s'", split.at), style = "sentence",
    prepend = FALSE, header = "need exactly one column name present among: ",
    last.sep = "comma"))
  if (pos == ncol(x))
    stop("column given by 'split.at' must not be the last one")
  pos
}

#' @rdname assert_splittable_matrix
#'
strip_whitespace <- function(x) {
  strip <- function(x) sub("^\\s+", "", sub("\\s+$", "", x, FALSE, TRUE),
    FALSE, TRUE)
  for (i in which(vapply(x, is.character, NA)))
    x[, i] <- strip(x[, i])
  for (i in which(vapply(x, is.factor, NA)))
    levels(x[, i]) <- strip(levels(x[, i]))
  x
}

#' @rdname assert_splittable_matrix
#'
vector2row <- function(x) matrix(x, 1L, length(x), FALSE, list(NULL, names(x)))

#' @rdname assert_splittable_matrix
#'
collect_rows <- function(x) {
  #sortable_indexes <- function(x) {
  #  n <- seq_along(x)
  #  sprintf(sprintf("%%0%ii", ceiling(log(n[length(n)], 10))), n)
  #}
  add_cols <- function(x, cols) {
    if (length(cols <- setdiff(cols, colnames(x))))
      cbind(x, matrix(NA, nrow(x), length(cols), FALSE, list(NULL, cols)))
    else
      x
  }
  cn <- unique.default(unlist(lapply(x, colnames), FALSE, FALSE))
  do.call(rbind, lapply(x, add_cols, cn))
}


################################################################################
################################################################################
#
# String processing
#


#' Create metadata key
#'
#' A helper function for \code{\link{metadata}} and the methods that are
#' dependent on it.
#'
#' @param x List, formula, or atomic object.
#' @param to.formula Logical scalar indicating whether conversion to a formula
#'   should be conducted.
#' @param remove Names of elements to be deleted after conversion to list.
#' @param syntactic Logical scalar indicating whether names should be converted
#'   to syntactic names.
#' @param ops Character vector containing the operators to use when converting
#'   a list to a formula. Recycled if necessary.
#' @param full.eval Logical scalar indicating whether to evaluate the result.
#'   Usually makes no sense for formulae here.
#' @param envir Passed to \code{eval}.
#' @param fmt Character scalar. The format of the formula; omitting \sQuote{~}
#'   yields an error.
#' @param f Formula to be converted to an infix operator.
#' @param ... Passed to \code{sprintf} after joining. It is an error to not
#'   pass enough arguments.
#' @param env Passed to \code{formula} as \sQuote{env} argument.
#' @param use Character scalar used for modifiying metadata queries in the
#'   environment of the caller.
#' @return List, formula or character vector.
#' @keywords internal
#'
metadata_key <- function(x, to.formula, ...) UseMethod("metadata_key")

#' @rdname metadata_key
#' @method metadata_key default
#' @export
#'
metadata_key.default <- function(x, to.formula = FALSE, remove = NULL, ...) {
  if (!is.atomic(x))
    stop(NOT_YET)
  if (length(x) == 1L && x %in% remove)
    return(NULL)
  if (to.formula) # no 'syntactic' argument here -- should always be syntactic
    create_formula("~ c(%s)", paste0(x, collapse = ", "))
  else
    x
}

#' @rdname metadata_key
#' @method metadata_key factor
#' @export
#'
metadata_key.factor <- function(x, ...) {
  metadata_key.character(structure(.Data = as.character(x), names = names(x)),
    ...)
}

#' @rdname metadata_key
#' @method metadata_key character
#' @export
#'
metadata_key.character <- function(x, to.formula = FALSE, remove = NULL,
    syntactic = FALSE, ...) {
  if (length(x) == 1L && x %in% remove)
    return(NULL)
  if (to.formula) {
    if (syntactic)
      x <- make.names(x)
    return(create_formula("~ `%s`",
      paste0(x, collapse = get("key.join", OPM_OPTIONS))))
  }
  if (is.null(names(x)))
    names(x) <- x
  x
}

#' @rdname metadata_key
#' @method metadata_key list
#' @export
#'
metadata_key.list <- function(x, to.formula = FALSE, remove = NULL,
    syntactic = FALSE, ops = "+", ...) {
  join <- function(x) vapply(x, paste0, "",
    collapse = get("key.join", OPM_OPTIONS))
  if (is.null(names(x <- flatten(x))))
    names(x) <- join(x)
  else
    names(x)[bad] <- join(x[bad <- !nzchar(names(x)) | is.na(names(x))])
  x <- x[!names(x) %in% remove]
  if (syntactic) {
    names(x) <- make.names(names(x))
    x <- lapply(x, make.names)
  }
  if (!to.formula)
    return(x)
  fmt <- case(length(x), stop("'x' must not be empty"), "",
    paste(rep_len(ops, length(x) - 1L), "`%s`", collapse = " "))
  create_formula(paste("~ `%s`", fmt), names(x))
}

#' @rdname metadata_key
#' @method metadata_key formula
#' @export
#'
metadata_key.formula <- function(x, to.formula = FALSE, remove = NULL,
    syntactic = FALSE, ..., full.eval = !to.formula, envir = parent.frame()) {
  elem_type <- function(name) switch(as.character(name),
    `::` =, `:::` =, `$` =, `@` = 1L, # operators with highest precedence
    `I` = 2L, # protected formula elements
    `J` = 3L, # causing on-the-fly joining of metadata elements
    4L # anything else
  )
  apply_to_tail <- function(x, fun) {
    for (i in seq_along(x)[-1L])
      x[[i]] <- fun(x[[i]])
    x
  }
  combine <- new.env(parent = emptyenv())
  comb_list <- function(...) {
    if (length(keys <- flatten(x <- list(...))) > 1L) {
      keys <- vapply(X = keys, FUN = paste0, FUN.VALUE = "",
        collapse = get("key.join", OPM_OPTIONS))
      combine[[paste0(keys,
        collapse = get("comb.key.join", OPM_OPTIONS))]] <- keys
    }
    x
  }
  comb_names <- function(x) {
    x <- all.vars(x)
    key <- paste0(x, collapse = get("comb.key.join", OPM_OPTIONS))
    if (length(x) > 1L)
      combine[[key]] <- x
    as.name(key)
  }
  final_comb_list <- function(x, remove) {
    x <- as.list(x)
    if (length(remove))
      x <- x[!vapply(x, function(y) any(y %in% remove), NA)]
    if (length(x))
      x
    else
      NULL
  }
  c.name <- as.name("c")
  list.name <- as.name("list")
  comblist.name <- as.name("comb_list")
  rec_listify <- function(x) case(length(x), NULL, if (is.call(x))
      NULL
    else if (is.name(x))
      as.character(x)
    else
      x, switch(
    elem_type(x[[1L]]),
    {
      x[[1L]] <- c.name # tight binding
      apply_to_tail(x, rec_listify)
    },
    {
      x[[1L]] <- c.name # tight binding, no changes
      eval(x, envir)
    },
    {
      x[[1L]] <- comblist.name
      apply_to_tail(x, rec_listify)
    },
    {
      x[[1L]] <- list.name
      apply_to_tail(x, rec_listify)
    }
  ))
  rec_replace <- function(x) case(length(x), x, if (is.character(x))
      as.name(x)
    else
      x, switch(
    elem_type(x[[1L]]),
    as.name(paste0(all.vars(apply_to_tail(x, rec_replace)),
      collapse = get("key.join", OPM_OPTIONS))),
    {
      x[[1L]] <- c.name
      as.name(paste0(eval(x, envir), collapse = get("key.join", OPM_OPTIONS)))
    },
    comb_names(apply_to_tail(x, rec_replace)),
    apply_to_tail(x, rec_replace)
  ))
  rec_make_names <- function(x) {
    if (is.name(x))
      as.name(make.names(x)) # make.names() converts to character mode
    else
      apply_to_tail(x, rec_make_names)
  }
  result <- if (to.formula)
    rec_replace(x[[length(x)]])
  else
    rec_listify(x[[length(x)]])
  if (full.eval) {
    result <- metadata_key(x = eval(result, enclos = envir), remove = remove,
      syntactic = syntactic, ...)
    if (length(result))
      attr(result, "combine") <- final_comb_list(combine, remove)
    result
  } else {
    # 'result' is a formula at this stage
    if (syntactic)
      result <- rec_make_names(result)
    x[[length(x)]] <- result
    attr(x, "combine") <- final_comb_list(combine, remove)
    x
  }
}

#' @rdname metadata_key
#' @keywords internal
#'
create_formula <- function(fmt, ..., .env = parent.frame()) {
  x <- c(list(fmt = fmt), lapply(list(...), as.list))
  formula(do.call(sprintf, unlist(x, FALSE, FALSE)), .env)
}

#' @rdname metadata_key
#' @keywords internal
#'
formula2infix <- function(f) {
  if (length(f) > 2L)
    sprintf("%%%s%%", all.vars(f[[2L]]))
  else
    "%q%"
}

#' @rdname metadata_key
#' @keywords internal
#'
reassign_args_using <- function(use) {
  case(use,
    i =, I = NULL,
    k =, K = assign("values", FALSE, parent.frame()),
    n = assign("negative", "any", parent.frame()),
    N = assign("negative", "all", parent.frame()),
    p = assign("positive", "any", parent.frame()),
    P = assign("positive", "all", parent.frame()),
    q = {
      assign("values", TRUE, parent.frame())
      assign("exact", FALSE, parent.frame())
    },
    Q = {
      assign("values", TRUE, parent.frame())
      assign("exact", TRUE, parent.frame())
    },
    t =, T = assign("time", TRUE, parent.frame()),
    c =, C = assign("common", TRUE, parent.frame())
  )
  invisible(NULL)
}


################################################################################


#' Parse time strings
#'
#' Parse time strings by trying potentially many formats in turn. Each
#' subsequent format is only applied to the \code{NA} values created in the
#' previous attempt, if any. A warning is issued if final \code{NA} values
#' remain.
#'
#' @param object Character vector.
#' @return Object of class \sQuote{POSIXlt}.
#' @seealso base::strptime
#' @keywords internal
#'
setGeneric("parse_time",
  function(object, format, ...) standardGeneric("parse_time"))

setMethod("parse_time", c("character", "missing"), function(object, format,
    tz = opm_opt("time.zone")) {
  parse_time(object, opm_opt("time.fmt"), tz)
}, sealed = SEALED)

setMethod("parse_time", c("character", "character"), function(object, format,
    tz = opm_opt("time.zone")) {
  if (!length(format))
    stop("need non-empty object 'format'")
  result <- strptime(object, format[1L], tz)
  for (fmt in format[-1L])
    result[isna] <- strptime(object[isna <- is.na(result)], fmt, tz)
  if (anyNA(result))
    warning("parsing time strings resulted in NA values")
  result
}, sealed = SEALED)


################################################################################


#' Trim string or add note in parentheses
#'
#' Trim a string to a given length, but by default append an indicator of
#' whether something has been trimmed. Alternatively, append an annotation in
#' parentheses to a string; trim it if necessary.
#'
#' @param x Character vector from which to remove the concentration indicators.
#' @param str Character vector or convertible to such.
#' @param max Numeric scalar. Maximum allowed length.
#' @param append Character scalar. To be appended to strings that needed to be
#'   trimmed, for indicating just that.
#' @param clean Logical scalar. If \code{TRUE}, clean trimmed end from non-word
#'   characters, and return empty string if only \code{append} remains.
#' @param word.wise Logical scalar. If \code{TRUE}, abbreviate words separately,
#'   deleting vowels first. Alternatively, do abbreviation per word?
#' @param str.1 Character vector or convertible to such.
#' @param str.2 Character vector or convertible to such, to be added in
#'   parentheses. Trimming only affects \code{str.2}, and not the parentheses.
#' @param brackets Logical scalar. Should brackets instead of parentheses be
#'   used?
#' @param paren.sep Character scalar. What to insert before the opening
#'   parenthesis (or bracket).
#' @param i Integer scalar indicating the position of the partial match.
#' @param m Object as returned by \code{regexpr}.
#' @param string Originally matched character vector.
#' @return Character vector.
#' @keywords internal
#'
trim_string <- function(str, max, append = ".", clean = TRUE,
    word.wise = FALSE) {
  do_trim <- function(x) {
    trim.len <- max(0L, max - nchar(append))
    if (word.wise) {
      if (clean)
        x <- gsub("\\W", "", x, FALSE, TRUE)
      result <- abbreviate(x, minlength = trim.len, strict = TRUE)
    } else {
      result <- strtrim(x, trim.len)
      if (clean)
        result <- sub("\\W+$", "", result, FALSE, TRUE)
    }
    result
  }
  long <- nchar(str) > max
  str[long] <- do_trim(str[long])
  if (clean)
    long <- long & nzchar(str)
  str[long] <- paste0(str[long], append)
  str
}

#' @rdname trim_string
#' @keywords internal
#'
add_in_parens <- function(str.1, str.2, max = 1000L, append = ".",
    clean = TRUE, brackets = FALSE, word.wise = FALSE, paren.sep = " ") {
  max <- max - nchar(str.1) - 3L
  if (!grepl("^\\s*$", paren.sep))
    stop("'paren.sep' must only contain whitespace characters")
  str.2 <- trim_string(str.2, max, append = append, clean = clean,
    word.wise = word.wise)
  if (brackets) {
    template <- "%s%s[%s]"
    str.2 <- chartr("[]", "()", str.2)
    remove <- " \\[\\]$"
  } else {
    template <- "%s%s(%s)"
    str.2 <- chartr("()", "[]", str.2)
    remove <- " \\(\\)$"
  }
  sub(remove, "", sprintf(template, str.1, paren.sep, str.2), FALSE, TRUE)
}

#' @rdname trim_string
#' @keywords internal
#'
remove_concentration <- function(x) {
  sub("\\s*#\\s*\\d+\\s*$", "", x, FALSE, TRUE)
}

#' @rdname trim_string
#' @keywords internal
#'
get_partial_match <- function(i, m, string) {
  start <- attr(m, "capture.start")[, i]
  substr(string, start, start + attr(m, "capture.length")[, i] - 1L)
}


################################################################################


#' Convert recursively to HTML or create HTML head
#'
#' This are the helper functions used by \code{\link{format}} for converting
#' user-defined additions to \acronym{HTML} or for creating the \sQuote{head}
#' entry of an \acronym{HTML} strings.
#'
#' @param x List or other vector.
#' @param level Integer scalar defining the starting level for indentation and
#'   naming of unnamed sections.
#' @param fmt Character scalar used for transforming \code{level} into section
#'   \sQuote{class} and \sQuote{title} attributes.
#' @param fac Integer scalar for inferring the number of spaces used for
#'   indentation from the current \code{level} (recursively incremented).
#' @param title Character scalar defining the title of the \acronym{HTML}
#'   document. Must contain an attribute called as returned by
#'   \code{\link{opm_string}}.
#' @param css Character vector containing the names of \acronym{CSS} files to
#'   link or embed.
#' @param meta Character vector defining additional meta tags.
#' @param emebed Logical scalar indicating whether \acronym{CSS} shall be
#'   embedded (not linked).
#' @param ... Optional arguments (for \code{single_tag}, named arguments with
#'   \acronym{HTML} tga attributes).
#' @return Character scalar or vector.
#' @details  If applied to lists, this functions works recursively, generating
#'   \sQuote{div} elements from each list. Names are used as \sQuote{class} and
#'   \sQuote{title} attributes. Where names are missing, \code{level} is used in
#'   conjunction with \code{fmt}. Non-list vectors are converted using
#'   \sQuote{span} tags if names are present, simply joined otherwise.
#' @keywords internal
#'
list2html <- function(x, level = 1L, fmt = opm_opt("html.class"), fac = 2L) {
  indent <- paste0(rep.int(" ", fac * (level - 1L)), collapse = "")
  if (is.list(x)) {
    if (is.null(n <- names(x)))
      n <- sprintf(fmt, level)
    else
      n[!nzchar(n)] <- sprintf(fmt, level)
    n <- ifelse(nzchar(n), safe_labels(n, "html"), NA_character_)
    x <- vapply(x, list2html, "", level + 1L, fmt)
    x <- paste0(x, indent)
    x <- hmakeTag("div", x, class = n, title = n, newline = TRUE)
    paste0(indent, x, collapse = "")
  } else {
    if (is.character(x) && !inherits(x, "AsIs"))
      x <- safe_labels(x, "html")
    if (!is.null(n <- names(x))) {
      n <- ifelse(nzchar(n), safe_labels(n, "html"), NA_character_)
      x <- hmakeTag("span", x, class = n, title = n)
    }
    paste0(indent, paste0(x, collapse = " "), "\n")
  }
}

#' @rdname list2html
#' @keywords internal
#'
single_tag <- function(x, ...) {
  listing(list(...), c("<", x), ">", style = " %s=\"%s\"", collapse = "")
}

#' @rdname list2html
#' @keywords internal
#'
html_head <- function(title, css, meta, embed) {

  html_comment <- function(x) {
    safe_labels(x, "html", comment = TRUE, enclose = FALSE)
  }

  if (length(title)) {
    from.opm <- attr(title, opm_string())
    # Tidy accepts only a single title entry
    title <- hmakeTag("title", data = safe_labels(title[1L], format = "html"))
    if (!from.opm)
      title <- c(html_comment("user-defined title"), title)
  } else
    title <- NULL

  if (length(css <- css[nzchar(css)]))
    if (embed) {
      x <- lapply(X = css, FUN = readLines, warn = FALSE)
      css <- html_comment(paste("CSS from user-defined file", css))
      css <- mapply(FUN = c, css, single_tag("style", type = "text/css"), x,
        MoreArgs = list("</style>", ""), SIMPLIFY = FALSE, USE.NAMES = FALSE)
      css <- unlist(css, FALSE, FALSE)
    } else {
      is.abs.path <- grepl("^(/|[a-zA-Z]:)", css, FALSE, TRUE)
      css[is.abs.path] <- sprintf("file://%s", css[is.abs.path])
      css <- vapply(css, function(y) {
        single_tag("link", rel = "stylesheet", type = "text/css", href = y)
      }, "")
      css <- c(html_comment("user-defined CSS file(s)"), unname(css))
    }
  else
    css <- NULL

  generator <- single_tag("meta", name = "generator",
    content = paste0(opm_string(version = TRUE), collapse = " version "))

  # see http://www.w3.org/TR/NOTE-datetime
  time <- format(Sys.time(), "%Y-%M-%dT%H:%M:%S%z")
  time <- single_tag("meta", name = "date", content = time)

  if (length(meta)) {
    meta <- vapply(meta, function(y) {
      if (is.null(names(y)))
        stop("HTML meta entry without names")
      do.call(single_tag, c(list(x = "meta"), as.list(y)))
    }, "")
    meta <- c(html_comment("user-defined metadata"), unname(meta))
  } else {
    meta <- NULL
  }

  c("<head>", title, generator, time, meta, css, "</head>")
}


################################################################################


#' Check HTML using the Tidy program
#'
#' Run the Tidy program for check or converting \acronym{HTML} character
#' vectors.
#'
#' @param object Query character vector, or list of such vectors, or missing. If
#'   missing, the location of the tidy executable is returned
#' @param check Logical scalar. If \code{TRUE}, the Tidy checking results,
#'   potentially including warnings and error messages, are captured in a
#'   character vector. Otherwise the converted \acronym{HTML} is returned.
#' @param args Character vector with arguments passed to Tidy. Is
#'   is currently an error to set any of its \sQuote{File manipulation} options.
#' @param ... Optional arguments passed between the methods.
#' @return Character vector, or list of such vectors. If \code{object} is
#'   missing, the method returns the location of the Tidy executable but
#'   \code{NULL} if it cannot be found.
#' @keywords internal
#'
setGeneric("tidy", function(object, ...) standardGeneric("tidy"))

setMethod("tidy", "missing", function() {
  if (nzchar(result <- Sys.which("tidy")))
    result
  else
    NULL
}, sealed = SEALED)

setMethod("tidy", "character", function(object, check = TRUE,
    args = c("-u", "-i")) {
  LL(check, program <- tidy())
  bad <- c("-o", "-output", "-config", "-file", "-f", "-modify", "-m")
  if (any(bad %in% (args <- as.character(args))))
    stop("you cannot set any of the 'File manipulation' options")
  if (stderr <- check)
    args <- c(args, "-e") # '-e' turns off the output of converted HTML
  else
    args <- setdiff(args, "-e")
  # NB: the combination of stderr = TRUE and stdout = FALSE/"" is impossible
  suppressWarnings(system2(command = program, args = unique(args),
    input = object, stderr = stderr, stdout = TRUE))
}, sealed = SEALED)

setMethod("tidy", "list", function(object, ...) {
  lapply(X = object, FUN = tidy, ...)
}, sealed = SEALED)


################################################################################
################################################################################
#
# Mapping functions
#


setAs(from = "ANY", to = "factor", function(from) as.factor(from))
setAs(from = "ANY", to = "ordered", function(from) as.ordered(from))


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
################################################################################
#
# YAML reparation
#


#' Repair NAs or names with dots
#'
#' Replace \sQuote{NA} by \code{NA_character_}. When reading \acronym{YAML}
#' input previously output by \R, \sQuote{NA} values cause numeric vectors to be
#' interpreted as character; \code{repair_na_strings} fixes this problem. In
#' contrast, \code{rescue_dots} replaces underscores by dots. Used for objects
#' passed through \code{\link{to_yaml}}. Not fail-safe.
#'
#' @param object Character vector or list.
#' @param type Character scalar denoting the type to which input character
#'   vectors shall be tried to be converted.
#' @return Character vector or list.
#' @seealso utils::type.convert
#' @details This problem does not occur anymore with the \pkg{yaml} package of
#'   at least version 2.1.7, but for legacy \acronym{YAML} (and \acronym{JSON})
#'   files it is necessary to conduct the conversions implemented here.
#' @keywords internal
#' @references \url{http://www.yaml.org/}
#'
repair_na_strings <- function(object, ...) UseMethod("repair_na_strings")

#' @rdname repair_na_strings
#' @method repair_na_strings character
#' @export
#'
repair_na_strings.character <- function(object, ...) {
  object[grepl("^(\\s*NA|\\.na(\\.(real|integer|character))?)$", object,
    FALSE, TRUE)] <- NA_character_
  object
}

#' @rdname repair_na_strings
#' @method repair_na_strings list
#' @export
#'
repair_na_strings.list <- function(object,
    type = c("double", "integer", "complex", "logical", "character"), ...) {
  type <- match.arg(type)
  mapfun <- if (type == "character")
    repair_na_strings.character
  else
    function(x) tryCatch(expr = {
      x <- repair_na_strings.character(x)
      storage.mode(x) <- type
      x
    }, warning = function(w) x)
  rapply(object, mapfun, "character", NULL, "replace")
}

#' @rdname repair_na_strings
#'
rescue_dots <- function(x) {
  if (is.character(x) && any(bad <- grepl("^_[^_]*_", x, FALSE, TRUE)))
    x[bad] <- chartr("_", ".", substr(x[bad], 2L, nchar(x[bad])))
  x
}


################################################################################
################################################################################
#
# Lists
#


#' Insert a list into a list
#'
#' Insert all values from another list in a list, either by overwriting the
#' previously present data or by only setting the missing ones. Note that this
#' comparison is based on the names. It does not matter whether the values are
#' \code{NULL}.
#'
#' @param object List.
#' @param other  List or other \R object to insert. Can also be missing.
#' @param ... Optional other items to insert.
#' @param .force Logical scalar. Overwite items that are already there?
#' @param .strict Logical scalar. If \code{TRUE}, has precedence over
#'   \code{.force} and causes some restrictions: Only names that are already
#'   present are allowed, and the classes must match the classes of the already
#'   contained values.
#' @return List, potentially modified.
#' @seealso utils::modifyList
#' @keywords internal
#'
insert <- function(object, ...) UseMethod("insert")

#' @rdname insert
#' @method insert list
#' @export
#'
insert.list <- function(object, other, ..., .force = FALSE, .strict = FALSE) {
  insert_carefully <- function(x, y) {
    if (length(bad <- setdiff(nn <- names(y), names(x))))
      stop("unknown key: ", bad[1L])
    for (name in nn) {
      novel <- y[[name]]
      if (!identical(class(novel), wanted <- class(x[[name]])))
        stop(sprintf("value of key '%s' must have class '%s'", name,
          paste0(wanted, collapse = " -> ")))
      x[[name]] <- novel
    }
    x
  }
  other <- if (missing(other))
    list(...)
  else if (is.list(other))
    c(other, list(...))
  else
    list(other, ...)
  if (.strict)
    return(insert_carefully(object, other))
  keys <- names(other)
  if (!.force)
    keys <- setdiff(keys, names(object))
  object[keys] <- other[keys]
  object
}


################################################################################


#' Modify a CMAT object
#'
#' Modify a CMAT object in a specified way.
#'
#' @param object An object of class \sQuote{CMAT}.
#' @param how Characater scalar indicating how \code{object} should be
#'   modified.
#' @param digits Integer scalar indicating the number of decimal points to
#'   consider when comparing \sQuote{double} values.
#' @param na.rm Logical scalar passed to \code{\link{is_constant}} (if
#'   applicable).
#' @return Object of the same class than \code{object}.
#' @keywords internal
#'
setGeneric("update")

setMethod("update", "CMAT", function(object,
    how = c("NA2int", "delete.uninf", "delete.constant", "delete.ambig"),
    digits = opm_opt("digits"), na.rm = TRUE) {
  if (!length(object))
    return(object)
  shiftable <- function(x) {
    x <- unique.default(x)
    length(x[!is.na(x)]) == 2L
  }
  shift_int <- function(x) {
    isna <- is.na(x)
    x.max <- max(x[!isna])
    x.min <- min(x[!isna])
    if (x.max == x.min + 1L) {
      x[x == x.max] <- x.max + 1L
      x.max <- x.max + 1L
    }
    x[isna] <- as.integer(mean(c(x.min, x.max)))
    x
  }
  has_ambig <- function(x) {
    if (na.rm)
      x <- lapply(x, na.exclude)
    for (item in x) {
      if (length(unique.default(item)) > 1L)
        return(TRUE)
    }
    FALSE
  }
  has_nonzero_sd <- function(x) {
    isTRUE(sd(x, na.rm = TRUE) > .Machine$double.eps ^ 0.5)
  }
  no.transformation <- "transforming NA impossible: not two non-NA entries"
  switch(how <- match.arg(how),
    NA2int = {
      switch(typeof(object),
        integer = if (shiftable(object))
          object[] <- shift_int(object)
        else
          warning(no.transformation)
        ,
        list = if (typeof(object[[1L]]) == "integer")
          if (shiftable(unlist(object)))
            object[] <- lapply(object, shift_int)
          else
            warning(no.transformation)
      )
    },
    {
      bad <- case(sub("^delete\\.", "", how, FALSE, TRUE),
        ambig = if (typeof(object) == "list")
          case(typeof(object[[1L]]),
            integer = apply(object, 2L, has_ambig),
            double = apply(object, 2L, has_nonzero_sd))
        else
          FALSE,
        constant = is_constant(object, strict = TRUE, digits = digits,
          na.rm = na.rm),
        uninf = is_constant(object, strict = FALSE, digits = digits,
          na.rm = na.rm)
      )
      if (any(bad))
        object <- as(object[, !bad, drop = FALSE], "CMAT")
    }
  )
  object
}, sealed = SEALED)


################################################################################

