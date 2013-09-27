

################################################################################
################################################################################
#
# Class definitions and associated methods.
#


#' DBTABLES class
#'
#' This virtual class is intended for holding, in each slot, a data frame that
#' can be written to (or read from) a table in a relational database.
#'
#' @details
#' The idea behind this class is to store the tables that are dependent on each
#' other via foreign keys, in order. Based on a simple naming convention,
#' methods check the supposed primary and the foreign keys (cross references) of
#' the tables.
#'
#' Primary keys must be present in a column called \sQuote{id}, which must not
#' contain duplicates. Columns with foreign keys must be named like
#' \code{paste0(x, "_id")} and then refer to the column \sQuote{id} in a
#' preceding slot named \code{paste0(x, "s")}. Irregular plurals are not
#' currently understood. The two columns must be setequal.
#'
#' For real classes inheriting from \code{DBTABLES}, see \code{\link{OPM_DB}}.
#'
#' For the methods of \code{DBTABLES}, see \code{\link{DBTABLES-methods}}.
#' @name DBTABLES
#' @docType class
#' @export
#' @aliases DBTABLES-class
#' @seealso methods::Methods
#' @family classes
#' @keywords methods classes
#'
setClass("DBTABLES",
  contains = "VIRTUAL",
  sealed = SEALED
)


################################################################################


#' Methods for DBTABLES objects
#'
#' Return or check the supposed primary and foreign keys in a
#' \code{\link{DBTABLES}} object, or show, traverse, combine, or split such
#' objects.
#'
#' @param object \code{\link{DBTABLES}} object.
#' @param x \code{\link{DBTABLES}} object.
#' @param incr Numeric vector or \code{NULL}. If a numeric vector and named, the
#'   names must be the slot names; otherwise they are inserted, in order. Each
#'   value of \code{incr} defines an increment for the primary key in the
#'   respective table. \code{update} takes care that the foreign keys referring
#'   to this table are modified in the same way.
#'
#'   If \code{NULL}, \code{incr} is set to a vector that causes all output
#'   primary keys to start at 1. This can be used to revert a previous call of
#'   \code{update}.
#' @param recursive Logical scalar passed from \code{c} as \code{drop} argument
#'   to \code{update}. Also causes \code{update} to be applied to \code{x} (and
#'   not only to the elements of \code{...}, if any).
#' @param f Missing or (list of) factor(s) that fit(s) to the dimensions of the
#'   first table returned by. If missing, the primary key of this table is used.
#' @param drop For \code{update}, a logical scalar that indicates whether or not
#'   the row names of all tables should be set to \code{NULL}.
#'
#'   For \code{split}, \code{drop} is either missing or a logical scalar.
#'   \code{TRUE} is the default. If so, after splitting, \code{update} is
#'   applied to each \code{\link{DBTABLES}} object that is an element of the
#'   resulting list, with \code{incr} set to \code{NULL} and \code{drop} set to
#'   \code{TRUE}.
#' @param fun Function to be applied to each table in turn. Should accept a
#'   data frame and a character scalar (name of the table) as the first two
#'   (unnamed) arguments.
#' @param ... Objects of the same class as \code{x}, or optional further
#'   arguments of \code{fun}.
#' @param simplify Logical scalar passed to \code{mapply} as \sQuote{SIMPLIFY}
#'   argument.
#' @return
#'   \code{primary_keys} yields a character vector with the (expected) name of
#'   the primary key column for each table. \code{foreign_keys} returns a matrix
#'   that describes the (expected) relations between the tables. For using
#'   another naming scheme for the keys, at least on of these methods has to be
#'   overwritten in a child class. The same holds if slots should be included
#'   that are not treated as data base tables. \code{split} should then also
#'   be overwritten to not ignore these other slots.
#'
#'   \code{foreign_keys_valid} and \code{primary_keys_valid} return \code{TRUE}
#'   if successful and a character vector describing the problems otherwise.
#'   These functions can thus be used as \code{validity} argument of
#'   \code{setClass}.
#'
#'   \code{summary} creates an S3 object that nicely prints to the screen (and
#'   is used by \code{show}).
#'
#'   \code{length} returns the number of rows of the data frame in the slot
#'   defined by the first entry of \code{foreign_keys}.
#'
#'   \code{head} and \code{tail} return the minimum and maximum available
#'   primary key, respectively, for all contained tables.
#'
#'   \code{update} and \code{c} return an object of the same class than
#'   \code{object} and \code{x}, respectively. \code{c} runs \code{update} on
#'   all objects in \code{...} to yield overall unique IDs and then runs
#'   \code{rbind} on all tables.
#'
#'   \code{split} is the approximate inverse of \code{c} for the \code{DBTABLES}
#'   class. It uses \code{f} for splitting the first table returned by
#'   \code{primary_keys} and then proceeds by accordingly splitting the tables
#'   that refer to it.
#' @name DBTABLES-methods
#' @seealso methods::setClass
#' @family classes-functions
#' @keywords attribute
#' @examples
#' ## see the examples for the non-virtual classes inheriting from DBTABLES
#'
NULL

#= foreign_keys DBTABLES-methods

#' @rdname DBTABLES-methods
#' @export
#'
setGeneric("foreign_keys", function(object) standardGeneric("foreign_keys"))

setMethod("foreign_keys", "DBTABLES", function(object) {
  pk <- primary_keys(object)
  do.call(rbind, lapply(names(pk), function(n) {
      refs <- grep("^\\w+_id$", colnames(slot(object, n)), FALSE, TRUE, TRUE)
      if (length(refs))
        to.tbl <- paste0(substr(refs, 1L, nchar(refs) - 3L), "s")
      else
        refs <- to.tbl <- NA_character_
      cbind(from.tbl = n, from.col = refs, to.tbl = to.tbl, to.col = pk[[n]])
    }))
}, sealed = SEALED)

#= primary_keys DBTABLES-methods

#' @rdname DBTABLES-methods
#' @export
#'
setGeneric("primary_keys", function(object) standardGeneric("primary_keys"))

setMethod("primary_keys", "DBTABLES", function(object) {
  x <- slotNames(object)
  structure(rep.int("id", length(x)), names = x)
}, sealed = SEALED)

#= foreign_keys_valid DBTABLES-methods

#' @rdname DBTABLES-methods
#' @export
#'
setGeneric("foreign_keys_valid",
  function(object) standardGeneric("foreign_keys_valid"))

setMethod("foreign_keys_valid", "DBTABLES", function(object) {
  x <- foreign_keys(object)[-1L, , drop = FALSE]
  bad <- is.na(x[, "from.col"])
  errs <- sprintf("no references in slot '%s'", x[bad, "from.tbl"])
  x <- x[!bad, , drop = FALSE]
  bad <- apply(x, 1L, function(row) tryCatch({
      other <- slot(object, row["to.tbl"])[, row["to.col"]]
      self <- slot(object, row["from.tbl"])[, row["from.col"]]
      if (!all(self %in% other))
        stop("dead references")
      if (!all(other %in% self))
        stop("superfluous ids")
      NA_character_
    }, error = conditionMessage))
  if (any(really <- !is.na(bad)))
    errs <- c(errs, sprintf("problem in %s/%s: %s",
      x[really, "from.tbl"], x[really, "to.tbl"], bad[really]))
  if (length(errs))
    errs
  else
    TRUE
}, sealed = SEALED)

#= primary_keys_valid DBTABLES-methods

#' @rdname DBTABLES-methods
#' @export
#'
setGeneric("primary_keys_valid",
  function(object) standardGeneric("primary_keys_valid"))

setMethod("primary_keys_valid", "DBTABLES", function(object) {
  pk <- primary_keys(object)
  result <- mapply(function(slotname, colname) tryCatch({
      if (anyDuplicated(slot(object, slotname)[, colname]))
        stop("non-unique IDs")
      NA_character_
    }, error = conditionMessage), names(pk), pk)
  if (length(result <- result[!is.na(result)]))
    sprintf("problem in %s: %s", names(result), result)
  else
    TRUE
}, sealed = SEALED)

#= summary DBTABLES-methods

setMethod("summary", "DBTABLES", function(object) {
  structure(list(Class = class(object), Crossrefs = foreign_keys(object)),
    class = "DBTABLES_Summary")
}, sealed = SEALED)

#= show DBTABLES-methods

setMethod("show", "DBTABLES", function(object) {
  print(summary(object))
}, sealed = SEALED)

#= length DBTABLES-methods

setMethod("length", "DBTABLES", function(x) {
  nrow(slot(x, names(primary_keys(x))[1L]))
}, sealed = SEALED)

#= head DBTABLES-methods

#' @rdname DBTABLES-methods
#' @export
#'
setGeneric("head")

setMethod("head", "DBTABLES", function(x) {
  pk <- primary_keys(x)
  mapply(function(slotname, colname) min(slot(x, slotname)[, colname]),
    names(pk), pk)
}, sealed = SEALED)

#= tail DBTABLES-methods

#' @rdname DBTABLES-methods
#' @export
#'
setGeneric("tail")

setMethod("tail", "DBTABLES", function(x) {
  pk <- primary_keys(x)
  mapply(function(slotname, colname) max(slot(x, slotname)[, colname]),
    names(pk), pk)
}, sealed = SEALED)

#= update DBTABLES-methods

#' @rdname DBTABLES-methods
#' @export
#'
setGeneric("update")

setMethod("update", "DBTABLES", function(object, incr, drop = TRUE) {
  unrowname <- function(x) {
    rownames(x) <- NULL
    x
  }
  add <- function(x, where, by) {
    x[, where] <- x[, where] + as.integer(by)
    x
  }
  if (is.null(incr))
    incr <- 1L - head(object)
  if (any(is.na(incr)))
    stop("'incr' contains missing values")
  pk <- primary_keys(object)
  if (is.null(names(incr)))
    names(incr) <- names(pk)[seq_along(incr)]
  if (drop)
    for (tn in names(pk))
      slot(object, tn) <- unrowname(slot(object, tn))
  if (!length(incr <- incr[!!incr]))
    return(object)
  crs <- foreign_keys(object)
  for (i in seq_along(incr)) {
    tn <- names(incr)[i]
    slot(object, tn) <- add(slot(object, tn), pk[[tn]], incr[i])
    cr <- crs[crs[, "to.tbl"] == tn, , drop = FALSE]
    for (j in seq_len(nrow(cr))) {
      tn <- cr[j, "from.tbl"]
      slot(object, tn) <- add(slot(object, tn), cr[j, "from.col"], incr[i])
    }
  }
  object
}, sealed = SEALED)

#= c DBTABLES-methods

setMethod("c", "DBTABLES", function(x, ..., recursive = FALSE) {
  if (recursive)
    x <- update(x, NULL, TRUE)
  if (missing(..1))
    return(x)
  klass <- class(x)
  pk <- primary_keys(x)
  x <- list(x, ...)
  for (i in seq_along(x)[-1L])
    x[[i]] <- update(x[[i]], tail(x[[i - 1L]]), recursive)
  result <- sapply(X = names(pk), simplify = FALSE,
    FUN = function(slotname) do.call(rbind, lapply(x, slot, slotname)))
  do.call(new, c(list(Class = klass), result))
}, sealed = SEALED)


#= split DBTABLES-methods

#' @rdname DBTABLES-methods
#' @export
#'
setGeneric("split")

setMethod("split", c("DBTABLES", "missing", "missing"), function(x, f, drop) {
  f <- primary_keys(x)[1L]
  split(x, slot(x, names(f))[, f], TRUE)
}, sealed = SEALED)

setMethod("split", c("DBTABLES", "missing", "logical"), function(x, f, drop) {
  f <- primary_keys(x)[1L]
  split(x, slot(x, names(f))[, f], drop)
}, sealed = SEALED)

setMethod("split", c("DBTABLES", "ANY", "missing"), function(x, f, drop) {
  split(x, f, TRUE)
}, sealed = SEALED)

setMethod("split", c("DBTABLES", "ANY", "logical"), function(x, f, drop) {
  id2pos <- function(x, i) structure(rep.int(i, length(x)), names = x)
  get_mapping <- function(x, key, text) {
    if (!is.list(x))
      stop("ordering problem encountered ", paste0(text, collapse = "->"))
    x <- lapply(lapply(x, `[[`, key), unique.default)
    x <- mapply(id2pos, x, seq_along(x), SIMPLIFY = FALSE, USE.NAMES = FALSE)
    unlist(x, FALSE, TRUE)
  }
  class.arg <- list(Class = class(x))
  result <- as.list(primary_keys(x))
  result[[1L]] <- split(slot(x, names(result)[1L]), f, TRUE)
  crs <- foreign_keys(x)
  for (i in seq_along(result)[-1L]) {
    this <- names(result)[i]
    cr <- crs[crs[, "from.tbl"] == this, , drop = FALSE][1L, ]
    mapping <- get_mapping(result[[cr[["to.tbl"]]]], cr[["to.col"]], cr)
    this <- slot(x, this)
    grps <- mapping[as.character(this[, cr[["from.col"]]])]
    result[[i]] <- split(this, grps, TRUE)
  }
  result <- lapply(seq_along(result[[1L]]), function(i) lapply(result, `[[`, i))
  result <- lapply(result, function(x) do.call(new, c(class.arg, x)))
  if (drop)
    result <- lapply(result, update, NULL, TRUE)
  result
}, sealed = SEALED)

#= oapply DBTABLES-methods

setMethod("oapply", "DBTABLES", function(object, fun, ...,
    simplify = TRUE) {
  tn <- names(primary_keys(object))
  mapply(FUN = fun, lapply(tn, slot, object = object), tn,
    MoreArgs = list(...), SIMPLIFY = simplify, USE.NAMES = TRUE)
}, sealed = SEALED)


################################################################################


#' Classes for opm database I/O
#'
#' These child classes of \code{\link{DBTABLES}} hold intermediary objects that
#' can be used for database input and output of \acronym{OPMX} objects as
#' created by \pkg{opm}.
#'
#' @details
#'   See the \pkg{opm} documentation for details on \code{OPMX} objects
#'   themselves. \pkg{opmDB} defines the following additional classes:
#'   \describe{
#'   \item{OPM_DB}{Holds all data that occur in an \acronym{OPM} object or in
#'   several such objects, as contained in an \acronym{OPMS} object.}
#'   \item{OPMA_DB}{The same for \acronym{OPMA} objects.}
#'   \item{OPMD_DB}{The same for \acronym{OPMD} objects.}
#'   }
#'   The inheritance relationships thus mirror those of the \code{OPMX} objects
#'   (with the exception of \acronym{OPMS}). Conversion with \code{as} is
#'   implemented from all \acronym{OPMX} classes to all classes defined here.
#'   List can also be converted provided they only contain \acronym{OPMX}
#'   objects (or lists of such objects).
#'
#'   Conversion in the other direction, yielding one of the \acronym{OPMX}
#'   classes, is also implemented. Attempting to convert several plates to an
#'   \code{OPMX} calls other than \code{OPMS} will yield an error, however, as
#'   well as trying to convert a single plate to \code{OPMS}, or several plates
#'   with distinct plate types. In contrast, conversion to a list will work in
#'   all instances, and such a list could further be processed with the
#'   \code{opms} function from the \pkg{opm} package, irrespective of the number
#'   of plates contained.
#'
#' @docType class
#' @export
#' @aliases OPM_DB-class
#' @seealso methods::Methods methods::new opm::opms
#' @family classes
#' @keywords methods classes
#' @examples
#'
#' library(opm)
#' (x <- as(vaas_1, "OPMD_DB"))
#' length(x)
#'
#' # conduct some checks
#' stopifnot(foreign_keys_valid(x), primary_keys_valid(x), length(x) == 1)
#'
#' # get first and last primary keys for each table
#' head(x)
#' tail(x)
#' stopifnot(head(x) == 1, tail(x) >= head(x))
#'
#' # increment the primary keys
#' offset <- 1:7 # one value per table
#' y <- update(x, offset)
#' head(y)
#' tail(y)
#' stopifnot(head(y) == head(x) + offset, tail(y) == tail(x) + offset)
#' stopifnot(foreign_keys_valid(y), primary_keys_valid(y))
#' y <- update(y, NULL) # revert the previous action
#' head(y)
#' tail(y)
#' stopifnot(identical(y, x))
#'
#' # combine data
#' (y <- c(x, x))
#' length(y)
#' stopifnot(length(y) == 2 * length(x), class(y) == class(x))
#' stopifnot(foreign_keys_valid(y), primary_keys_valid(y))
#'
#' # split them again
#' (y <- split(y))
#' stopifnot(identical(y, list(x, x)))
#' # i.e. approximate reverse of c(), but only under default settings
#'
#' ## with OPMS object as starting point
#' (x <- as(vaas_4, "OPMD_DB"))
#' length(x)
#' stopifnot(foreign_keys_valid(x), primary_keys_valid(x), length(x) == 4)
#' (y <- split(x))
#' stopifnot(is.list(y), sapply(y, class) ==  "OPMD_DB", length(y) == 4)
#' (y <- do.call(c, y)) # join the datasets again
#' stopifnot(identical(y, x))
#'
#' # conversions back and forth
#' (x <- as(vaas_1, "OPMD_DB"))
#' stopifnot(identical(as(x, "OPMA"), as(vaas_1, "OPMA")))
#' (y <- try(as(x, "OPMS"), silent = TRUE))
#' stopifnot(inherits(y, "try-error")) # does not work because only 1 plate
#' (x <- as(vaas_4, "OPM_DB"))
#' stopifnot(identical(as(x, "OPMS"), vaas_4[drop = TRUE]))
#' (y <- try(as(x, "OPM"), silent = TRUE)) # does not work because > 1 plate
#' stopifnot(inherits(y, "try-error"))
#' (y <- as(x, "list")) # one can always go through a list
#' stopifnot(sapply(y, is, "OPM")) # opms() could now be called
#'
setClass("OPM_DB",
  contains = "DBTABLES",
  representation = representation(plates = "data.frame",
    wells = "data.frame", measurements = "data.frame"),
  validity = foreign_keys_valid,
  sealed = SEALED
)

#' @rdname OPM_DB
#' @name OPMA_DB
#' @aliases OPMA_DB-class
#' @docType class
#' @export
#'
setClass("OPMA_DB",
  contains = "OPM_DB",
  # the superclass slots must be repeated here to enforce the ordering
  representation = representation(plates = "data.frame",
    wells = "data.frame", measurements = "data.frame",
    aggr_settings = "data.frame", aggregated = "data.frame"),
  validity = foreign_keys_valid,
  sealed = SEALED
)

#' @rdname OPM_DB
#' @name OPMD_DB
#' @aliases OPMD_DB-class
#' @docType class
#' @export
#'
setClass("OPMD_DB",
  contains = "OPMA_DB",
  # the superclass slots must be repeated here to enforce the ordering
  representation = representation(plates = "data.frame",
    wells = "data.frame", measurements = "data.frame",
    aggr_settings = "data.frame", aggregated = "data.frame",
    disc_settings = "data.frame", discretized = "data.frame"),
  validity = foreign_keys_valid,
  sealed = SEALED
)


################################################################################
#
# Conversions with as()
#


setAs("OPM", "OPM_DB", function(from) {
  x <- forward_OPM_to_list(from)
  new("OPM_DB", plates = x$plates, wells = x$wells,
    measurements = x$measurements)
})

setAs("OPM_DB", "OPM", function(from) {
  as(backward_OPM_to_list(from), "OPM")
})

setAs("OPMA", "OPMA_DB", function(from) {
  x <- forward_OPMA_to_list(from)
  new("OPMA_DB", plates = x$plates, wells = x$wells,
    measurements = x$measurements, aggr_settings = x$aggr_settings,
    aggregated = x$aggregated)
})

setAs("OPMA_DB", "OPMA", function(from) {
  as(backward_OPMA_to_list(from), "OPMA")
})

setAs("OPMD", "OPMD_DB", function(from) {
  x <- forward_OPMA_to_list(from)
  dsets <- settings_forward(from@disc_settings, x$plates[, "id"])
  ddata <- from@discretized
  ddata <- data.frame(id = seq_along(ddata), stringsAsFactors = FALSE,
    well_id = match(names(ddata), x$wells[, "coordinate"]),
    disc_setting_id = 1L, value = unname(ddata), check.names = FALSE)
  new("OPMD_DB", plates = x$plates, wells = x$wells,
    measurements = x$measurements, aggr_settings = x$aggr_settings,
    aggregated = x$aggregated, disc_settings = dsets, discretized = ddata)
})

setAs("OPMD_DB", "OPMD", function(from) {
  as(backward_OPMD_to_list(from), "OPMD")
})


################################################################################
#
# Conversion to and from lists
#


setAs("list", "OPM_DB", function(from) {
  do.call(c, rapply(object = from, f = as, Class = "OPM_DB"))
})

setAs("list", "OPMA_DB", function(from) {
  do.call(c, rapply(object = from, f = as, Class = "OPMA_DB"))
})

setAs("list", "OPMD_DB", function(from) {
  do.call(c, rapply(object = from, f = as, Class = "OPMD_DB"))
})

setAs("OPM_DB", "list", function(from) {
  lapply(split(from), as, "OPM")
})

setAs("OPMA_DB", "list", function(from) {
  lapply(split(from), as, "OPMA")
})

setAs("OPMD_DB", "list", function(from) {
  lapply(split(from), as, "OPMD")
})


################################################################################
#
# Conversion to and from OPMS objects
#


setAs("OPMS", "OPM_DB", function(from) {
  do.call(c, lapply(from@plates, as, "OPM_DB"))
})

setAs("OPMS", "OPMA_DB", function(from) {
  do.call(c, lapply(from@plates, as, "OPMA_DB"))
})

setAs("OPMS", "OPMD_DB", function(from) {
  do.call(c, lapply(from@plates, as, "OPMD_DB"))
})

setAs("OPM_DB", "OPMS", function(from) {
  as(lapply(split(from), backward_OPM_to_list), "OPMS")
})

setAs("OPMA_DB", "OPMS", function(from) {
  as(lapply(split(from), backward_OPMA_to_list), "OPMS")
})

setAs("OPMD_DB", "OPMS", function(from) {
  as(lapply(split(from), backward_OPMD_to_list), "OPMS")
})


################################################################################


