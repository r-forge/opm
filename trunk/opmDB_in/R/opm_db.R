

################################################################################


#' Classes for \pkg{opm} database I/O
#'
#' These child classes of \code{\link{DBTABLES}} hold intermediary objects that
#' can be used for database input and output of \acronym{OPMX} objects. These
#' classes are not normally directly dealt with by an \pkg{opm} user but are
#' documented here for completeness. See \code{\link{opm_dbput}} for methods
#' that internally use these classes for database I/O.
#'
#' @details
#'   See their documentation for details on \acronym{OPMX} objects themselves.
#'   We here define the following additional classes: \describe{
#'   \item{OPM_DB}{Holds all data that occur in an \acronym{OPM} object, or in
#'   several such objects as contained in an \acronym{OPMS} object.}
#'   \item{OPMA_DB}{Holds all data that occur in an \acronym{OPMA} object, or in
#'   several such objects as contained in an \acronym{OPMS} object.}
#'   \item{OPMD_DB}{Holds all data that occur in an \acronym{OPMD} object, or in
#'   several such objects as contained in an \acronym{OPMS} object.}
#'   }
#'   The inheritance relationships thus mirror those of the \code{OPMX} objects
#'   (with the exception of \acronym{OPMS}). Conversion with \code{as} is
#'   implemented from all \acronym{OPMX} classes to all classes defined here.
#'   Lists can also be converted provided they only contain \acronym{OPMX}
#'   objects (or lists of such objects).
#'
#'   Conversion in the other direction, yielding one of the \acronym{OPMX}
#'   classes, is also implemented. Attempting to convert several plates to an
#'   \code{OPMX} class other than \code{OPMS} will yield an error, however, as
#'   well as trying to convert a single plate to \code{OPMS}, or several plates
#'   with distinct plate types. In contrast, conversion to a list will work in
#'   all instances, and such a list could further be processed with the
#'   \code{opms} function from the \pkg{opm} package, irrespective of the number
#'   of plates contained.
#'
#'   In contrast to the \acronym{OPMX} classes, the three ones defined here can
#'   be created using \code{new}, yielding empty objects. These can neither be
#'   converted to \acronym{OPMX} objects nor combined with them using
#'   \code{\link{c}}. Instead, they are useful in conjunction with
#'   \code{\link{by}} from the \pkg{pkgutils} packages with \code{do_inline} set
#'   to \code{TRUE}. They contain all \code{\link{fkeys}} information and can be
#'   filled using a suitable \code{FUN} argument.
#'
#' @docType class
#' @export
#' @name OPM_DB-classes
#' @aliases OPM_DB
#' @aliases OPM_DB-class
#' @seealso methods::Methods methods::new opm::opms
#' @family opm_db-functions
#' @keywords methods classes database
#' @examples
#'
#' library(pkgutils)
#' library(opm)
#' ## conversions back and forth, OPMD as starting point
#' (x <- as(vaas_1, "OPMD_DB"))
#' (y <- as(x, "OPMD"))
#' stopifnot(
#'   dim(y) == dim(vaas_1),
#'   # numeric data remain except for rounding errors:
#'   all.equal(measurements(y), measurements(vaas_1)),
#'   all.equal(aggregated(y), aggregated(vaas_1)),
#'   all.equal(discretized(y), discretized(vaas_1)),
#'   # file names get normalized, hence CSV dat may get unequal:
#'   !isTRUE(all.equal(csv_data(y), csv_data(vaas_1)))
#' )
#' (y <- try(as(x, "OPMS"), silent = TRUE))
#' stopifnot(inherits(y, "try-error")) # does not work because only 1 plate
#'
#' ## conversions back and forth, OPMS as starting point
#' (x <- as(vaas_4, "OPMD_DB"))
#' (y <- as(x, "OPMS"))
#' stopifnot(sapply(1:length(y), # same values
#'   function(i) dim(y[i]) == dim(vaas_4[i])))
#' (y <- try(as(x, "OPMD"), silent = TRUE)) # does not work because > 1 plate
#' stopifnot(inherits(y, "try-error"))
#' (y <- as(x, "list")) # one can always go through a list
#' stopifnot(sapply(y, is, "OPMD")) # opms() could now be called
#'
#' ## one can create new objects without data
#' (y <- new("OPMD_DB"))
#' stopifnot(fkeys_valid(y), fkeys(y) == fkeys(x), !length(y))
#' # such objects cannot be converted to OPMX but can be filled using by()
#'
setClass("OPM_DB",
  contains = "DBTABLES",
  slots = c(plates = "data.frame", wells = "data.frame",
    measurements = "data.frame"),
  prototype = list(plates = data.frame(id = integer(), machine_id = integer()),
    wells = data.frame(id = integer(), plate_id = integer()),
    measurements = data.frame(id = integer(), well_id = integer())),
  validity = fkeys_valid,
  sealed = SEALED
)

#' @rdname OPM_DB-classes
#' @name OPMA_DB
#' @aliases OPMA_DB-class
#' @docType class
#' @export
#'
setClass("OPMA_DB",
  contains = "OPM_DB",
  # the superclass slots must be repeated here to enforce the ordering
  slots = c(plates = "data.frame", wells = "data.frame",
    measurements = "data.frame", aggr_settings = "data.frame",
    aggregated = "data.frame"),
  prototype = list(plates = data.frame(id = integer(), machine_id = integer()),
    wells = data.frame(id = integer(), plate_id = integer()),
    measurements = data.frame(id = integer(), well_id = integer()),
    aggr_settings = data.frame(id = integer(), plate_id = integer()),
    aggregated = data.frame(id = integer(), well_id = integer(),
      aggr_setting_id = integer())),
  validity = fkeys_valid,
  sealed = SEALED
)

#' @rdname OPM_DB-classes
#' @name OPMD_DB
#' @aliases OPMD_DB-class
#' @docType class
#' @export
#'
setClass("OPMD_DB",
  contains = "OPMA_DB",
  # the superclass slots must be repeated here to enforce the ordering
  slots = c(plates = "data.frame", wells = "data.frame",
    measurements = "data.frame", aggr_settings = "data.frame",
    aggregated = "data.frame", disc_settings = "data.frame",
    discretized = "data.frame"),
  prototype = list(plates = data.frame(id = integer(), machine_id = integer()),
    wells = data.frame(id = integer(), plate_id = integer()),
    measurements = data.frame(id = integer(), well_id = integer()),
    aggr_settings = data.frame(id = integer(), plate_id = integer()),
    aggregated = data.frame(id = integer(), well_id = integer(),
      aggr_setting_id = integer()),
    disc_settings = data.frame(id = integer(), plate_id = integer()),
    discretized = data.frame(id = integer(), well_id = integer(),
      disc_setting_id = integer())),
  validity = fkeys_valid,
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


setOldClass("RODBC")


#' Database I/O for \pkg{opm}
#'
#' Methods for inserting, querying and deleting \code{\link{OPMX}} object into
#' or from (\acronym{SQL}-based) relational databases. A common database scheme
#' is assumed as defined in the auxiliary \acronym{SQL} files of this package,
#' but tables could be named differently, and columns could be added containing
#' user-defined combinations of metadata.
#'
#' @param object \code{\link{OPMX}} or \code{\link{OPM_DB}} object, integer
#'   vector containing real or potential primary keys of a database table, or
#'   character scalar containing an \acronym{SQL} query.
#' @param conn Database connection object. Currently \code{DBIConnection} object
#'   from one of the reverse dependencies of \pkg{DBI} (recommended) and
#'   \code{RODBC} objects as created by the \pkg{RODBC} package are supported.
#' @param map_tables Passed as \code{do_map} argument to \code{\link{by}}.
#' @param include Integer scalar indicating whether aggregated data (1) or
#'   aggregated and discretised data (2) should be added to the result.
#' @param start Integer vector determining the minimum primary keys to which
#'   those in \code{object} should be coerced. Necessary for appending to a
#'   database table without overwriting previously inserted data.
#' @param metadata Empty or data frame with metadata to be added to the check
#'   object \code{vaas_4}. If a data frame, it must contain exactly 2 rows.
#'   Adding metadata makes only sense if according columns have been added to
#'   the database table for the plates; see the examples below. The original
#'   metadata from \code{vaas_4} are always removed.
#' @param time.points Index of one to several time points. Selection speeds up
#'   database I/O during checking.
#' @param wells Index of one to several wells. Selection speeds up database I/O
#'   during checking.
#' @param ... Optional arguments passed between the methods.
#'
#' @details The \code{DBIConnection} methods send table and column names are
#'   through \code{make.db.names} from the \pkg{DBI} package or its dependencies
#'   before including them into \acronym{SQL} queries, if any. As dictated by
#'   \code{by} from the \pkg{pkgutils} packages, this is done after applying
#'   \code{map_tables}. The \code{RODBC} methods use a simple quoting scheme.
#'
#'   \code{opm_dbcheck} attempts to insert, query and delete the first two
#'   plates from the object \code{vaas_4} into the database. If everything is
#'   correctly set up, this should work without error \strong{unless} these two
#'   plates from \code{vaas_4} have already been inserted. If errors occur, it
#'   is up to the user to clean up the data base (as far as necessary).
#'
#' @return
#'   \code{opm_dbget} returns an \code{\link{OPMX}} object or \code{NULL}.
#'
#'   \code{opm_dbput} returns an integer vector containing the primary keys of
#'   the inserted plates.
#'
#'   \code{opm_dbnext} returns an integer vector that is suitable as
#'   \code{start} argument of \code{opm_dbput}.
#'
#'   \code{opm_dbclear} invisibly returns the result of \code{dbGetQuery} (which
#'   is usually \code{NULL}).
#'
#'   \code{opm_dbcheck} returns a character vector whose elements are either
#'   \kbd{ok} or a description of the error that has occurred at that step
#'   of the checking process.
#'
#' @family opm_db-functions
#' @seealso DBI::make.db.names pkgutils::by
#' @keywords database
#' @export
#' @examples
#' # The SQL files for generating the expected database tables. Tables can
#' # be renamed, but the an according 'map_tables' argument must be used.
#' pkgutils::pkg_files("opmDB", "auxiliary")
#'
#' # Usage examples are given in these demos. An according database must be
#' # made accessible beforehand.
#' demo(package = "opmDB")
#'
setGeneric("opm_dbput",
  function(object, conn, ...) standardGeneric("opm_dbput"))

setMethod("opm_dbput", c("OPM_DB", "DBIConnection"), function(object, conn,
    map_tables = NULL, start = opm_dbnext(object, conn, map_tables)) {
  object <- update(object, start, TRUE)
  by(object, TRUE, dbWriteTable, conn = conn, append = TRUE, row.names = FALSE,
    do_quote = function(x) make.db.names(conn, x), do_map = map_tables)
  object@plates[, "id"]
}, sealed = SEALED)

setMethod("opm_dbput", c("OPM_DB", "RODBC"), function(object, conn,
    map_tables = NULL, start = opm_dbnext(object, conn, map_tables)) {
  object <- update(object, start, TRUE)
  by(object, TRUE, function(n, x, ...) sqlSave(dat = x, tablename = n, ...),
    channel = conn, append = TRUE, test = FALSE, rownames = FALSE, fast = TRUE,
    verbose = FALSE, do_quote = if (attr(conn, "isMySQL"))
      "`"
    else
      "\"", do_map = map_tables, simplify = FALSE)
  object@plates[, "id"]
}, sealed = FALSE)

setMethod("opm_dbput", c("OPM", "ANY"), function(object, conn, ...) {
  opm_dbput(as(object, paste0(class(object), "_DB")), conn, ...)
})

setMethod("opm_dbput", c("OPMS", "ANY"), function(object, conn, ...) {
  klass <- if (all(has_disc(object)))
      "OPMD"
    else if (all(has_aggr(object)))
      "OPMA"
    else
      "OPM"
  opm_dbput(as(object, paste0(klass, "_DB")), conn, ...)
})

#= opm_dbget opm_dbput

#' @rdname opm_dbput
#' @export
#'
setGeneric("opm_dbget",
  function(object, conn, ...) standardGeneric("opm_dbget"))

setMethod("opm_dbget", c("integer", "DBIConnection"), function(object, conn,
    map_tables = NULL, include = 2L) {
  db2opmx(by(int2dbclass(include), object, dbGetQuery, conn = conn,
    do_map = map_tables, do_inline = TRUE, simplify = TRUE,
    do_quote = function(x) make.db.names(conn, x)))
}, sealed = SEALED)

setMethod("opm_dbget", c("character", "DBIConnection"), function(object, conn,
    map_tables = NULL, include = 2L) {
  db2opmx(by(int2dbclass(include), object, dbGetQuery, conn = conn,
    do_map = map_tables, do_inline = TRUE, simplify = FALSE,
    do_quote = function(x) make.db.names(conn, x)))
}, sealed = SEALED)

setMethod("opm_dbget", c("integer", "RODBC"), function(object, conn,
    map_tables = NULL, include = 2L) {
  db2opmx(by(int2dbclass(include), object, sqlQuery, channel = conn,
    do_map = map_tables, do_inline = TRUE, do_quote = if (attr(conn, "isMySQL"))
      "`"
    else
      "\"", stringsAsFactors = FALSE, simplify = TRUE))
}, sealed = FALSE)

setMethod("opm_dbget", c("character", "RODBC"), function(object, conn,
    map_tables = NULL, include = 2L) {
  db2opmx(by(int2dbclass(include), object, sqlQuery, channel = conn,
    do_map = map_tables, do_inline = TRUE, do_quote = if (attr(conn, "isMySQL"))
      "`"
    else
      "\"", stringsAsFactors = FALSE, simplify = FALSE))
}, sealed = FALSE)

#= opm_dbnext opm_dbput

#' @rdname opm_dbput
#' @export
#'
setGeneric("opm_dbnext",
  function(object, conn, ...) standardGeneric("opm_dbnext"))

setMethod("opm_dbnext", c("OPM", "ANY"), function(object, conn, ...) {
  opm_dbnext(as(object, paste0(class(object), "_DB")), conn, ...)
})

setMethod("opm_dbnext", c("OPMS", "ANY"), function(object, conn, ...) {
  opm_dbnext(all(has_disc(object)) + all(has_aggr(object)), conn, ...)
})

setMethod("opm_dbnext", c("integer", "ANY"), function(object, conn,
    map_tables = NULL) {
  opm_dbnext(int2dbclass(object), conn, map_tables)
}, sealed = SEALED)

setMethod("opm_dbnext", c("OPM_DB", "DBIConnection"), function(object, conn,
    map_tables = NULL) {
  get_last <- function(tn, id, conn) {
    sql <- sprintf("SELECT max(%s) FROM %s;", make.db.names(conn, id),
      make.db.names(conn, tn))
    dbGetQuery(conn, sql)
  }
  db2ids(by(object, TRUE, get_last, conn = conn, do_map = map_tables,
    do_inline = FALSE, simplify = TRUE))
}, sealed = SEALED)

setMethod("opm_dbnext", c("OPM_DB", "RODBC"), function(object, conn,
    map_tables = NULL) {
  get_last <- function(tn, id, conn, char) {
    sql <- sprintf("SELECT max(%s) FROM %s;",
      quote_protected(id, char), quote_protected(tn, char))
    sqlQuery(conn, sql)
  }
  db2ids(by(object, TRUE, get_last, conn = conn,
    char = if (attr(conn, "isMySQL"))
      "`"
    else
      "\"", do_map = map_tables, do_inline = FALSE, simplify = TRUE))
}, sealed = FALSE)

#= opm_dbclear opm_dbput

#' @rdname opm_dbput
#' @export
#'
setGeneric("opm_dbclear",
  function(object, conn, ...) standardGeneric("opm_dbclear"))

setMethod("opm_dbclear", c("integer", "DBIConnection"), function(object, conn,
    map_tables = NULL) {
  pk <- pkeys(new("OPM_DB"))[1L]
  sql <- sprintf("DELETE FROM %s WHERE %s;", make.db.names(conn, names(pk)),
    paste(make.db.names(conn, pk), object, sep = " = ", collapse = " OR "))
  invisible(dbGetQuery(conn, sql))
}, sealed = SEALED)

setMethod("opm_dbclear", c("integer", "RODBC"), function(object, conn,
    map_tables = NULL) {
  pk <- pkeys(new("OPM_DB"))[1L]
  char = if (attr(conn, "isMySQL"))
      "`"
    else
      "\""
  sql <- sprintf("DELETE FROM %s WHERE %s;", quote_protected(names(pk), char),
    paste(quote_protected(pk, char), object, sep = " = ", collapse = " OR "))
  invisible(sqlQuery(conn, sql))
}, sealed = SEALED)

#= opm_dbcheck opm_dbput

#' @rdname opm_dbput
#' @export
#'
setGeneric("opm_dbcheck", function(conn, ...) standardGeneric("opm_dbcheck"))

setMethod("opm_dbcheck", "ANY", function(conn, metadata = NULL,
    time.points = TRUE, wells = TRUE) {
  slots_equal <- function(a, b) {
    fmt <- "%Y-%m-%d %H:%M:%S"
    old <- opm_opt("time.fmt")
    if (!fmt %in% old) {
      on.exit(opm_opt(time.fmt = old))
      opm_opt(time.fmt = c(fmt, old))
    }
    c(sapply(setdiff(slotNames(a), "csv_data"),
      function(n) all.equal(slot(a, n), slot(b, n))),
      all.equal(csv_data(a, normalize = TRUE), csv_data(b, normalize = TRUE)))
  }
  x <- vaas_4[1L:2L, time.points, wells]
  metadata(x) <- structure(list(), names = character())
  if (length(metadata))
    if (is.data.frame(metadata))
      metadata(x) <- metadata
    else
      stop("'metadata' must be empty or a data frame")
  result <- c(insert = NA, receive = NA, clear = NA, compare = NA)
  storage.mode(result) <- "character"
  step <- 0L
  tryCatch({
    ids <- opm_dbput(x, conn)
    result[[step <- step + 1L]] <- "ok"
    y <- opm_dbget(ids, conn)
    result[[step <- step + 1L]] <- "ok"
    opm_dbclear(ids, conn)
    result[[step <- step + 1L]] <- "ok"
    cmp <- sapply(seq_along(x), function(i) slots_equal(y[i], x[i]))
    if (!is.logical(cmp))
      stop(paste0(cmp, collapse = " / "))
    result[[step <- step + 1L]] <- "ok"
  }, error = function(e) result[[step + 1L]] <<- conditionMessage(e))
  result
}, sealed = SEALED)


################################################################################


