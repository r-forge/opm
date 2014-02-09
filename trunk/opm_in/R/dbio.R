

################################################################################
################################################################################
#
# Database I/O
#


setOldClass("RODBC")


#' Database I/O for \pkg{opm}
#'
#' Methods for inserting, querying and deleting \code{\link{OPMX}} objects into
#' or from (\acronym{SQL}-based) relational databases. A common database scheme
#' is assumed as defined in the auxiliary \acronym{SQL} files of this package
#' (run \code{\link{opm_files}} in \code{"sql"} mode), but tables could be named
#' differently, and columns could be added containing user-defined combinations
#' of metadata.
#'
#' @param object \code{\link{OPMX}} or \code{\link{OPM_DB}} object, integer
#'   vector containing real or potential primary keys of a database table, or
#'   character scalar containing a partial \acronym{SQL} query (the part after
#'   the \sQuote{WHERE} keyword).
#' @param conn Database connection object. Currently \code{DBIConnection} object
#'   from one of the reverse dependencies of \pkg{DBI} (recommended) and
#'   \code{RODBC} objects as created by the \pkg{RODBC} package are supported.
#' @param map.tables Passed as \code{do_map} argument to \code{\link{by}}.
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
#'   \code{map.tables}. The \code{RODBC} methods use a simple quoting scheme.
#'
#'   \code{opm_dbcheck} attempts to insert, query and delete the first two
#'   plates from the object \code{vaas_4} into the database. If everything is
#'   correctly set up, this should work without error \strong{unless} these two
#'   plates from \code{vaas_4} have already been inserted. If errors occur, it
#'   is up to the user to clean up the data base (as far as necessary).
#'
#'   Note that the deletion mechanism is based on \code{ON DELETE CASCADE}. To
#'   enable this in \code{SQLite}, \code{PRAGMA foreign_keys = ON;} has to be
#'   called each time a database is opened.
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
#' @family dbio-functions
#' @seealso DBI::make.db.names pkgutils::by
#' @keywords database
#' @export
#' @examples
#' # The SQL files for generating the expected database tables. Tables can
#' # be renamed, but then an according 'map.tables' argument must be used.
#' opm_files("sql")
#'
#' # Usage examples are given in these demos. An according database must be
#' # made accessible beforehand.
#' if (interactive())
#'   demo(package = "opm")
#'
setGeneric("opm_dbput",
  function(object, conn, ...) standardGeneric("opm_dbput"))

setMethod("opm_dbput", c("OPM_DB", "DBIConnection"), function(object, conn,
    map.tables = NULL, start = opm_dbnext(object, conn, map.tables)) {
  object <- update(object, start, TRUE)
  by(object, TRUE, dbWriteTable, conn = conn, append = TRUE, row.names = FALSE,
    do_quote = function(x) make.db.names(conn, x), do_map = map.tables)
  object@plates[, "id"]
}, sealed = SEALED)

setMethod("opm_dbput", c("OPM_DB", "RODBC"), function(object, conn,
    map.tables = NULL, start = opm_dbnext(object, conn, map.tables)) {
  object <- update(object, start, TRUE)
  by(object, TRUE, function(n, x, ...) sqlSave(dat = x, tablename = n, ...),
    channel = conn, append = TRUE, test = FALSE, rownames = FALSE, fast = TRUE,
    verbose = FALSE, do_quote = if (attr(conn, "isMySQL"))
      "`"
    else
      "\"", do_map = map.tables, simplify = FALSE)
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

#= opm_dbfind opm_dbput

#' @rdname opm_dbput
#' @export
#'
setGeneric("opm_dbfind",
  function(object, conn, ...) standardGeneric("opm_dbfind"))

setMethod("opm_dbfind", c("character", "DBIConnection"), function(object, conn,
    map.tables = NULL) {
  pk <- pkeys(new("OPM_DB"))[1L]
  sql <- sprintf("SELECT %s FROM %s WHERE %s;", make.db.names(conn, pk),
    make.db.names(conn, map_values(names(pk), map.tables)), object)
  ids <- dbGetQuery(conn, sql)
  if (ncol(ids))
    as.integer(ids[, 1L])
  else
    integer()
}, sealed = SEALED)

setMethod("opm_dbfind", c("character", "RODBC"), function(object, conn,
    map.tables = NULL) {
  pk <- pkeys(new("OPM_DB"))[1L]
  char <- if (attr(conn, "isMySQL"))
      "`"
    else
      "\""
  sql <- sprintf("SELECT %s FROM %s WHERE %s;", quote_protected(pk, char),
    quote_protected(map_values(names(pk), map.tables), char), object)
  ids <- sqlQuery(conn, sql)
  if (ncol(ids))
    as.integer(ids[, 1L])
  else
    integer()
}, sealed = SEALED)

#= opm_dbget opm_dbput

#' @rdname opm_dbput
#' @export
#'
setGeneric("opm_dbget",
  function(object, conn, ...) standardGeneric("opm_dbget"))

setMethod("opm_dbget", c("integer", "DBIConnection"), function(object, conn,
    map.tables = NULL, include = 2L) {
  db2opmx(by(int2dbclass(include), object, dbGetQuery, conn = conn,
    do_map = map.tables, do_inline = TRUE, simplify = TRUE,
    do_quote = function(x) make.db.names(conn, x)))
}, sealed = SEALED)

setMethod("opm_dbget", c("integer", "RODBC"), function(object, conn,
    map.tables = NULL, include = 2L) {
  db2opmx(by(int2dbclass(include), object, sqlQuery, channel = conn,
    do_map = map.tables, do_inline = TRUE, do_quote = if (attr(conn, "isMySQL"))
      "`"
    else
      "\"", stringsAsFactors = FALSE, simplify = TRUE))
}, sealed = FALSE)

setMethod("opm_dbget", c("character", "ANY"), function(object, conn,
    map.tables = NULL, include = 2L) {
  opm_dbget(opm_dbfind(object, conn, map.tables), conn, map.tables, include)
}, sealed = SEALED)

#= opm_dbnext opm_dbput

#' @rdname opm_dbput
#' @export
#'
setGeneric("opm_dbnext",
  function(object, conn, ...) standardGeneric("opm_dbnext"))

setMethod("opm_dbnext", c("OPM", "ANY"), function(object, conn, ...) {
  opm_dbnext(has_disc(object) + has_aggr(object), conn, ...)
})

setMethod("opm_dbnext", c("OPMS", "ANY"), function(object, conn, ...) {
  opm_dbnext(all(has_disc(object)) + all(has_aggr(object)), conn, ...)
})

setMethod("opm_dbnext", c("integer", "ANY"), function(object, conn,
    map.tables = NULL) {
  opm_dbnext(int2dbclass(object), conn, map.tables)
}, sealed = SEALED)

setMethod("opm_dbnext", c("OPM_DB", "DBIConnection"), function(object, conn,
    map.tables = NULL) {
  get_last <- function(tn, id, conn) {
    sql <- sprintf("SELECT max(%s) FROM %s;", make.db.names(conn, id),
      make.db.names(conn, tn))
    dbGetQuery(conn, sql)
  }
  db2ids(by(object, TRUE, get_last, conn = conn, do_map = map.tables,
    do_inline = FALSE, simplify = TRUE))
}, sealed = SEALED)

setMethod("opm_dbnext", c("OPM_DB", "RODBC"), function(object, conn,
    map.tables = NULL) {
  get_last <- function(tn, id, conn, char) {
    sql <- sprintf("SELECT max(%s) FROM %s;",
      quote_protected(id, char), quote_protected(tn, char))
    sqlQuery(conn, sql)
  }
  db2ids(by(object, TRUE, get_last, conn = conn,
    char = if (attr(conn, "isMySQL"))
      "`"
    else
      "\"", do_map = map.tables, do_inline = FALSE, simplify = TRUE))
}, sealed = FALSE)

#= opm_dbclear opm_dbput

#' @rdname opm_dbput
#' @export
#'
setGeneric("opm_dbclear",
  function(object, conn, ...) standardGeneric("opm_dbclear"))

setMethod("opm_dbclear", c("character", "ANY"), function(object, conn,
    map.tables = NULL) {
  opm_dbclear(opm_dbfind(object, conn, map.tables), conn, map.tables)
}, sealed = SEALED)

setMethod("opm_dbclear", c("integer", "DBIConnection"), function(object, conn,
    map.tables = NULL) {
  pk <- pkeys(new("OPM_DB"))[1L]
  sql <- sprintf("DELETE FROM %s WHERE %s;", make.db.names(conn,
    map_values(names(pk), map.tables)),
    paste(make.db.names(conn, pk), object, sep = " = ", collapse = " OR "))
  invisible(dbGetQuery(conn, sql))
}, sealed = SEALED)

setMethod("opm_dbclear", c("integer", "RODBC"), function(object, conn,
    map.tables = NULL) {
  pk <- pkeys(new("OPM_DB"))[1L]
  char <- if (attr(conn, "isMySQL"))
      "`"
    else
      "\""
  sql <- sprintf("DELETE FROM %s WHERE %s;",
    quote_protected(map_values(names(pk), map.tables), char),
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
  result <- c(last1 = NA, insert = NA, receive = NA, clear = NA, compare = NA,
    last2 = NA, samelast = NA)
  storage.mode(result) <- "character"
  step <- 0L
  tryCatch({
    last1 <- opm_dbnext(2L, conn)
    result[[step <- step + 1L]] <- "ok"
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
    last2 <- opm_dbnext(2L, conn)
    result[[step <- step + 1L]] <- "ok"
    stopifnot(last1 == last2)
    result[[step <- step + 1L]] <- "ok"
  }, error = function(e) result[[step + 1L]] <<- conditionMessage(e))
  result
}, sealed = SEALED)


################################################################################


#' Database I/O object conversion helper functions
#'
#' Internal helper functions for the \code{as} methods for the database I/O
#' helper objects defined in this package.
#'
#' @param from List or one of the S4 objects defined in this package.
#' @param x List with aggregation or discretization settings.
#' @param plate.id Integer scalar.
#' @return List.
#' @keywords internal
#'
settings_forward <- function(x, plate.id) {
  x$options <- toJSON(x$options)
  x <- x[c("software", "version", "method", "options")]
  data.frame(id = 1L, plate_id = plate.id, x, stringsAsFactors = FALSE,
    check.names = FALSE)
}

#' @rdname settings_forward
#'
settings_backward <- function(x) {
  x <- x[, c("method", "options", "software", "version"), drop = TRUE]
  x$options <- fromJSON(x$options)
  x
}

#' @rdname settings_forward
#'
forward_OPM_to_list <- function(from) {
  p <- data.frame(id = 1L, plate_type = plate_type(from),
    setup_time = csv_data(from, what = "setup_time", normalize = TRUE),
    position = csv_data(from, what = "position", normalize = TRUE),
    machine_id = opm_opt("machine.id"), stringsAsFactors = FALSE,
    csv_data = toJSON(csv_data(from, normalize = TRUE)), check.names = FALSE)
  if (length(md <- from@metadata)) {
    if (any(bad <- names(md) %in% colnames(p)))
      stop("use of forbidden metadata name: ", names(md)[bad][1L])
    p <- cbind(p, md, stringsAsFactors = FALSE)
  }
  w <- wells(from)
  w <- data.frame(id = seq_along(w), plate_id = 1L, coordinate = w,
    stringsAsFactors = FALSE, check.names = FALSE)
  m <- opm::flatten(object = from, numbers = TRUE)
  names(m) <- map_values(names(m), MEASUREMENT_COLUMN_MAP)
  m <- cbind(id = seq.int(nrow(m)), m[, MEASUREMENT_COLUMN_MAP])
  list(plates = p, wells = w, measurements = m)
}

#' @rdname settings_forward
#'
backward_OPM_to_list <- function(from) {
  to_measurements <- function(m, w) {
    m[, "well_id"] <- w[match(m[, "well_id"], w[, "id"]), "coordinate"]
    m <- split.data.frame(m, m[, "well_id"]) # <= probably most time-consuming
    c(list(Hour = sort.int(m[[1]][, "time"])),
      lapply(m, function(x) x[order(x[, "time"]), "value"]))
  }
  if (nrow(p <- from@plates) != 1L)
    stop("object does not contain a single plate")
  list(measurements = to_measurements(from@measurements, from@wells),
    metadata = p[, setdiff(colnames(p), c("id", "plate_type", "setup_time",
      "position", "machine_id", "csv_data")), drop = TRUE],
    csv_data = unlist(fromJSON(p[, "csv_data"])))
}

#' @rdname settings_forward
#'
forward_OPMA_to_list <- function(from) {
  aggr_forward <- function(x, coords) data.frame(id = seq_along(x),
    well_id = rep(match(colnames(x), coords), each = nrow(x)),
    aggr_setting_id = 1L, parameter = rownames(x), value = as.vector(x),
    check.names = FALSE, stringsAsFactors = FALSE)
  x <- forward_OPM_to_list(from)
  x$aggregated <- aggr_forward(from@aggregated, x$wells[, "coordinate"])
  x$aggr_settings <- settings_forward(from@aggr_settings, x$plates[, "id"])
  x
}

#' @rdname settings_forward
#'
backward_OPMA_to_list <- function(from) {
  aggr_backward <- function(a, w) {
    a[, "well_id"] <- w[match(a[, "well_id"], w[, "id"]), "coordinate"]
    a <- split.data.frame(a, a[, "well_id"])
    lapply(a, function(x) structure(x[, "value"], names = x[, "parameter"]))
  }
  c(backward_OPM_to_list(from), list(
    aggr_settings = settings_backward(from@aggr_settings),
    aggregated = aggr_backward(from@aggregated, from@wells)))
}

#' @rdname settings_forward
#'
backward_OPMD_to_list <- function(from) {
  disc_backward <- function(d, w) as.list(structure(as.logical(d[, "value"]),
    names = w[match(d[, "well_id"], w[, "id"]), "coordinate"]))
  c(backward_OPMA_to_list(from), list(
    disc_settings = settings_backward(from@disc_settings),
    discretized = disc_backward(from@discretized, from@wells)))
}


################################################################################


#' Database I/O helper functions
#'
#' Internal helper functions for the databsse I/O methods.
#'
#' @param x Character or integer vector.
#' @param s Character scalar.
#' @return Character or integer vector or \code{OPMX} object or \code{NULL}.
#' @keywords internal
#'
quote_protected <- function(x, s) {
  sprintf(sprintf("%s%%s%s", s, s),
    gsub(s, sprintf("%s%s", s, s), x, FALSE, FALSE, TRUE))
}

#' @rdname quote_protected
#'
int2dbclass <- function(x) {
  new(paste0(case(x, "OPM", "OPMA", "OPMD"), "_DB"))
}

#' @rdname quote_protected
#'
db2opmx <- function(x) {
  x <- as(x, "list")
  case(length(x), NULL, x[[1L]], new("OPMS", plates = x))
}

#' @rdname quote_protected
#'
db2ids <- function(x) {
  x <- unlist(x, FALSE, FALSE)
  storage.mode(x) <- "integer"
  x[is.na(x)] <- 0L
  x + 1L
}


################################################################################


