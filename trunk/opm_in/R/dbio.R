

################################################################################
################################################################################
#
# Database I/O
#


#' Database I/O for \pkg{opm}
#'
#' Methods for inserting, querying and deleting \code{\link{OPMX}} objects into
#' or from (\acronym{SQL}-based) relational databases. A common database scheme
#' is assumed as defined in the auxiliary \acronym{SQL} files of this package
#' (run \code{\link{opm_files}} in \code{"sql"} mode), but tables could be named
#' differently, and columns could be added containing user-defined combinations
#' of metadata.
#'
#' @param object \code{\link{OPMX}}, \code{\link{MOPMX}} or \code{\link{OPM_DB}}
#'   object, integer vector containing real or potential primary keys of a
#'   database table, or character scalar containing a partial \acronym{SQL}
#'   query (the part after the \sQuote{WHERE} keyword).
#' @param conn Database connection object. Currently only \code{DBIConnection}
#'   objects from the \pkg{DBI} package are supported, but they allow for using
#'   any of the reverse dependencies of \pkg{DBI} in conjunction with \pkg{opm}
#'   and thus the majority of open \acronym{SQL} databases.
#'
#'   For using \code{RODBC} objects as created by the \pkg{RODBC} package see
#'   the \pkg{opmextra} package.
#' @param map.tables Passed as \code{do_map} argument to \code{by} from the
#'   \pkg{pkgutils} package. Necessary if table names that deviate from the
#'   defaults are to be used.
#' @param klass Character vector indicating one or several class names. For
#'   \acronym{PM} data this argument should \strong{not} be changed.
#' @param include Integer scalar indicating whether aggregated data (1) or
#'   aggregated and discretised data (2) or neither (0) should be added to the
#'   result. The numeric method of \code{opm_dbnext} needs the same kind of
#'   \code{object} argument.
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
#'   \code{map.tables}.
#'
#'   \code{opm_dbcheck} attempts to insert, query and delete the first two
#'   plates from the object \code{vaas_4} into the database. If everything is
#'   correctly set up, this should work without error \strong{unless} these two
#'   plates from \code{vaas_4} have already been inserted. If errors occur, it
#'   is up to the user to clean up the data base (as far as necessary).
#'
#'   Note that the deletion mechanism is based on \code{ON DELETE CASCADE}. To
#'   enable this in \code{SQLite}, \code{PRAGMA foreign_keys = ON;} has to be
#'   called each time a database is opened. See the according \code{demo} entry.
#'
#' @return
#'   The main functions are those for create, search, read and delete
#'   operations:\itemize{
#'   \item{\code{opm_dbput} returns an integer vector containing the primary
#'   keys of the inserted plates.}
#'   \item{\code{opm_dbfind} returns an integer vector containing the primary
#'   keys of the found plates.}
#'   \item{\code{opm_dbget} returns a \code{\link{MOPMX}} object with one
#'   element per plate type.}
#'   \item{\code{opm_dbclear} invisibly returns the result of \code{dbGetQuery}
#'   (which is usually \code{NULL}).}
#'   }
#'   Regarding the helper functions, \code{opm_dbnext} returns an integer scalar
#'   that is suitable as \code{start} argument of \code{opm_dbput}, whereas
#'   \code{opm_dbclass} returns a character scalar with the name of the
#'   intermediary class (derived from \code{\link{OPM_DB}}) to be created for
#'   database I/O. These need not normally be called by an \pkg{opm} user.
#'
#'   For checking whether a database (connection) is correctly set up,
#'   \code{opm_dbcheck} is available, which returns a character vector whose
#'   elements are either \kbd{ok} or a description of the error that has
#'   occurred at that step of the checking process.
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

setMethod("opm_dbput", c("DBTABLES", "DBIConnection"), function(object, conn,
    map.tables = NULL, start = opm_dbnext(object, conn, map.tables)) {
  object <- update(object, start, TRUE)
  by(data = object, INDICES = TRUE, FUN = dbWriteTable, conn = conn,
    append = TRUE, row.names = FALSE, do_quote = function(x)
      make.db.names(conn, x), do_map = map.tables)
  slot(object, slotNames(object)[[1L]])[, "id"]
}, sealed = SEALED)

setMethod("opm_dbput", c("ANY", "ANY"), function(object, conn, ...) {
  opm_dbput(as(object, opm_dbclass(object)), conn, ...)
}, sealed = SEALED)

#= opm_dbclass opm_dbput

#' @rdname opm_dbput
#' @export
#'
setGeneric("opm_dbclass", function(object) standardGeneric("opm_dbclass"))

setMethod("opm_dbclass", "integer", function(object) {
  int2dbclass(object)
}, sealed = SEALED)

setMethod("opm_dbclass", "OPM", function(object) {
  paste0(class(object), "_DB")
}, sealed = SEALED)

setMethod("opm_dbclass", "OPMS", function(object) {
  int2dbclass(all(has_aggr(object)) + all(has_disc(object)))
}, sealed = SEALED)

setMethod("opm_dbclass", "MOPMX", function(object) {
  int2dbclass(all(unlist(has_disc(object), FALSE, FALSE)) +
    all(unlist(has_aggr(object), FALSE, FALSE)))
}, sealed = SEALED)

#= opm_dbfind opm_dbput

#' @rdname opm_dbput
#' @export
#'
setGeneric("opm_dbfind",
  function(object, conn, ...) standardGeneric("opm_dbfind"))

setMethod("opm_dbfind", c("character", "DBIConnection"), function(object, conn,
    map.tables = NULL, klass = "OPM_DB") {
  pk <- pkeys(new(klass))[1L] # names are needed, hence not [[
  sql <- sprintf("SELECT %s FROM %s WHERE %s;", make.db.names(conn, pk),
    make.db.names(conn, map_values(names(pk), map.tables)), object)
  ids <- dbGetQuery(conn, sql)
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
    map.tables = NULL, include = 2L, klass = c(opm_dbclass(include), "MOPMX")) {
  as(by(data = new(klass[[1L]]), INDICES = object, FUN = dbGetQuery,
    conn = conn, do_map = map.tables, do_inline = TRUE, simplify = TRUE,
    do_quote = function(x) make.db.names(conn, x)), klass[[2L]])
}, sealed = SEALED)

setMethod("opm_dbget", c("character", "ANY"), function(object, conn,
    map.tables = NULL, include = 2L, klass = c(opm_dbclass(include), "MOPMX")) {
  opm_dbget(opm_dbfind(object, conn, map.tables, klass[[1L]]),
    conn, map.tables, include, klass)
}, sealed = SEALED)

#= opm_dbnext opm_dbput

#' @rdname opm_dbput
#' @export
#'
setGeneric("opm_dbnext",
  function(object, conn, ...) standardGeneric("opm_dbnext"))

setMethod("opm_dbnext", c("ANY", "ANY"), function(object, conn,
    map.tables = NULL) {
  opm_dbnext(new(opm_dbclass(object)), conn, map.tables)
}, sealed = SEALED)

setMethod("opm_dbnext", c("DBTABLES", "DBIConnection"), function(object, conn,
    map.tables = NULL) {
  get_last <- function(tn, id, conn) {
    sql <- sprintf("SELECT max(%s) FROM %s;", make.db.names(conn, id),
      make.db.names(conn, tn))
    dbGetQuery(conn, sql)
  }
  db2ids <- function(x) {
    x <- unlist(x, FALSE, FALSE)
    storage.mode(x) <- "integer"
    x[is.na(x)] <- 0L
    x + 1L
  }
  db2ids(by(data = object, INDICES = TRUE, FUN = get_last, conn = conn,
    do_map = map.tables, do_inline = FALSE, simplify = TRUE))
}, sealed = SEALED)

#= opm_dbclear opm_dbput

#' @rdname opm_dbput
#' @export
#'
setGeneric("opm_dbclear",
  function(object, conn, ...) standardGeneric("opm_dbclear"))

setMethod("opm_dbclear", c("character", "ANY"), function(object, conn,
    map.tables = NULL, klass = "OPM_DB") {
  opm_dbclear(opm_dbfind(object, conn, map.tables), conn, map.tables, klass)
}, sealed = SEALED)

setMethod("opm_dbclear", c("integer", "DBIConnection"), function(object, conn,
    map.tables = NULL, klass = "OPM_DB") {
  pk <- pkeys(new(klass))[1L]
  sql <- sprintf("DELETE FROM %s WHERE %s;", make.db.names(conn,
    map_values(names(pk), map.tables)),
    paste(make.db.names(conn, pk), object, sep = " = ", collapse = " OR "))
  invisible(dbGetQuery(conn, sql))
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
    sn <- setdiff(slotNames(a), "csv_data")
    result <- lapply(sn, function(n) all.equal(slot(a, n), slot(b, n)))
    names(result) <- sn
    c(unlist(result), csv_data = all.equal(csv_data(a, normalize = TRUE),
      csv_data(b, normalize = TRUE)))
  }

  data(list = "vaas_4", package = opm_string(), envir = environment())
  # the get() call avoids a NOTE issued by R CMD check (vaas_4 not found)
  x <- get("vaas_4", , environment())[1L:2L, time.points, wells]
  metadata(x) <- structure(.Data = list(), names = character())
  if (length(metadata))
    if (is.data.frame(metadata))
      metadata(x) <- metadata
    else
      stop("'metadata' must be empty or a data frame")

  result <- c(last1 = NA, insert = NA, receive = NA, clear = NA, object = NA,
    compare = NA, last2 = NA, samelast = NA)
  storage.mode(result) <- "character"
  step <- 0L

  tryCatch(expr = {
    last1 <- opm_dbnext(2L, conn)
    result[[step <- step + 1L]] <- "ok"
    ids <- opm_dbput(x, conn)
    result[[step <- step + 1L]] <- "ok"
    y <- opm_dbget(ids, conn)
    result[[step <- step + 1L]] <- "ok"
    opm_dbclear(ids, conn)
    result[[step <- step + 1L]] <- "ok"
    if (!is(y, "MOPMX") || length(y) != 1L)
      stop("expected MOPMX object of length 1")
    result[[step <- step + 1L]] <- "ok"
    last2 <- opm_dbnext(y, conn)
    y <- y[[1L]]
    cmp <- unlist(lapply(seq_along(x), function(i) slots_equal(y[i], x[i])))
    if (!is.logical(cmp))
      stop(paste(names(cmp), cmp, sep = ": ", collapse = " / "))
    result[[step <- step + 1L]] <- "ok"
    last3 <- opm_dbnext(2L, conn)
    result[[step <- step + 1L]] <- "ok"
    stopifnot(last1 == last2, last1 == last3)
    result[[step <- step + 1L]] <- "ok"
  }, error = function(e) result[[step + 1L]] <<- conditionMessage(e))

  result
}, sealed = SEALED)


################################################################################
