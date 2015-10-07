

setOldClass("RODBC")


#' Database I/O for \pkg{opm}
#'
#' Methods for inserting, querying and deleting \code{\link{OPMX}} objects into
#' or from (\acronym{SQL}-based) relational databases using \pkg{RODBC}.
#'
#' @param object \code{\link{OPMX}}, \code{\link{MOPMX}} or \code{\link{OPM_DB}}
#'   object, integer vector containing real or potential primary keys of a
#'   database table, or character scalar containing a partial \acronym{SQL}
#'   query (the part after the \sQuote{WHERE} keyword).
#' @param conn Database connection object. \code{RODBC} objects as created by
#'   the \pkg{RODBC} package are supported.
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
#' @param ... Optional arguments passed between the methods.
#'
#' @details The \code{RODBC} methods use a simple quoting scheme for table and
#'   column names.
#'
#'   Note that the deletion mechanism is based on \code{ON DELETE CASCADE}. To
#'   enable this in \code{SQLite}, \code{PRAGMA foreign_keys = ON;} has to be
#'   called each time a database is opened. See the according \code{demo} entry.
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
#'   \code{opm_dbcheck} is available in the \pkg{opm} package, which returns a
#'   character vector whose elements are either \kbd{ok} or a description of the
#'   error that has occurred at that step of the checking process.
#' @family dbio-functions
#' @seealso DBI::make.db.names pkgutils::by opm::opm_dbcheck
#' @keywords database
#' @export
#' @examples
#' # The SQL files for generating the expected database tables. Tables can
#' # be renamed, but then an according 'map.tables' argument must be used.
#' library(opm)
#' opm_files("sql")
#'
#' # Usage examples are given in these demos. An according database must be
#' # made accessible beforehand.
#' if (interactive())
#'   demo(package = "opmextra")
#'
setMethod("opm_dbput", c("DBTABLES", "RODBC"), function(object, conn,
    map.tables = NULL, start = opm_dbnext(object, conn, map.tables)) {
  object <- update(object, start, TRUE)
  by(data = object, INDICES = TRUE, FUN = function(n, x, ...)
    sqlSave(dat = x, tablename = n, ...), channel = conn, append = TRUE,
    test = FALSE, rownames = FALSE, fast = TRUE, verbose = FALSE,
    do_quote = if (attr(conn, "isMySQL"))
      "`"
    else
      "\"", do_map = map.tables, simplify = FALSE)
  slot(object, slotNames(object)[[1L]])[, "id"]
}, sealed = SEALED)

#= opm_dbfind opm_dbput

#' @rdname opm_dbput
#' @export
#'
setMethod("opm_dbfind", c("character", "RODBC"), function(object, conn,
    map.tables = NULL, klass = "OPM_DB") {
  pk <- pkeys(new(klass))[1L] # names are needed, hence not [[
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
setMethod("opm_dbget", c("integer", "RODBC"), function(object, conn,
    map.tables = NULL, include = 2L, klass = c(opm_dbclass(include), "MOPMX")) {
  as(by(data = new(klass[[1L]]), INDICES = object, FUN = sqlQuery,
    channel = conn, do_map = map.tables, do_inline = TRUE,
    do_quote = if (attr(conn, "isMySQL"))
      "`"
    else
      "\"", stringsAsFactors = FALSE, simplify = TRUE), klass[[2L]])
}, sealed = SEALED)

#= opm_dbnext opm_dbput

#' @rdname opm_dbput
#' @export
#'
setMethod("opm_dbnext", c("DBTABLES", "RODBC"), function(object, conn,
    map.tables = NULL) {
  get_last <- function(tn, id, conn, char) {
    sql <- sprintf("SELECT max(%s) FROM %s;",
      quote_protected(id, char), quote_protected(tn, char))
    sqlQuery(conn, sql)
  }
  db2ids <- function(x) {
    x <- unlist(x, FALSE, FALSE)
    storage.mode(x) <- "integer"
    x[is.na(x)] <- 0L
    x + 1L
  }
  db2ids(by(data = object, INDICES = TRUE, FUN = get_last, conn = conn,
    char = if (attr(conn, "isMySQL"))
      "`"
    else
      "\"", do_map = map.tables, do_inline = FALSE, simplify = TRUE))
}, sealed = SEALED)

#= opm_dbclear opm_dbput

#' @rdname opm_dbput
#' @export
#'
setMethod("opm_dbclear", c("integer", "RODBC"), function(object, conn,
    map.tables = NULL, klass = "OPM_DB") {
  pk <- pkeys(new(klass))[1L]
  char <- if (attr(conn, "isMySQL"))
      "`"
    else
      "\""
  sql <- sprintf("DELETE FROM %s WHERE %s;",
    quote_protected(map_values(names(pk), map.tables), char),
    paste(quote_protected(pk, char), object, sep = " = ", collapse = " OR "))
  invisible(sqlQuery(conn, sql))
}, sealed = SEALED)

