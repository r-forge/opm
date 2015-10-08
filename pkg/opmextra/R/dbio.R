setOldClass("RODBC")

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

setMethod("opm_dbget", c("integer", "RODBC"), function(object, conn,
    map.tables = NULL, include = 2L, klass = c(opm_dbclass(include), "MOPMX")) {
  as(by(data = new(klass[[1L]]), INDICES = object, FUN = sqlQuery,
    channel = conn, do_map = map.tables, do_inline = TRUE,
    do_quote = if (attr(conn, "isMySQL"))
      "`"
    else
      "\"", stringsAsFactors = FALSE, simplify = TRUE), klass[[2L]])
}, sealed = SEALED)

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

