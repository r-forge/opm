

library(opm)
library(opmDB)
library(RSQLite)


conn <- dbConnect("SQLite",
  dbname = Sys.getenv("OPM_SQLITE_DB", file.path("misc", "pmdata.db")))

result <- opm_dbcheck(conn)

dbDisconnect(conn)

stopifnot(result == "ok")

