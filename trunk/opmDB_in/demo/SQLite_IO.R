

library(opm)
library(opmDB)
library(RSQLite)


conn <- dbConnect("SQLite",
  dbname = Sys.getenv("OPM_SQLITE_DB", file.path("misc", "pmdata.db")))

result <- opm_dbcheck(conn)

dbGetQuery(conn, c("ALTER TABLE plates ADD COLUMN strain text;",
  "ALTER TABLE plates ADD COLUMN replicate integer;"))

## one cannot drop columns from an SQLite table
#dbGetQuery(conn, c("ALTER TABLE plates DROP COLUMN strain;",
#  "ALTER TABLE plates DROP COLUMN replicate;"))

dbDisconnect(conn)

stopifnot(result == "ok")

