

library(opm)
library(opmDB)
library(RMySQL)


conn <- dbConnect("MySQL", dbname = Sys.getenv("OPM_MYSQL_DB", "pmdata"))

result <- opm_dbcheck(conn)

dbGetQuery(conn, c("ALTER TABLE plates ADD COLUMN strain text;",
  "ALTER TABLE plates ADD COLUMN replicate integer;"))

dbGetQuery(conn, c("ALTER TABLE plates DROP COLUMN strain;",
  "ALTER TABLE plates DROP COLUMN replicate;"))

dbDisconnect(conn)

stopifnot(result == "ok")

