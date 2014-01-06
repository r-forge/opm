

library(opm)
library(opmDB)
library(RPostgreSQL)


conn <- dbConnect("PostgreSQL",
  dbname = Sys.getenv("OPM_POSTGRESQL_DB", "pmdata"))

result <- opm_dbcheck(conn)

dbDisconnect(conn)

stopifnot(result == "ok")

