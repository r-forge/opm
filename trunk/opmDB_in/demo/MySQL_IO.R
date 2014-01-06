

library(opm)
library(opmDB)
library(RMySQL)


conn <- dbConnect("MySQL", dbname = Sys.getenv("OPM_MYSQL_DB", "pmdata"))

result <- opm_dbcheck(conn)

dbDisconnect(conn)

stopifnot(result == "ok")

