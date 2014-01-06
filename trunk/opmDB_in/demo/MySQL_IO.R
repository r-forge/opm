

library(opm)
library(opmDB)
library(RMySQL)


conn <- dbConnect("MySQL", dbname = Sys.getenv("OPM_MYSQL_DB", "pmdata"))

# check without metadata
result <- opm_dbcheck(conn)

print(opm_dbnext(2L, conn))

if (all(result == "ok")) {

  # addition of metadata columns
  dbGetQuery(conn,
    "ALTER TABLE plates ADD COLUMN strain text, ADD COLUMN replicate integer;")

  # check with metadata
  md <- data.frame(strain = c("X", "Y"), replicate = c(3L, 7L),
    stringsAsFactors = FALSE)
  result2 <- opm_dbcheck(conn, md)

  # removal of metadata columns
  dbGetQuery(conn,
    "ALTER TABLE plates DROP COLUMN strain, DROP COLUMN replicate;")

}

dbDisconnect(conn)

print(result)
stopifnot(result == "ok")
print(result2)
stopifnot(result2 == "ok")

