

library(pkgutils)
library(opmDB)
library(opm)
library(RODBC)


conn <- odbcConnect("test_opm;TextAsLongVarchar=1;MaxLongVarcharSize=8190")

# Insertions via RODBC in this manner are slow. Subsetting speeds things up.
result <- opm_dbcheck(conn, time.points = 1:5, wells = 12:14)

print(opm_dbnext(2L, conn))

if (all(result == "ok")) {

  # addition of metadata columns
  sqlQuery(conn,
    "ALTER TABLE plates ADD COLUMN strain text, ADD COLUMN replicate integer;")

  # check with metadata
  md <- data.frame(strain = c("X", "Y"), replicate = c(3L, 7L),
    stringsAsFactors = FALSE)
  result2 <- opm_dbcheck(conn, md, time.points = 1:5, wells = 12:14)

  # removal of metadata columns
  sqlQuery(conn,
    "ALTER TABLE plates DROP COLUMN strain, DROP COLUMN replicate;")

}

odbcClose(conn)

print(result)
stopifnot(result == "ok")
print(result2)
stopifnot(result2 == "ok")

