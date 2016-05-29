\name{opm_dbput}
\alias{opm_dbclear}
\alias{opm_dbfind}
\alias{opm_dbget}
\alias{opm_dbnext}
\alias{opm_dbput}
\title{Database I/O for \pkg{opm}}
\arguments{
  \item{object}{\code{\link{OPMX}}, \code{\link{MOPMX}} or
  \code{\link{OPM_DB}} object, integer vector containing
  real or potential primary keys of a database table, or
  character scalar containing a partial \acronym{SQL} query
  (the part after the \sQuote{WHERE} keyword).}

  \item{conn}{Database connection object. \code{RODBC}
  objects as created by the \pkg{RODBC} package are
  supported.}

  \item{map.tables}{Passed as \code{do_map} argument to
  \code{by} from the \pkg{pkgutils} package. Necessary if
  table names that deviate from the defaults are to be
  used.}

  \item{klass}{Character vector indicating one or several
  class names. For \acronym{PM} data this argument should
  \strong{not} be changed.}

  \item{include}{Integer scalar indicating whether
  aggregated data (1) or aggregated and discretised data
  (2) or neither (0) should be added to the result. The
  numeric method of \code{opm_dbnext} needs the same kind
  of \code{object} argument.}

  \item{start}{Integer vector determining the minimum
  primary keys to which those in \code{object} should be
  coerced. Necessary for appending to a database table
  without overwriting previously inserted data.}

  \item{...}{Optional arguments passed between the
  methods.}
}
\value{
  The main functions are those for create, search, read and
  delete operations:\itemize{ \item{\code{opm_dbput}
  returns an integer vector containing the primary keys of
  the inserted plates.} \item{\code{opm_dbfind} returns an
  integer vector containing the primary keys of the found
  plates.} \item{\code{opm_dbget} returns a
  \code{\link{MOPMX}} object with one element per plate
  type.} \item{\code{opm_dbclear} invisibly returns the
  result of \code{dbGetQuery} (which is usually
  \code{NULL}).} } Regarding the helper functions,
  \code{opm_dbnext} returns an integer scalar that is
  suitable as \code{start} argument of \code{opm_dbput},
  whereas \code{opm_dbclass} returns a character scalar
  with the name of the intermediary class (derived from
  \code{\link{OPM_DB}}) to be created for database I/O.
  These need not normally be called by an \pkg{opm} user.

  For checking whether a database (connection) is correctly
  set up, \code{opm_dbcheck} is available in the \pkg{opm}
  package, which returns a character vector whose elements
  are either \kbd{ok} or a description of the error that
  has occurred at that step of the checking process.
}
\description{
  Methods for inserting, querying and deleting
  \code{\link{OPMX}} objects into or from
  (\acronym{SQL}-based) relational databases using
  \pkg{RODBC}.
}
\details{
  The \code{RODBC} methods use a simple quoting scheme for
  table and column names.

  Note that the deletion mechanism is based on \code{ON
  DELETE CASCADE}. To enable this in \code{SQLite},
  \code{PRAGMA foreign_keys = ON;} has to be called each
  time a database is opened. See the according \code{demo}
  entry.
}
\examples{
# The SQL files for generating the expected database tables. Tables can
# be renamed, but then an according 'map.tables' argument must be used.
library(opm)
opm_files("sql")

# Usage examples are given in these demos. An according database must be
# made accessible beforehand.
if (interactive())
  demo(package = "opmextra")
}
\seealso{
  DBI::make.db.names pkgutils::by opm::opm_dbcheck
}
\keyword{database}

%% BEGIN INSERTION BY repair_S4_docu.rb
\docType{methods}
\alias{opm_dbclear-methods}
\alias{opm_dbclear,integer,RODBC-method}
\alias{opm_dbfind-methods}
\alias{opm_dbfind,character,RODBC-method}
\alias{opm_dbget-methods}
\alias{opm_dbget,integer,RODBC-method}
\alias{opm_dbnext-methods}
\alias{opm_dbnext,DBTABLES,RODBC-method}
\alias{opm_dbput-methods}
\alias{opm_dbput,DBTABLES,RODBC-method}
\usage{
  \S4method{opm_dbclear}{integer,RODBC}(object, conn, map.tables = NULL, klass = "OPM_DB")

  \S4method{opm_dbfind}{character,RODBC}(object, conn, map.tables = NULL, klass = "OPM_DB")

  \S4method{opm_dbget}{integer,RODBC}(object, conn,
    map.tables = NULL, include = 2L, klass = c(opm_dbclass(include), "MOPMX")) 

  \S4method{opm_dbnext}{DBTABLES,RODBC}(object, conn, map.tables = NULL)

  \S4method{opm_dbput}{DBTABLES,RODBC}(object, conn,
    map.tables = NULL, start = opm_dbnext(object, conn, map.tables)) 
}
%% END INSERTION BY repair_S4_docu.rb
