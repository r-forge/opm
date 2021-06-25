\name{records}
\alias{as.data.frame.dsmz_result}
\alias{as.data.frame.records}
\alias{print.dsmz_result}
\alias{print.records}
\alias{records}
\alias{records.dsmz_result}
\alias{records.list}
\alias{summary.dsmz_result}
\alias{summary.records}
\title{Methods for nested lists and \sQuote{dsmz_result} objects}
\usage{
  records(object, ...)

  \method{records}{list} (object, ...)

  \method{records}{dsmz_result} (object, ...)

  \method{as.data.frame}{records} (x, row.names = NULL,
    optional = TRUE, ...)

  \method{as.data.frame}{dsmz_result} (x, row.names = NULL,
    optional = TRUE, ...)

  \method{summary}{records} (object, ...)

  \method{summary}{dsmz_result} (object, ...)

  \method{print}{records} (x, ...)

  \method{print}{dsmz_result} (x, ...)
}
\arguments{
  \item{object}{List or object of class \sQuote{records}
  (which is a special kind of list), or a list or other
  object to be converted to a \sQuote{records} object.}

  \item{x}{Object of class \sQuote{records} or other object
  that can be converted to such an object.}

  \item{row.names}{\code{NULL} or a character vector with
  row names for the data frame.}

  \item{optional}{Logical vector of length 1 indicating
  whether creation of syntactic and unique (column) names
  for the data frame is optional. Note that non-unique
  names within \code{x}, if any, could be corrected but
  could still yield unexpected assignments of values to
  columns. The presence of empty names yields an error,
  hence setting \code{optional} to \code{FALSE} is
  necessary in that case.}

  \item{...}{Optional arguments passed to other methods.}
}
\value{
  The \code{as.data.frame} method creates a data frame from
  a list of class \sQuote{records}. The \code{records}
  method creates an object of that class if the given
  object passes the tests (see the examples). The other
  methods yield or display basic information about a
  \sQuote{records} object.

  The \code{summary} function returns a list or character
  vector. The \code{print} method returns \code{x},
  invisibly.
}
\description{
  Calls to a \acronym{JSON}-based \acronym{API} may yield
  nested lists whose conversion to a data frame may not be
  straightforward. The \sQuote{records} class of objects
  can assist in such conversions.
}
\details{
  This package uses \sQuote{dsmz_result} objects for
  storing the direct results of querying the \acronym{API}
  and \sQuote{records} objects for storing compiled results
  (created from \sQuote{dsmz_result} objects).

  Note that each \sQuote{dsmz_result} object also responds
  to \code{`$`} and \code{`[[`}. The most important key is
  probably \sQuote{results} as it yields the \code{API}
  entries (one per taxon name). \code{summary} and
  \code{print} show an overview of all keys.

  When the server signals that the \code{API} query was
  erroneous (as opposed to just yielding zero results), the
  structure of the returned \sQuote{dsmz_result} object is
  different. While a \sQuote{results} entry should be
  missing, entries such as \sQuote{code} (giving the
  \acronym{HTTP} status code), \sQuote{message} and
  \sQuote{title} should be present in such a case and
  should indicate the kind of problem.

  The compiled \acronym{API} results as returned by
  \code{retrieve} are of class \sQuote{records}. A
  dedicated \code{as.data.frame} method can convert such
  objects to a data frame.
}
\examples{
print(records(list()))

x <- records(list(list(A = 1, B = 2)))
print(x)

# the list elements must be lists
x <- try(records(list(A = 1, B = 2)))
stopifnot(inherits(x, "try-error"))

# the list elements must be named lists
x <- try(records(list(list(1, 2))))
stopifnot(inherits(x, "try-error"))

# resulting data frame columns can be lists
x <- records(list(list(A = -1, B = 2), list(B = 3:4)))
print(x)
print(as.data.frame(x))

# missing keys yield missing data (NA values)
x <- records(list(list(A = 1, B = 2), list(C = 3, D = 4)))
print(x)
print(as.data.frame(x))
}
\references{
  \url{https://www.restapitutorial.com/httpstatuscodes.html}
}
\seealso{
  Other common.functions:
  \code{\link{print.dsmz_keycloak}}, \code{\link{refresh}},
    \code{\link{retrieve}},
    \code{\link{summary.dsmz_keycloak}}
}
\keyword{database}
\keyword{list}
\keyword{manip}
\keyword{print}

