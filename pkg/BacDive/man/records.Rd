\name{records}
\alias{as.data.frame.records}
\alias{print.records}
\alias{records}
\alias{records.list}
\alias{summary.records}
\title{Methods for nested lists}
\usage{
  records(object, ...)

  \method{records}{list} (object, ...)

  \method{as.data.frame}{records} (x, row.names = NULL,
    optional = TRUE, ...)

  \method{summary}{records} (object, ...)

  \method{print}{records} (x, ...)
}
\arguments{
  \item{object}{List or object of class \sQuote{records}
  (which is a special kind of list).}

  \item{x}{Object of class \sQuote{records}.}

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
  method creates on object of that class if the given
  object passes the tests (see the examples). The other
  methods yield or display basic information about a
  \sQuote{records} object.
}
\description{
  Calls to a \acronym{JSON}-based \acronym{API} may yield
  nested lists whose conversion to a data frame may not be
  straightforward. The \sQuote{records} class of objects
  can assist in such conversions.
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
\seealso{
  Other common-functions:
  \code{\link{print.dsmz_keycloak}}, \code{\link{refresh}},
    \code{\link{summary.dsmz_keycloak}}
}
\keyword{list}
\keyword{manip}
\keyword{print}

