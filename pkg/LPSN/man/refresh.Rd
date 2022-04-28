\name{refresh}
\alias{print.dsmz_keycloak}
\alias{refresh}
\alias{refresh.dsmz_keycloak}
\alias{retrieve}
\alias{retrieve.dsmz_keycloak}
\alias{summary.dsmz_keycloak}
\title{Methods for \sQuote{dsmz_keycloak} objects}
\usage{
  refresh(object, ...)

  \method{refresh}{dsmz_keycloak} (object, self = TRUE,
    ...)

  \method{summary}{dsmz_keycloak} (object, ...)

  \method{print}{dsmz_keycloak} (x, ...)

  retrieve(object, ...)

  \method{retrieve}{dsmz_keycloak} (object, ...,
    handler = NULL, sleep = 0.5)
}
\arguments{
  \item{object}{Object of class \sQuote{dsmz_keycloak}.}

  \item{self}{Logical vector of length 1 indicating whether
  \code{object} should itself be modified or a new object
  returned.}

  \item{x}{Object of class \sQuote{dsmz_keycloak}.}

  \item{handler}{If empty, ignored. Otherwise a function to
  which each data chunk retrieved from the \acronym{API} is
  transferred in turn. The function should accept a single
  argument. \code{retrieve} and the handler function may
  thus best be called within a dedicated enclosing
  function.}

  \item{sleep}{A waiting period in seconds between
  successive \acronym{API} requests, if any.}

  \item{...}{Optional arguments passed to other methods.}
}
\value{
  \code{refresh} returns a new or modified
  \sQuote{dsmz_keycloak} object if \code{object} still
  permits refreshing. It is supposed to do so if
  \sQuote{refresh_expired} is \code{FALSE}, if otherwise an
  error results.

  Refreshing is supposed to be necessary if
  \sQuote{expired} is \code{TRUE} (although the local
  estimate may deviate from the actions of the server).
  Both values are given by \code{summary}, which returns a
  logical vector.

  The \code{print} method returns \code{x}, invisibly.

  \code{retrieve} combines the functionality of
  \code{request}, \code{fetch} and \code{upgrade} to
  download all entries found in the \acronym{API},
  traversing all chunks of a paginated result in turn. The
  resulting list (of class \sQuote{records}) may be huge,
  hence care should be taken. It may be advisable to use
  \code{handler}. If this function is given, each chunk is
  passed to \code{handler} in turn. The \code{handler}
  function could then store the data in a database or in a
  file. If \code{handler} is given, the number of its calls
  is returned.
}
\description{
  This package uses \sQuote{dsmz_keycloak} objects for
  managing the access to the \acronym{API} services
  provided by \acronym{DSMZ}.
}
\details{
  The actual usage of \sQuote{dsmz_keycloak} objects is
  demonstrated by querying a \acronym{DSMZ} \acronym{API}.
  See the examples for the according functions.

  When generating a \sQuote{dsmz_keycloak} object, the
  package responds to a system environment variable called
  \sQuote{DSMZ_API_VERBOSE}. Its primary interpretation is
  as an integer number. Non-empty values of
  \sQuote{DSMZ_API_VERBOSE} that cannot be interpreted as
  an integer number are treated like 1; the empty character
  string is treated like 0.

  When downloading data from the \acronym{API}, a verbosity
  of 1 means that the \acronym{URL} of each \acronym{API}
  request is output; when 2 or larger, more intermediary
  results may be shown.
}
\examples{
## Examples are deliberately not given here.
}
\references{
  \url{https://www.keycloak.org/}

  \url{https://www.dsmz.de/privacy-statement}
}
\seealso{
  Other common-functions:
  \code{\link{as.data.frame.dsmz_result}},
  \code{\link{as.data.frame.records}},
  \code{\link{print.dsmz_result}},
  \code{\link{print.records}}, \code{\link{records}},
      \code{\link{summary.dsmz_result}},
  \code{\link{summary.records}}
}
\keyword{connection}
\keyword{database}
\keyword{print}

