\name{refresh}
\alias{print.dsmz_keycloak}
\alias{refresh}
\alias{refresh.dsmz_keycloak}
\alias{summary.dsmz_keycloak}
\title{Methods for \sQuote{dsmz_keycloak} objects}
\usage{
  refresh(object, ...)

  \method{refresh}{dsmz_keycloak} (object, ...)

  \method{summary}{dsmz_keycloak} (object, ...)

  \method{print}{dsmz_keycloak} (x, ...)
}
\arguments{
  \item{object}{Object of class \sQuote{dsmz_keycloak}.}

  \item{x}{Object of class \sQuote{dsmz_keycloak}.}

  \item{...}{Optional arguments passed to other methods.}
}
\value{
  \code{refresh} returns a new \sQuote{dsmz_keycloak}
  object if \code{object} still permits refreshing. It is
  supposed to do so if \sQuote{refresh_expired} is
  \code{FALSE}, if otherwise an error results.  Refreshing
  is supposed to be necessary if \sQuote{expired} is
  \code{TRUE}.  Both values are given by \code{summary},
  which returns a logical vector.  The \code{print} method
  returns \code{x}, invisibly.
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
}
\examples{
## Examples are deliberately not given here.
}
\references{
  \url{https://www.keycloak.org/}
}
\keyword{connection}
\keyword{database}
\keyword{print}

