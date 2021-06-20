\name{open_lpsn}
\alias{open_lpsn}
\title{Creating \sQuote{lpsn_access} objects}
\usage{
  open_lpsn(username, password)
}
\arguments{
  \item{username}{Character vector of length 1.}

  \item{password}{Character vector of length 1.}
}
\value{
  If login is successful, \code{refresh} returns a new
  \sQuote{dsmz_keycloak} object, if otherwise an error
  results. Login is, of course, only successful if a user
  of the given name is already registered with the given
  password.
}
\description{
  This package uses \sQuote{lpsn_access} objects for
  managing the access to the \acronym{LPSN} \acronym{API}.
  These objects are variants of \sQuote{dsmz_keycloak}
  objects.
}
\details{
  The actual usage of \sQuote{lpsn_access} objects is
  demonstrated by querying the \acronym{LPSN}
  \acronym{API}. See the examples given for the according
  functions.

  The usage of the LPSN API requires registration, although
  registration is free and easy to accomplish. See the LPSN
  API web page.

  The usage of the LPSN data is only permitted when in
  compliance with the LPSN copyright, although this
  copyright is liberal, see the LPSN web page.
}
\examples{
## Examples are deliberately not given here.
}
\references{
  \url{https://lpsn.dsmz.de/text/copyright}

  \url{https://api.lpsn.dsmz.de/}

  \url{https://www.keycloak.org/}
}
\seealso{
  \code{\link{refresh}}

  Other query-functions: \code{\link{advanced_search}},
    \code{\link{fetch}},   \code{\link{flexible_search}},
  }
\keyword{connection}
\keyword{database}

