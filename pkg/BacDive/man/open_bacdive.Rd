\name{open_bacdive}
\alias{open_bacdive}
\title{Creating \sQuote{bacdive_access} objects}
\usage{
  open_bacdive(username, password)
}
\arguments{
  \item{username}{Character vector of length 1.}

  \item{password}{Character vector of length 1.}
}
\value{
  If login is successful, \code{open_bacdive} returns a new
  \sQuote{dsmz_keycloak} object, if otherwise an error
  results. Login is, of course, only successful if a user
  of the given name is already registered with the given
  password.
}
\description{
  This package uses \sQuote{bacdive_access} objects for
  managing the access to the \acronym{BacDive}
  \acronym{API}. These objects are variants of
  \sQuote{dsmz_keycloak} objects.
}
\details{
  The actual usage of \sQuote{bacdive_access} objects is
  demonstrated by querying the \acronym{BacDive}
  \acronym{API}. See the examples given for the according
  functions, such as \code{\link{fetch}}.

  The usage of the \acronym{BacDive} \acronym{API} requires
  registration, although registration is free and easy to
  accomplish. See the \acronym{BacDive} \acronym{API} web
  page.

  The usage of the \acronym{BacDive} data is only permitted
  when in compliance with the \acronym{BacDive} terms and
  conditions, see the \acronym{BacDive} web page.
}
\examples{
## Examples are deliberately not given here.
}
\references{
  \url{https://bacdive.dsmz.de/about}

  \url{https://bacdive.dsmz.de/mailinglist}

  \url{https://api.bacdive.dsmz.de/}

  \url{https://www.keycloak.org/}
}
\seealso{
  \code{\link{refresh}}

  Other query.functions: \code{\link{fetch}},
    \code{\link{request}},
    \code{\link{retrieve.bacdive_access}},
  \code{\link{upgrade.bacdive_access}}
}
\keyword{connection}
\keyword{database}

