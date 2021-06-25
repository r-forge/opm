\name{summary.bacdive_result}
\alias{print.bacdive_result}
\alias{summary.bacdive_result}
\title{Methods for \sQuote{bacdive_result} objects}
\usage{
  \method{summary}{bacdive_result} (object, ...)

  \method{print}{bacdive_result} (x, ...)
}
\arguments{
  \item{object}{Object of class \sQuote{bacdive_result}.}

  \item{x}{Object of class \sQuote{bacdive_result}.}

  \item{...}{Optional arguments passed to other methods.}
}
\value{
  The \code{summary} function returns a list or character
  vector. The \code{print} method returns \code{x},
  invisibly.
}
\description{
  This package uses \sQuote{bacdive_result} objects for
  storing the direct results of querying the
  \acronym{BacDive} \acronym{API} and \sQuote{records}
  objects for storing compiled results (created from
  \sQuote{bacdive_result} objects).
}
\details{
  These methods yield or display basic information about
  the result of a download attempt. For real examples of
  their usage see \code{\link{fetch}} and friends.

  Note that each \sQuote{bacdive_result} object also
  responds to \code{`$`} and \code{`[[`}. The most
  important key is probably \sQuote{results} as it yields
  the \code{API} entries (one per taxon name).
  \code{summary} and \code{print} show an overview of all
  keys.

  When the server signals that the \code{API} query was
  erroneous (as opposed to just yielding zero results), the
  structure of the returned \sQuote{bacdive_result} object
  is different. While a \sQuote{results} entry should be
  missing, entries such as \sQuote{code} (giving the
  \acronym{HTTP} status code), \sQuote{message} and
  \sQuote{title} should be present in such a case and
  should indicate the kind of problem.

  The compiled \acronym{BacDive} \acronym{API} results as
  returned by \code{retrieve} are of class
  \sQuote{records}. A dedicated \code{as.data.frame} method
  can convert such objects to a data frame.

  A detailed description of the content of the
  \acronym{API} entries is given on the \acronym{BacDive}
  web site.
}
\examples{
## Examples are deliberately not given here.
}
\references{
  \url{https://api.bacdive.dsmz.de/}

  \url{https://www.restapitutorial.com/httpstatuscodes.html}
}
\seealso{
  \code{\link{fetch}} \code{\link{request}}
  \code{\link{retrieve}} \code{\link{upgrade}}
  \code{\link{records}}
}
\keyword{database}
\keyword{print}

