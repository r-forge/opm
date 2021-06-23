\name{summary.lpsn_result}
\alias{print.lpsn_result}
\alias{summary.lpsn_result}
\title{Methods for \sQuote{lpsn_result} objects}
\usage{
  \method{summary}{lpsn_result} (object, ...)

  \method{print}{lpsn_result} (x, ...)
}
\arguments{
  \item{object}{Object of class \sQuote{lpsn_result}.}

  \item{x}{Object of class \sQuote{lpsn_result}.}

  \item{...}{Optional arguments passed to other methods.}
}
\value{
  The \code{summary} function returns a list or character
  vector.  The \code{print} method returns \code{x},
  invisibly.
}
\description{
  This package uses \sQuote{lpsn_result} objects for
  storing the direct results of querying the \acronym{LPSN}
  \acronym{API} and \sQuote{records} objects for storing
  compiled results (created from \sQuote{lpsn_result}
  objects).
}
\details{
  These methods yield or display basic information about
  the result of a download attempt. For real examples of
  their usage see \code{\link{fetch}} and friends.

  Note that each \sQuote{lpsn_result} object also responds
  to \code{`$`} and \code{`[[`}. The most important key is
  probably \sQuote{results} as it yields the \code{API}
  entries (one per taxon name). \code{summary} and
  \code{print} show an overview of all keys.

  When the server signals that the \code{API} query was
  erroneous (as opposed to just yielding zero results), the
  structure of the returned \sQuote{lpsn_result} object is
  different. While a \sQuote{results} entry should be
  missing, entries such as \sQuote{code} (giving the
  \acronym{HTTP} status code), \sQuote{message} and
  \sQuote{title} should be present in such a case and
  should indicate the kind of problem.
}
\examples{
## Examples are deliberately not given here.
}
\references{
  \url{https://api.lpsn.dsmz.de/}

  \url{https://www.restapitutorial.com/httpstatuscodes.html}
}
\seealso{
  \code{\link{fetch}} \code{\link{request}}
  \code{\link{retrieve}} \code{\link{upgrade}}
  \code{\link{records}}
}
\keyword{database}
\keyword{print}

