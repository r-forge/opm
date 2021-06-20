\name{summary}
\alias{print}
\alias{print.lpsn_result}
\alias{summary}
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
  storing the results of a query to the \acronym{LPSN}
  \acronym{API}.
}
\details{
  These methods yield or display basic information about
  the result of the download attempt.
}
\examples{
## Examples are deliberately not given here.
}
\references{
  \url{https://api.lpsn.dsmz.de/}
}
\seealso{
  \code{\link{advanced_search}} \code{\link{fetch}}
  \code{\link{flexible_search}}
}
\keyword{database}
\keyword{print}

