\name{flatten}
\alias{flatten}
\alias{flatten.default}
\alias{flatten.list}
\title{Flatten a list}
\usage{
  flatten(object, ...)

  \method{flatten}{default} (object, ...)

  \method{flatten}{list} (object, ...)
}
\arguments{
  \item{object}{Usually a list. The default method just
  returns \code{object} if it is atomic but fails
  otherwise.}

  \item{...}{Optional arguments passed to and from other
  methods, or between the methods.}
}
\value{
  The list method returns a list, the default method
  \code{object}.
}
\description{
  Convert to a \sQuote{flat} object, such as a non-nested
  list created from a list.
}
\note{
  The list method is based on
  \url{http://stackoverflow.com/questions/8139677/} with
  some slight improvements.
}
\examples{
x <- list(a = list(b = 1:5, c = letters[1:5]), d = LETTERS[1:3],
  e = list(pi))
(y <- flatten(x))
stopifnot(is.list(y), length(y) == 4, !sapply(y, is.list))
stopifnot(identical(letters, flatten(letters)))
}
\seealso{
  Other coding-functions: \code{\link{L}},
  \code{\link{LL}}, \code{\link{case}},
    \code{\link{listing}},         \code{\link{must}}
}
\keyword{manip}

