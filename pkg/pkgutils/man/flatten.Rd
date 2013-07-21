\name{flatten}
\alias{flatten}
\alias{flatten.default}
\alias{flatten.list}
\title{Flatten an object}
\usage{
  flatten(object, ...)

  \method{flatten}{default} (object, ...)

  \method{flatten}{list} (object, ...)
}
\arguments{
  \item{object}{Usually a list. The default method just
  returns \code{object} if it is atomic but raises an error
  otherwise.}

  \item{...}{Optional arguments passed to and from other
  methods, or between the methods.}
}
\value{
  The list method returns a non-nested list.
}
\description{
  Methods for making an object \sQuote{flat}, such as
  creating a non-nested list from a list.
}
\details{
  The list method is based on
  \url{http://stackoverflow.com/questions/8139677/} with
  some slight improvements.
}
\examples{
x <- list(a = list(b = 1:5, c = letters[1:5]), d = LETTERS[1:3],
  e = list(pi))
(y <- flatten(x)) # sublists removed, non-list elements kept
stopifnot(is.list(y), length(y) == 4, !sapply(y, is.list))
# atomic objects are not modified by default
stopifnot(identical(letters, flatten(letters)))
}
\seealso{
  base::unlist

  Other coding-functions: \code{\link{L}},
  \code{\link{LL}}, \code{\link{case}},
    \code{\link{listing}},         \code{\link{must}}
}
\keyword{manip}

