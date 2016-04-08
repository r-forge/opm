\name{assert}
\alias{assert}
\title{Make assertions}
\usage{
  assert(cond, orig, msg, quiet = FALSE, ...)
}
\arguments{
  \item{cond}{A logical vector, character scalar or
  function. If a character scalar, converted to a function
  with \code{match.fun}. If a function, \code{orig} is
  passed to it as its first argument. The function should
  return a logical vector of the length of \code{orig}.}

  \item{orig}{Mandatory if \code{cond} is (the name of) a
  function, otherwise ignored when empty or missing.
  Otherwise a vector of the length of \code{cond} if
  \code{cond} is a logical vector, or the single argument
  of \code{cond} if it is (the name of) a function.}

  \item{msg}{When empty or missing, an error message is
  constructed automatically in the case of failure.
  Otherwise either are \code{sprintf} template to be
  applied to the failing elements of \code{orig}, or a
  character scalar directly providing the error message.}

  \item{quiet}{Logical scalar indicating whether, in the
  case of failure, an exception should be raised or a
  character vector with descriptions of the problems should
  be returned. If \code{quiet} is \code{NA}, a warning is
  issued instead of an error.}

  \item{...}{Optional arguments passed to \code{cond} when
  it is (the name of) a function.}
}
\value{
  The return value is \code{TRUE} (for \code{quiet =
  FALSE}) or an empty character vector (for \code{quiet =
  TRUE}) when all elements of \code{cond} are \code{TRUE}.
  Otherwise either an error is raised or a character vector
  with the description of the problems is returned.
}
\description{
  A function similar to \code{stopifnot} reporting more
  details on vector elements.
}
\details{
  Compared to \code{stopifnot} this function can only
  conduct a test on a single object but can report
  element-specific details of failures.
}
\examples{
stopifnot(assert(function(x) x > 0, 1:10))
(x <- try(assert(function(x) x > 0, -1:8), TRUE))
stopifnot(inherits(x, "try-error"))
}
\seealso{
  base::stopifnot base::match.fun base::sprintf

  Other coding-functions: \code{\link{L}},
  \code{\link{LL}}, \code{\link{case}},
    \code{\link{check}}, \code{\link{collect}},
    \code{\link{contains}}, \code{\link{flatten}},
  \code{\link{listing}}, \code{\link{map_names}},
  \code{\link{map_values}}, \code{\link{must}},
  \code{\link{unnest}}
}
\keyword{utilities}

