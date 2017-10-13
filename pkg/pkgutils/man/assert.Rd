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
  return a logical vector of the length of \code{orig}.
  Contained \code{NA} values, if any, are replaced by
  \code{FALSE}.}

  \item{orig}{Mandatory if \code{cond} is (the name of) a
  function, otherwise ignored when empty or missing.
  Otherwise a vector of the length of \code{cond} if
  \code{cond} is a logical vector, or the single argument
  of \code{cond} if it is (the name of) a function.}

  \item{msg}{When empty or missing, an error message is
  constructed automatically in the case of failure.
  Otherwise either a \code{sprintf} template to be applied
  to the failing elements of \code{orig}, or a character
  scalar directly providing the error message.}

  \item{quiet}{Logical scalar determining the type of
  output in case of success (i.e., all values of
  \code{cond} are \code{TRUE}) or failure. \describe{
  \item{FALSE}{\code{TRUE} is returned when successful, an
  error is raised otherwise.} \item{NA}{\code{cond} is
  returned or the resulting logical vector when \code{cond}
  is (the name of) a function. In case of failure, a
  warning is issued. This can be used to drop parts of
  objects such as data frames, with a warning.}
  \item{TRUE}{A character vector with the descriptions of
  the problems is returned. This vector is empty in case of
  success.} }}

  \item{...}{Optional arguments passed to \code{cond} when
  it is (the name of) a function.}
}
\value{
  The type of return value depends on the values of
  \code{quiet} and \code{cond}.
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
    \code{\link{check}},
  \code{\link{collect}},   \code{\link{contains}},
  \code{\link{flatten}}, \code{\link{listing}},
  \code{\link{map_names}}, \code{\link{map_values}},
  \code{\link{must}}, \code{\link{set}}, \code{\link{sql}},
    \code{\link{unnest}}
}
\keyword{utilities}

