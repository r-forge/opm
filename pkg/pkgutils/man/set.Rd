\name{set}
\alias{set}
\title{Assign values only when needed}
\usage{
  set(name, expr, template = "\%s.Rda",
    env = parent.frame(), inherits = TRUE)
}
\arguments{
  \item{name}{Character scalar indicating the name of an
  object to be assigned.}

  \item{expr}{Expression that will only be evaluated if
  \code{name} does not yet exist and, optionally, cannot be
  read from an \code{Rda} file.}

  \item{template}{Character scalar with a template as used
  by \code{sprintf} from the base package, with the
  placeholder, if any, referring to \code{name}. Set this
  to an empty object to turn of the \code{Rda} file
  mechanism.}

  \item{env}{Passed as \code{envir} argument to
  \code{assign}.}

  \item{inherits}{Passed as \code{inherits} argument to
  \code{assign}.}
}
\value{
  An invisible returned integer code indicating what has
  been done. \describe{ \item{0}{The object already
  existed, nothing was done.} \item{1}{The result of
  \code{expr} was assigned to the object, the \code{Rda}
  file mechanism was not used.} \item{2}{The given
  \code{Rda} file was found and its content assigned to the
  object.} \item{3}{The given \code{Rda} file was not
  found. The result of \code{expr} was assigned to the
  object and was also stored in the given \code{Rda} file.}
  }
}
\description{
  A helper function for avoiding long-running computations
  if the object to be generated is either already present
  or can be read from an \code{Rda} file.
}
\details{
  If the \code{Rda} file name has no directory component,
  it is assumed to be located in the directory given by
  \code{getOption("rda_store")} and, if this does not
  exist, in the directory given by \code{getwd()}.
}
\examples{
the_answer <- function() {
  print("answer requested")
  42L
}
set("answer", the_answer(), NULL) # prints 'answer requested'
set("answer", the_answer(), NULL) # does not print
answer # 42
}
\seealso{
  base::assign base::readRDS base::saveRDS

  Other coding-functions: \code{\link{L}},
  \code{\link{LL}}, \code{\link{assert}},
  \code{\link{case}},     \code{\link{check}}, \code{\link{collect}},
    \code{\link{contains}}, \code{\link{flatten}},
  \code{\link{listing}}, \code{\link{map_names}},
  \code{\link{map_values}}, \code{\link{must}},
  \code{\link{sql}},   \code{\link{unnest}}
}
\keyword{IO}
\keyword{utilities}

