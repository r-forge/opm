\name{match_parts}
\alias{match_parts}
\title{Partial matches}
\usage{
  match_parts(x, pattern, ignore.case = FALSE)
}
\arguments{
  \item{x}{Character vector. If it has names, these names
  will be used as row names or vector names in the output.}

  \item{pattern}{Character scalar with regular expression
  that defines one to several capturing groups (partial
  matches) in parentheses. If they are named, these names
  will be used as column names in the output.
  Alternatively, if no capturing groups are present and
  thus no partial matches occur, the complete matches are
  returned as vector.}

  \item{ignore.case}{Logical scalar passed to
  \code{regexpr}.}
}
\value{
  If capturing groups are used, a matrix of mode
  \sQuote{character} with the number of rows equal to the
  length of \code{x} and one column per capturing group. If
  these were named, their names appear as column names.  If
  no capturing groups were present, a character vector of
  the same length as \code{x} is returned, containing the
  complete matches, if any.  Non-matches are always
  represented as \code{NA}.
}
\description{
  Helper function to collect partial (or complete) matches
  of (Perl-compatible) regular expressions.
}
\details{
  For discarding regular expression groups in the output,
  use non-capturing groups such as \code{(?:left|right)}.
}
\examples{
x <- structure(letters, names = LETTERS)
(m <- match_parts(x, "(?<pos1>.)"))
stopifnot(m == letters,
  colnames(m) == "pos1", rownames(m) == LETTERS)
}
\seealso{
  base::regmatches
}
\keyword{character}

