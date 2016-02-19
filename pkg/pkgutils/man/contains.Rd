\name{contains}
\alias{check}
\alias{contains}
\title{Query an object with another object}
\arguments{
  \item{object}{List or data frame containing the data.}

  \item{against}{Character vector whose names indicate
  column names of \code{object} and whose values indicate
  types or classes to assert.}

  \item{other}{List used as query.}

  \item{values}{Logical scalar. Compare also the values or
  only the keys? If \code{FALSE}, \code{exact} is ignored.}

  \item{exact}{Logical scalar. If \code{FALSE}, the data
  value(s) might by any of the query value(s), and some
  coercion is done before comparing (see \code{match} for
  details.

  If \code{TRUE}, the data value(s) must exactly correspond
  to the query value(s), and no coercion is done (see
  \code{identical}) for details). This might be too strict
  for most applications.}

  \item{...}{Optional arguments passed to \code{identical}
  from the \pkg{base} package, allowing for fine-control of
  identity. Has no effect unless \code{exact} is
  \code{TRUE}.}
}
\value{
  \code{contains} yields a logical scalar, \code{check} a
  (potentially empty) character vector describing each
  failed assertion.
}
\description{
  One use of \code{contains} is to test whether all names
  of a query list occur as names in a data list and
  optionally also whether they point to the same elements;
  this principle is applied recursively to all contained
  lists. The \code{check} methods apply various tests to
  objects.
}
\details{
  Non-list elements are ignored by \code{contains} if
  \code{values} is \code{FALSE}. Otherwise the comparison
  is done using \code{identical} if \code{exact} is
  \code{TRUE}. If \code{exact} is \code{FALSE}, the
  value(s) in the data list can be any of the values at the
  corresponding position in the query list, and the
  comparison is done by coercion to character vectors. An
  empty query list results in \code{TRUE}. Missing names in
  a non-empty query list result in \code{FALSE}.

  The \code{check} method for data frames tests for the
  presence of each column listed by \code{against} in
  \code{object}. For the columns found, it checks whether
  \code{is.<name>} returns \code{TRUE}, which \code{<name>}
  given by the according element of \code{against}. It is
  an error if the function \code{is.<name>} does not exist.
}
\examples{
# contains() list/list method
x <- list(a = 1:8, c = 9, d = list(d1 = 'x', d2 = 'y'))
y <- list(a = 1:10, c = "9", d = list(d1 = "x"))
stopifnot(contains(x, y))
stopifnot(!contains(x, y, exact = TRUE))
stopifnot(contains(x, y, exact = TRUE, values = FALSE))

# check() data.frame/character method
(x <- check(Puromycin,
  c(conc = "numeric", rate = "numeric", state = "factor")))
(y <- check(Puromycin,
  c(missing = "numeric", rate = "numeric", state = "character")))
stopifnot(is.character(x), is.character(y))
}
\seealso{
  base::list base::as.list base::`[` base::`[[` base::match

  base::identity

  Other coding-functions: \code{\link{L}},
  \code{\link{LL}}, \code{\link{case}},
    \code{\link{collect}},   \code{\link{flatten}},
  \code{\link{listing}}, \code{\link{map_names}},
  \code{\link{map_values}}, \code{\link{must}}
}
\keyword{attribute}
\keyword{list}

%% BEGIN INSERTION BY repair_S4_docu.rb
\docType{methods}
\alias{check-methods}
\alias{check,data.frame,character-method}
\alias{contains-methods}
\alias{contains,list,list-method}
\usage{
  \S4method{check}{data.frame,character}(object, against) 

  \S4method{contains}{list,list}(object, other, values = TRUE, exact = FALSE, ...)
}
%% END INSERTION BY repair_S4_docu.rb
