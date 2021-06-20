\name{fetch}
\alias{advanced_search}
\alias{advanced_search.lpsn_access}
\alias{fetch}
\alias{fetch.lpsn_access}
\alias{flexible_search}
\alias{flexible_search.lpsn_access}
\title{Query the \acronym{LPSN} \acronym{API}}
\usage{
  fetch(object, ...)

  \method{fetch}{lpsn_access} (object, ids, ...)

  advanced_search(object, ...)

  \method{advanced_search}{lpsn_access} (object, query,
    page = 0L, ...)

  flexible_search(object, ...)

  \method{flexible_search}{lpsn_access} (object, query,
    not = FALSE, page = 0L, ...)
}
\arguments{
  \item{object}{Object of class \sQuote{lpsn_access}.}

  \item{ids}{Numeric vector or list containing such
  vectors. If empty, \code{...} must contain at last one
  \code{ID}.}

  \item{query}{Atomic vector or list containing such
  vectors or lists. If empty, \code{...} must yield a
  non-empty query.}

  \item{page}{Integer vector of length 1. Needed because
  the results of \code{advanced_search} and
  \code{flexible_search} are paginated.}

  \item{not}{Logical vector of length 1. In the case of
  \code{flexible_search} this negates the query if
  \code{TRUE}.}

  \item{...}{For \code{fetch}, additional objects like
  \code{ids}. These are mandatory if and only if \code{ids}
  is empty.

  For \code{advanced_search}, additional arguments to be
  added to \code{query}. These are mandatory if and only if
  \code{query} is empty.  When given, they must be named.

  For \code{flexible_search}, additional arguments to be
  added to \code{query}. These are mandatory if and only if
  \code{query} is empty.}
}
\value{
  The methods for \code{fetch}, \code{advanced_search} and
  \code{flexible_search} return an \sQuote{lpsn_result}
  object.
}
\description{
  This package uses \sQuote{lpsn_access} objects for
  managing the access to the \acronym{LPSN} \acronym{API}.
  Given such an object, the \acronym{API} can be queried.
}
\details{
  The actual usage of \sQuote{lpsn_access} objects is
  demonstrated by querying the \acronym{LPSN}
  \acronym{API}. This is only possible for a user with a
  registered account. See \code{\link{open_lpsn}} for
  details.
}
\examples{
credentials <- Sys.getenv(c("DSMZ_API_USER", "DSMZ_API_PASSWORD"))

if (all(nzchar(credentials))) {

# create the LPSN access object
lpsn <- open_lpsn(credentials[[1L]], credentials[[2L]])
print(lpsn)
## it would be frustrating if the object was already expired on creation
stopifnot(!summary(lpsn)[c("expired", "refresh_expired")])

## fetch data, given some LPSN IDs (record numbers)
## (1) each ID given as separate argument
got1 <- fetch(lpsn, 520424, 4948, 17724)
print(got1)
stopifnot(summary(got1)[["count"]] == 3L)

## (2) all IDs in vector
got2 <- fetch(lpsn, c(520424, 4948, 17724))
stopifnot(identical(got1, got2))

## (3) as above, but simplifying a list
got3 <- fetch(lpsn, list(520424, 4948, 17724))
stopifnot(identical(got1, got3))

## run flexible search
## (1) each part of the query given as separate argument
fs1 <- flexible_search(lpsn, monomial = "Flexithrix")
print(fs1)
stopifnot(summary(fs1)[["count"]] >= 2L)
## Flexible search is flexible because one can query for
## all key-value pairs available in some API entry. But
## the values are matched exactly. Compare advanced
## search.

## (2) all parts of the query given in vector
fs2 <- flexible_search(lpsn, c(monomial = "Flexithrix"))
stopifnot(identical(fs1, fs2))

## (3) all parts of the query given in list
## flexible search allows for nested queries
fs3 <- flexible_search(lpsn, list(monomial = "Flexithrix"))
stopifnot(identical(fs1, fs3))

## run advanced search
## (1) each part of the query given as separate argument
as1 <- advanced_search(lpsn, `taxon-name` = "Flexithrix")
print(as1)
stopifnot(summary(as1)[["count"]] >= 4L)
## Advanced search uses the equivalent of a predefined set of
## keys (or combinations thereof) to query. The values are not
## matched exactly only; case is not distinguished and substrings
## are recognized. Compare flexible search.

## (2) all parts of the query given in vector
as2 <- advanced_search(lpsn, c(`taxon-name` = "Flexithrix"))
stopifnot(identical(as1, as2))

## (3) all parts of the query given in list
## advanced search allows for nested queries
as3 <- advanced_search(lpsn, list(`taxon-name` = "Flexithrix"))
stopifnot(identical(as1, as3))

} else {

warning("username or password missing, cannot run examples")

}
}
\references{
  \url{https://api.lpsn.dsmz.de/}
}
\seealso{
  Other query-functions: \code{\link{open_lpsn}}
}
\keyword{connection}
\keyword{database}

