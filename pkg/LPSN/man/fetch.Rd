\name{fetch}
\alias{fetch}
\alias{fetch.lpsn_access}
\alias{request}
\alias{request.lpsn_access}
\alias{retrieve}
\alias{retrieve.lpsn_access}
\alias{upgrade.lpsn_access}
\title{Querying the \acronym{LPSN} \acronym{API}}
\usage{
  fetch(object, ...)

  \method{fetch}{lpsn_access} (object, ids, ...)

  request(object, ...)

  \method{request}{lpsn_access} (object, query,
    search = c("flexible", "advanced"), not = FALSE,
    page = 0L, ...)

  retrieve(object, ...)

  \method{retrieve}{lpsn_access} (object, query,
    search = "flexible", handler = NULL, sleep = 0.5, ...)

  \method{upgrade}{lpsn_access} (object, previous,
    keep = TRUE, ...)
}
\arguments{
  \item{object}{Object of class \sQuote{lpsn_access}.}

  \item{ids}{Numeric vector or list containing such
  vectors. If empty, \code{...} must contain at least one
  \code{ID}.}

  \item{query}{Atomic vector or list containing such
  vectors or lists. If empty, \code{...} must yield a
  non-empty query.}

  \item{search}{Character vector of length 1 determining
  which search method to apply to the query. Processed by
  \code{match.arg}.}

  \item{not}{Logical vector of length 1. In the case of
  \code{request} and flexible search, this negates the
  query if \code{TRUE}. The argument is ignored when
  advanced search is chosen.}

  \item{page}{Integer vector of length 1. Needed because
  the results of \code{request} are paginated. The first
  page has the number 0.}

  \item{handler}{If empty, ignored. Otherwise a function to
  which each data chunk retrieved from the \acronym{API} is
  transferred in turn. The function should accept a single
  argument. \code{retrieve} and the handler function may
  thus best be called within a dedicated enclosing
  function.}

  \item{sleep}{A waiting period in seconds between
  successive \acronym{API} requests, if any.}

  \item{previous}{Object of class \sQuote{lpsn_result}.}

  \item{keep}{Logical vector of length 1 that determines
  the return value of \code{upgrade} in case of failure.}

  \item{...}{For \code{fetch}, additional objects like
  \code{ids}. These are mandatory if and only if \code{ids}
  is empty.

  For \code{request} and \code{retrieve}, additional
  arguments to be added to \code{query}. These are
  mandatory if and only if \code{query} is empty.  When
  given, they must be named if advanced search is chosen.
  In the case of flexible search unnamed queries can be
  used but may just silently return nothing.

  For \code{upgrade}, optional arguments (currently
  ignored).}
}
\value{
  The methods for \code{fetch}, \code{request} and
  \code{upgrade} return an \sQuote{lpsn_result} object. In
  the case of \code{request} this object contains
  \acronym{LPSN} record numbers. Each of them is used as a
  unique identifier by the \code{API}.

  In contrast, \code{fetch} yields full data entries, given
  \acronym{LPSN} record numbers.

  \code{upgrade} yields the next data chunk of a paginated
  result. If there is no next one, if \code{keep} is
  \code{TRUE}, \code{previous} is returned, with a warning;
  otherwise \code{NULL} is returned.

  \code{retrieve} combines the functionality of
  \code{request}, \code{fetch} and \code{upgrade} to
  download all entries found in the \acronym{API},
  traversing all chunks of a paginated result in turn. The
  resulting list (of class \sQuote{records}) may be huge,
  hence care should be taken. It may be advisable to use
  \code{handler}. If this function is given, each chunk is
  passed to \code{handler} in turn. The \code{handler}
  function could then store the data in a database or in a
  file. If \code{handler} is given, the number of its calls
  is returned.

  By using \code{request}, \code{fetch} and \code{upgrade},
  users can build their own loops to download and process
  paginated results, as an alternative to \code{retrieve}.
}
\description{
  This package uses \sQuote{lpsn_access} objects for
  managing the access to the \acronym{LPSN} \acronym{API}.
  Once such an object has been created by applying
  \code{\link{open_lpsn}}, the \acronym{API} can be
  queried. The data are subject to the \acronym{LPSN}
  copyright (which is liberal, see the \acronym{LPSN} web
  site).
}
\details{
  The actual usage of \sQuote{lpsn_access} objects is
  demonstrated by querying the \acronym{LPSN}
  \acronym{API}. This is only possible for a user with a
  registered account. See \code{\link{open_lpsn}} for
  details.

  A more detailed description of how to use advanced search
  and flexible search is given on the \acronym{LPSN}
  \acronym{API} web site. These search facilities may be
  augmented in the future without the need for changes to
  this client.
}
\examples{
## Registration for LPSN is required but free and easy to accomplish.
## In real applications username and password could of course also be stored
## within the R code itself, or read from a file.
credentials <- Sys.getenv(c("DSMZ_API_USER", "DSMZ_API_PASSWORD"))

if (all(nzchar(credentials))) {

## create the LPSN access object
lpsn <- open_lpsn(credentials[[1L]], credentials[[2L]])
print(lpsn)
# it would be frustrating if the object was already expired on creation
stopifnot(!summary(lpsn)[c("expired", "refresh_expired")])

## fetch data, given some LPSN IDs (record numbers)
# (1) each ID given as separate argument
got1 <- fetch(lpsn, 520424, 4948, 17724)
print(got1)
stopifnot(summary(got1)[["count"]] == 3L)

# (2) all IDs in vector
got2 <- fetch(lpsn, c(520424, 4948, 17724))
stopifnot(identical(got1, got2))

# (3) as above, but simplifying a list
got3 <- fetch(lpsn, list(520424, 4948, 17724))
stopifnot(identical(got1, got3))

## run flexible search
# (1) each part of the query given as separate argument
fs1 <- request(lpsn, search = "flexible", monomial = "Flexithrix")
print(fs1)
stopifnot(summary(fs1)[["count"]] >= 2L)
# Flexible search is flexible because one can query for
# all key-value pairs available in some API entry. But
# the values are matched exactly. Compare advanced
# search.

# (2) all parts of the query given in vector
fs2 <- request(lpsn, c(monomial = "Flexithrix"), "flexible")
stopifnot(identical(fs1, fs2))

# (3) all parts of the query given in list
fs3 <- request(lpsn, list(monomial = "Flexithrix"), "flexible")
stopifnot(identical(fs1, fs3))
# flexible search also allows for nested queries

## run advanced search
# (1) each part of the query given as separate argument
as1 <- request(lpsn, search = "advanced", `taxon-name` = "Flexithrix")
print(as1)
stopifnot(summary(as1)[["count"]] >= 4L)
# Advanced search uses the equivalent of a predefined set of
# keys (or combinations thereof) to query. The values are not
# matched exactly only; case is not distinguished and substrings
# are recognized. Compare flexible search.

# (2) all parts of the query given in vector
as2 <- request(lpsn, c(`taxon-name` = "Flexithrix"), "advanced")
stopifnot(identical(as1, as2))

# (3) all parts of the query given in list
as3 <- request(lpsn, list(`taxon-name` = "Flexithrix"), "advanced")
stopifnot(identical(as1, as3))
# advanced search does not permit nested queries

## run search + fetch in one step
# (a) via flexible search
fsf <- retrieve(lpsn, search = "flexible", monomial = "Flexithrix")
stopifnot(length(fsf) == summary(fs1)[["count"]])
# it may be more convenient in R to deal with a data frame
print(dim(as.data.frame(fsf)))

# (b) via advanced search
asf <- retrieve(lpsn, search = "advanced", `taxon-name` = "Flexithrix")
stopifnot(length(asf) == summary(as1)[["count"]])
# again conversion to data frame possible
print(dim(as.data.frame(asf)))

# (c) try a handler
asfh <- list()
retrieve(lpsn, search = "advanced", `taxon-name` = "Flexithrix",
  handler = function(x) asfh <<- c(asfh, x))
stopifnot(length(asfh) == length(asf))
# there are of course better ways to use a handler

# (4) if nothing is found
nil <- retrieve(lpsn, search = "flexible", monomial = "unknown")
stopifnot(length(nil) == 0L)

# (5) and now for something huge
bac <- retrieve(lpsn, list(monomial = "Bacillus"),
  sleep = 0.1) # just for the sake of testing
stopifnot(length(bac) > 400L)
bacdf <- as.data.frame(bac)
stopifnot(nrow(bac) > 400L)

} else {

warning("username or password missing, cannot run examples")

}
}
\references{
  \url{https://api.lpsn.dsmz.de/}

  \url{https://lpsn.dsmz.de/text/copyright}
}
\seealso{
  \code{\link{summary.lpsn_result}}
  \code{\link{print.lpsn_result}}
  \code{\link{as.data.frame}}

  Other query-functions: \code{\link{open_lpsn}}
}
\keyword{connection}
\keyword{database}

