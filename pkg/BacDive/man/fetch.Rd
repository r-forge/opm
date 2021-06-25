\name{fetch}
\alias{fetch}
\alias{fetch.bacdive_access}
\alias{request}
\alias{request.bacdive_access}
\alias{retrieve.bacdive_access}
\alias{upgrade.bacdive_access}
\title{Querying the \acronym{BacDive} \acronym{API}}
\usage{
  fetch(object, ...)

  \method{fetch}{bacdive_access} (object, ids, ...)

  request(object, ...)

  \method{request}{bacdive_access} (object, query,
    search = c("taxon", "deposit", "16S", "genome"),
    page = 0L, ...)

  \method{retrieve}{bacdive_access} (object, query,
    search = "taxon", ...)

  \method{upgrade}{bacdive_access} (object, previous,
    keep = TRUE, ...)
}
\arguments{
  \item{object}{Object of class \sQuote{bacdive_access}.}

  \item{ids}{Numeric vector or list containing such
  vectors. If empty, \code{...} must contain at least one
  \code{ID}.}

  \item{query}{Atomic vector or list containing such
  vectors or lists. If empty, \code{...} must yield a
  non-empty query. The conversion of \code{query} depends
  on the \code{search} argument.}

  \item{search}{Character vector of length 1 determining
  which search method to apply to the query, i.e. which
  \acronym{API} endpoint to use. Each endpoint has one
  \code{search} equivalent. Processed by \code{match.arg}.
  The \code{search} argument is passed from \code{request}
  to \code{retrieve}.}

  \item{page}{Integer vector of length 1. Needed because
  the results of \code{request} are paginated. The first
  page has the number 0.}

  \item{previous}{Object of class \sQuote{bacdive_result}.}

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
  \code{upgrade} return an \sQuote{bacdive_result} object.
  In the case of \code{request} this object contains
  \acronym{BacDive} IDs. Each of them is used as a unique
  identifier by the \code{API}.

  In contrast, \code{fetch} yields full data entries, given
  \acronym{BacDive} IDs.

  \code{upgrade} yields the next data chunk of a paginated
  result. If there is no next one, if \code{keep} is
  \code{TRUE}, \code{previous} is returned, with a warning;
  otherwise \code{NULL} is returned.

  For \code{retrieve}, see the documentation of the parent
  method. Note particularly the possibility to use
  \code{handler} and \code{sleep} as arguments.

  By using \code{request}, \code{fetch} and \code{upgrade},
  users can build their own loops to download and process
  paginated results, as an alternative to \code{retrieve}.
}
\description{
  This package uses \sQuote{bacdive_access} objects for
  managing the access to the \acronym{BacDive}
  \acronym{API}. Once such an object has been created by
  applying \code{\link{open_bacdive}}, the \acronym{API}
  can be queried. The data are subject to the
  \acronym{BacDive} copyright (which is liberal, see the
  \acronym{BacDive} web site).
}
\details{
  The actual usage of \sQuote{bacdive_access} objects is
  demonstrated by querying the \acronym{BacDive}
  \acronym{API}. This is only possible for a user with a
  registered account. See \code{\link{open_bacdive}} for
  details.

  A more detailed description of how to use advanced search
  and flexible search is given on the \acronym{BacDive}
  \acronym{API} web site. These search facilities may be
  augmented in the future without the need for changes to
  this client.

  Forthcoming changes to the \acronym{BacDive}
  \acronym{API} are announced on the \acronym{BacDive}
  mailing list. Regular users of the \acronym{API} are
  advised to subscribe to this list.
}
\examples{
## Registration for BacDive is required but free and easy to accomplish.
## In real applications username and password could of course also be stored
## within the R code itself, or read from a file.
credentials <- Sys.getenv(c("DSMZ_API_USER", "DSMZ_API_PASSWORD"))

if (all(nzchar(credentials))) {

## create the BacDive access object
bacdive <- open_bacdive(credentials[[1L]], credentials[[2L]])
print(bacdive)
# it would be frustrating if the object was already expired on creation
stopifnot(!summary(bacdive)[c("expired", "refresh_expired")])

## fetch data, given some BacDive IDs
# (1) each ID given as separate argument
id1 <- fetch(bacdive, 624, 6583, 24493)
print(id1)
stopifnot(summary(id1)[["count"]] == 3L)
# conversion to data frame is possible
id1d <- as.data.frame(id1)
head(id1d)
stopifnot(is.data.frame(id1d), nrow(id1d) == 3L)

# (2) all IDs in vector
id2 <- fetch(bacdive, c(624, 6583, 24493))
stopifnot(identical(id1, id2))

# (3) as above, but simplifying a list
id3 <- fetch(bacdive, list(624, 6583, 24493))
stopifnot(identical(id1, id3))

## search for culture collection numbers
ccn1 <- request(bacdive, "DSM 26640", "deposit")
print(ccn1)
stopifnot(summary(ccn1)[["count"]] == 1L)
# conversion to data frame is possible
ccn1d <- as.data.frame(ccn1)
head(ccn1d)
stopifnot(is.data.frame(ccn1d), nrow(ccn1d) == 1L)

## search for 16S accession numbers
ssu1 <- request(bacdive, "AF000162", "16S")
print(ssu1)
stopifnot(summary(ssu1)[["count"]] == 1L)
# conversion to data frame is possible
ssu1d <- as.data.frame(ssu1)
head(ssu1d)
stopifnot(is.data.frame(ssu1d), nrow(ssu1d) == 1L)

## search for genome accession numbers
gen1 <- request(bacdive, "GCA_006094295", "genome")
print(gen1)
stopifnot(summary(gen1)[["count"]] == 1L)
# conversion to data frame is possible
gen1d <- as.data.frame(gen1)
head(gen1d)
stopifnot(is.data.frame(gen1d), nrow(gen1d) == 1L)

## search for taxon names
# (1) given as length-1 character vector
bac1 <- request(bacdive,
  "Bacillus subtilis subsp. subtilis", "taxon")
stopifnot(summary(bac1)[["count"]] > 200L)
# conversion to data frame is possible but does not yield all
# entries if the result has a non-empty 'next' component
bac1d <- as.data.frame(bac1)
head(bac1d)
stopifnot(is.data.frame(bac1d))

# (2) given separately in character vector
bac2 <- request(bacdive,
  c("Bacillus", "subtilis", "subtilis"), "taxon")
stopifnot(identical(bac2, bac1))

## run search + fetch in one step
# (a) simple example for taxon names
bg1 <- retrieve(object = bacdive,
  query = "Bacillus subtilis subsp. subtilis",
  search = "taxon", handler = NULL, sleep = 0.1)
stopifnot(summary(bac1)[["count"]] == length(bg1))
# conversion to data frame, here supposed to contain
# all entries
bg1d <- as.data.frame(bg1)
head(bg1d)
stopifnot(is.data.frame(bg1d),
  length(bg1) == nrow(bg1d))

# (b) something big
bg2 <- retrieve(object = bacdive, query = "Bacillus",
  search = "taxon", handler = NULL, sleep = 0.1)
stopifnot(length(bg2) > 1000L,
  identical(class(bg2), class(bg1)))

# (c) try a handler
bg2h <- list()
retrieve(object = bacdive, query = "Bacillus",
  search = "taxon", sleep = 0.1,
  handler = function(x) bg2h <<- c(bg2h, x))
stopifnot(length(bg2h) == length(bg2))
# there are of course better ways to use a handler

# (d) if nothing is found
nil <- retrieve(object = bacdive,
  query = "Thiscannotbefound", search = "taxon")
stopifnot(length(nil) == 0L,
  identical(class(nil), class(bg1)))
# conversion to data frame
nild <- as.data.frame(nil)
head(nild)
stopifnot(is.data.frame(nild),
  length(nil) == nrow(nild))

## and finally a refresh, whether needed or not
refresh(bacdive, TRUE)
# this is also done internally and automatically
# in some situations when apparently needed

} else {

warning("username or password missing, cannot run examples")

}
}
\references{
  \url{https://api.bacdive.dsmz.de/}

  \url{https://bacdive.dsmz.de/about}

  \url{https://bacdive.dsmz.de/mailinglist}
}
\seealso{
  \code{\link{summary.dsmz_result}} \code{\link{retrieve}}
  \code{\link{print.dsmz_result}}
  \code{\link{as.data.frame}}

  Other query.functions: \code{\link{open_bacdive}}
}
\keyword{connection}
\keyword{database}

