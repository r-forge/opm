################################################################################


#' Creating \sQuote{lpsn_access} objects
#'
#' This package uses \sQuote{lpsn_access} objects for managing the access to the
#' \acronym{LPSN} \acronym{API}. These objects are variants of
#' \sQuote{dsmz_keycloak} objects.
#'
#' @param username Character vector of length 1.
#' @param password Character vector of length 1.
#'
#' @export
#' @return If login is successful, \code{open_lpsn} returns a new
#'   \sQuote{dsmz_keycloak} object, if otherwise an error results. Login is, of
#'   course, only successful if a user of the given name is already registered
#'   with the given password.
#' @details The actual usage of \sQuote{lpsn_access} objects is demonstrated by
#'   querying the \acronym{LPSN} \acronym{API}. See the examples given for the
#'   according functions.
#'
#'   The usage of the LPSN API requires registration, although registration is
#'   free and easy to accomplish. See the LPSN API web page.
#'
#'   The usage of the LPSN data is only permitted when in compliance with the
#'   LPSN copyright, although this copyright is liberal, see the LPSN web page.
#'
#' @references \url{https://lpsn.dsmz.de/text/copyright}
#' @references \url{https://api.lpsn.dsmz.de/}
#' @references \url{https://www.keycloak.org/}
#'
#' @family query-functions
#' @seealso \code{\link{refresh}}
#' @keywords connection database
#' @examples
#' ## Examples are deliberately not given here.
#'
open_lpsn <- function(username, password) {
  create_dsmz_keycloak(assert_scalar(username), assert_scalar(password),
    "api.lpsn.public", "lpsn_access")
}


################################################################################


#' Query the \acronym{LPSN} \acronym{API}
#'
#' This package uses \sQuote{lpsn_access} objects for managing the access to the
#' \acronym{LPSN} \acronym{API}. Given such an object, the \acronym{API} can be
#' queried.
#'
#' @param object Object of class \sQuote{lpsn_access}.
#' @param ids Numeric vector or list containing such vectors. If empty,
#'   \code{...} must contain at least one \code{ID}.
#' @param query Atomic vector or list containing such vectors or lists. If
#'   empty, \code{...} must yield a non-empty query.
#' @param search Character vector of length 1 determining which search method to
#'   apply to the query. Processed by \code{match.arg}.
#' @param not Logical vector of length 1. In the case of \code{request} and
#'   flexible search, this negates the query if \code{TRUE}. The argument is
#'   ignored when advanced search is chosen.
#' @param page Integer vector of length 1. Needed because the results of
#'   \code{request} are paginated. The first page has the number 0.
#' @param previous Object of class \sQuote{lpsn_access}.
#' @param ... For \code{fetch}, additional objects like \code{ids}. These are
#'   mandatory if and only if \code{ids} is empty.
#'
#'   For \code{request} and \code{retrieve}, additional arguments to be added to
#'   \code{query}. These are mandatory if and only if \code{query} is empty.
#'   When given, they must be named if advanced search is chosen. In the case of
#'   flexible search unnamed queries may just silently return nothing.
#'
#'   For \code{upgrade}, optional arguments (currently ignored).
#' @export
#' @return The methods for \code{fetch}, \code{request} and \code{upgrade}
#'   return an \sQuote{lpsn_result} object. In the case of \code{request} this
#'   object contains \acronym{LPSN} record numbers. Each of them is used as a
#'   unique identifier by the \code{API}. \code{upgrade} yields the next data
#'   chunk of a paginated result. If there is no next one, \code{previous} is
#'   returned, with a warning.
#'
#'   \code{fetch} yields full data entries. \code{retrieve} combines
#'   \code{request}, \code{fetch} and if necessary \code{upgrade}. This is done
#'   successively. The resulting list may be huge, hence care should be taken.
#'
#' @details The actual usage of \sQuote{lpsn_access} objects is demonstrated by
#'   querying the \acronym{LPSN} \acronym{API}. This is only possible for a user
#'   with a registered account. See \code{\link{open_lpsn}} for details.
#'
#'   A more detailed description of how to use advanced search and flexible
#'   search is given on the \acronym{LPSN} \acronym{API} web site. These search
#'   facilities may be augmented in the future without the need for changes to
#'   this client.
#'
#' @references \url{https://api.lpsn.dsmz.de/}
#'
#' @family query-functions
#' @keywords connection database
#' @examples
#' ## Registration for LPSN is required but free and easy to accomplish.
#' ## In real applications username and password could of course also be stored
#' ## within the R code itself, or read from a file.
#' credentials <- Sys.getenv(c("DSMZ_API_USER", "DSMZ_API_PASSWORD"))
#'
#' if (all(nzchar(credentials))) {
#'
#' # create the LPSN access object
#' lpsn <- open_lpsn(credentials[[1L]], credentials[[2L]])
#' print(lpsn)
#' ## it would be frustrating if the object was already expired on creation
#' stopifnot(!summary(lpsn)[c("expired", "refresh_expired")])
#'
#' ## fetch data, given some LPSN IDs (record numbers)
#' ## (1) each ID given as separate argument
#' got1 <- fetch(lpsn, 520424, 4948, 17724)
#' print(got1)
#' stopifnot(summary(got1)[["count"]] == 3L)
#'
#' ## (2) all IDs in vector
#' got2 <- fetch(lpsn, c(520424, 4948, 17724))
#' stopifnot(identical(got1, got2))
#'
#' ## (3) as above, but simplifying a list
#' got3 <- fetch(lpsn, list(520424, 4948, 17724))
#' stopifnot(identical(got1, got3))
#'
#' ## run flexible search
#' ## (1) each part of the query given as separate argument
#' fs1 <- request(lpsn, search = "flexible", monomial = "Flexithrix")
#' print(fs1)
#' stopifnot(summary(fs1)[["count"]] >= 2L)
#' ## Flexible search is flexible because one can query for
#' ## all key-value pairs available in some API entry. But
#' ## the values are matched exactly. Compare advanced
#' ## search.
#'
#' ## (2) all parts of the query given in vector
#' fs2 <- request(lpsn, c(monomial = "Flexithrix"), "flexible")
#' stopifnot(identical(fs1, fs2))
#'
#' ## (3) all parts of the query given in list
#' ## flexible search allows for nested queries
#' fs3 <- request(lpsn, list(monomial = "Flexithrix"), "flexible")
#' stopifnot(identical(fs1, fs3))
#'
#' ## run advanced search
#' ## (1) each part of the query given as separate argument
#' as1 <- request(lpsn, search = "advanced", `taxon-name` = "Flexithrix")
#' print(as1)
#' stopifnot(summary(as1)[["count"]] >= 4L)
#' ## Advanced search uses the equivalent of a predefined set of
#' ## keys (or combinations thereof) to query. The values are not
#' ## matched exactly only; case is not distinguished and substrings
#' ## are recognized. Compare flexible search.
#'
#' ## (2) all parts of the query given in vector
#' as2 <- request(lpsn, c(`taxon-name` = "Flexithrix"), "advanced")
#' stopifnot(identical(as1, as2))
#'
#' ## (3) all parts of the query given in list
#' ## advanced search allows for nested queries
#' as3 <- request(lpsn, list(`taxon-name` = "Flexithrix"), "advanced")
#' stopifnot(identical(as1, as3))
#'
#' ## run search + fetch in one step
#' ## (a) flexible search
#' fsf <- retrieve(lpsn, search = "flexible", monomial = "Flexithrix")
#' stopifnot(length(fsf) == summary(fs1)[["count"]])
#'
#' ## (b) advanced search
#' asf <- retrieve(lpsn, search = "advanced", `taxon-name` = "Flexithrix")
#' stopifnot(length(asf) == summary(as1)[["count"]])
#'
#' } else {
#'
#' warning("username or password missing, cannot run examples")
#'
#' }
#'
fetch <- function(object, ...) UseMethod("fetch")

#' @rdname fetch
#' @method fetch lpsn_access
#' @export
#'
fetch.lpsn_access <- function(object, ids, ...) {
  if (!missing(...))
    ids <- c(ids, ..., recursive = TRUE, use.names = FALSE)
  else if (!is.atomic(ids))
    ids <- unlist(ids, TRUE, FALSE)
  if (!length(ids))
    stop("no IDs given")
  if (!is.integer(ids))
    storage.mode(ids) <- "integer"
  if (anyNA(ids))
    stop("non-integer ID given")
  if (anyDuplicated.default(ids))
    ids <- unique.default(ids)
  download_lpsn_json(object, "fetch", ids)
}

#' @rdname fetch
#' @export
#'
request <- function(object, ...) UseMethod("request")

#' @rdname fetch
#' @method request lpsn_access
#' @export
#'
request.lpsn_access <- function(object, query,
    search = c("flexible", "advanced"),
    not = FALSE, page = 0L, ...) {
  switch(
    EXPR = match.arg(search),

    advanced = {
      if (missing(query))
        query <- NULL
      if (!missing(...))
        query <- c(query, ..., recursive = TRUE, use.names = TRUE)
      else if (!is.atomic(query))
        query <- unlist(query, TRUE, TRUE)
      if (!length(query))
        stop("empty query")
      if (is.null(names(query)) || !all(nzchar(names(query))))
        stop("missing names in query")
      download_lpsn_json(object, "advanced_search",
        c(query, page = assert_scalar(page)))
    },

    flexible = {
      if (missing(query))
        query <- list()
      if (!missing(...) || !is.list(query))
        query <- c(query, list(...))
      if (!length(query))
        stop("empty query")
      query <- toJSON(x = query, auto_unbox = TRUE)
      download_lpsn_json(object, "flexible_search", c(search = query,
        not = assert_scalar(not), page = assert_scalar(page)))
    },

    stop("unexpected value for 'search'")
  )
}

#' @rdname fetch
#' @export
#'
retrieve <- function(object, ...) UseMethod("retrieve")

#' @rdname fetch
#' @method retrieve lpsn_access
#' @export
#'
retrieve.lpsn_access <- function(object, query, search = "flexible", ...) {
  # store the initial chunk
  found <- request(object, query, search, ...)
  result <- vector("list", get("count", found))
  outcome <- fetch(object, get("results", found))
  size <- length(get("results", outcome))
  result[seq_len(size)] <- get("results", outcome)
  offset <- size
  # obtain the remaining chunks, if any
  while (length(get("next", found))) {
    refresh(object, TRUE)
    found <- download_lpsn_json(object, get("next", found), NULL)
    outcome <- fetch(object, get("results", found))
    size <- length(get("results", outcome))
    result[offset + seq_len(size)] <- get("results", outcome)
    offset <- offset + size
  }
  if (offset < length(result)) # not sure whether this can happen
    result[seq_len(offset)] # but you never know
  else
    result
}


#' @rdname fetch
#' @method upgrade lpsn_access
#' @export
#'
upgrade.lpsn_access <- function(object, previous, ...) {
  if (!inherits(previous, "lpsn_result"))
    stop("'previous' must be an 'lpsn_result' object")
  if (length(get("next", previous)))
    return(download_lpsn_json(object, get("next", previous), NULL))
  warning("object 'previous' lacks a 'next' entry")
  previous
}


################################################################################
