################################################################################


# Non-public function that does the LPSN-specific download work.
#
download_lpsn_json <- function(object, endpoint, query) {
  internal <- get("dsmz_internal", object)
  url <- if (length(query))
      compose_url(if (internal)
          "http://api.pnu-dev.dsmz.local"
        else
          "https://api.lpsn.dsmz.de", endpoint, query)
    else
      endpoint # here we assume that the full URL is already given
  result <- download_json_with_retry(url, object)
  class(result) <- "lpsn_result"
  result
}


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
#'   according functions, such as \code{\link{fetch}}.
#'
#'   The usage of the \acronym{LPSN} \acronym{API} requires registration,
#'   although registration is free and easy to accomplish. See the
#'   \acronym{LPSN} \acronym{API} web page.
#'
#'   The usage of the \acronym{LPSN} data is only permitted when in compliance
#'   with the \acronym{LPSN} copyright, although this copyright is liberal, see
#'   the \acronym{LPSN} web page.
#'
#' @references \url{https://lpsn.dsmz.de/text/copyright}
#' @references \url{https://lpsn.dsmz.de/mailinglist/subscribe}
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


#' Querying the \acronym{LPSN} \acronym{API}
#'
#' This package uses \sQuote{lpsn_access} objects for managing the access to the
#' \acronym{LPSN} \acronym{API}. Once such an object has been created by
#' applying \code{\link{open_lpsn}}, the \acronym{API} can be queried. The data
#' are subject to the \acronym{LPSN} copyright (which is liberal, see the
#' \acronym{LPSN} web site).
#'
#' @param object Object of class \sQuote{lpsn_access}.
#' @param ids Numeric vector or list containing such vectors. If empty,
#'   \code{...} must contain at least one \code{ID}.
#' @param query Atomic vector or list containing such vectors or lists. If
#'   empty, \code{...} must yield a non-empty query. The conversion of
#'   \code{query} depends on the \code{search} argument, which determines the
#'   \acronym{API} endpoint to be used.
#' @param search Character vector of length 1 determining which search method to
#'   apply to the query, i.e. which \acronym{API} endpoint to use. Processed by
#'   \code{match.arg}.
#' @param not Logical vector of length 1. In the case of \code{request} and
#'   flexible search, this negates the query if \code{TRUE}. The argument is
#'   ignored when advanced search is chosen.
#' @param page Integer vector of length 1. Needed because the results of
#'   \code{request} are paginated. The first page has the number 0.
#' @param handler If empty, ignored. Otherwise a function to which each data
#'   chunk retrieved from the \acronym{API} is transferred in turn. The function
#'   should accept a single argument. \code{retrieve} and the handler function
#'   may thus best be called within a dedicated enclosing function.
#' @param sleep A waiting period in seconds between successive \acronym{API}
#'   requests, if any.
#' @param previous Object of class \sQuote{lpsn_result}.
#' @param keep Logical vector of length 1 that determines the return value of
#'   \code{upgrade} in case of failure.
#' @param ... For \code{fetch}, additional objects like \code{ids}. These are
#'   mandatory if and only if \code{ids} is empty.
#'
#'   For \code{request} and \code{retrieve}, additional arguments to be added to
#'   \code{query}. These are mandatory if and only if \code{query} is empty.
#'   When given, they must be named if advanced search is chosen. In the case of
#'   flexible search unnamed queries can be used but may just silently return
#'   nothing.
#'
#'   For \code{upgrade}, optional arguments (currently ignored).
#'
#' @return The methods for \code{fetch}, \code{request} and \code{upgrade}
#'   return an \sQuote{lpsn_result} object. In the case of \code{request} this
#'   object contains \acronym{LPSN} record numbers. Each of them is used as a
#'   unique identifier by the \code{API}.
#'
#'   In contrast, \code{fetch} yields full data entries, given \acronym{LPSN}
#'   record numbers.
#'
#'   \code{upgrade} yields the next data chunk of a paginated result. If there
#'   is no next one, if \code{keep} is \code{TRUE}, \code{previous} is returned,
#'   with a warning; otherwise \code{NULL} is returned.
#'
#'   \code{retrieve} combines the functionality of \code{request}, \code{fetch}
#'   and \code{upgrade} to download all entries found in the \acronym{API},
#'   traversing all chunks of a paginated result in turn. The resulting list (of
#'   class \sQuote{records}) may be huge, hence care should be taken. It may be
#'   advisable to use \code{handler}. If this function is given, each chunk is
#'   passed to \code{handler} in turn. The \code{handler} function could then
#'   store the data in a database or in a file. If \code{handler} is given, the
#'   number of its calls is returned.
#'
#'   By using \code{request}, \code{fetch} and \code{upgrade}, users can build
#'   their own loops to download and process paginated results, as an
#'   alternative to \code{retrieve}.
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
#'   Forthcoming changes to the \acronym{LPSN} \acronym{API} are announced on
#'   the \acronym{LPSN} mailing list. Regular users of the \acronym{API} are
#'   advised to subscribe to this list.
#'
#' @references \url{https://api.lpsn.dsmz.de/}
#' @references \url{https://lpsn.dsmz.de/text/copyright}
#' @references \url{https://lpsn.dsmz.de/mailinglist/subscribe}
#'
#' @family query-functions
#' @seealso \code{\link{summary.lpsn_result}} \code{\link{print.lpsn_result}}
#'   \code{\link{as.data.frame}}
#' @keywords connection database
#' @export
#' @examples
#' ## Registration for LPSN is required but free and easy to accomplish.
#' ## In real applications username and password could of course also be stored
#' ## within the R code itself, or read from a file.
#' credentials <- Sys.getenv(c("DSMZ_API_USER", "DSMZ_API_PASSWORD"))
#'
#' if (all(nzchar(credentials))) {
#'
#' ## create the LPSN access object
#' lpsn <- open_lpsn(credentials[[1L]], credentials[[2L]])
#' print(lpsn)
#' # it would be frustrating if the object was already expired on creation
#' stopifnot(!summary(lpsn)[c("expired", "refresh_expired")])
#'
#' ## fetch data, given some LPSN IDs (record numbers)
#' # (1) each ID given as separate argument
#' got1 <- fetch(lpsn, 520424, 4948, 17724)
#' print(got1)
#' stopifnot(summary(got1)[["count"]] == 3L)
#'
#' # (2) all IDs in vector
#' got2 <- fetch(lpsn, c(520424, 4948, 17724))
#' stopifnot(identical(got1, got2))
#'
#' # (3) as above, but simplifying a list
#' got3 <- fetch(lpsn, list(520424, 4948, 17724))
#' stopifnot(identical(got1, got3))
#'
#' ## run flexible search
#' # (1) each part of the query given as separate argument
#' fs1 <- request(lpsn, search = "flexible", monomial = "Flexithrix")
#' print(fs1)
#' stopifnot(summary(fs1)[["count"]] >= 2L)
#' # Flexible search is flexible because one can query for
#' # all key-value pairs available in some API entry. But
#' # the values are matched exactly. Compare advanced
#' # search.
#'
#' # (2) all parts of the query given in vector
#' fs2 <- request(lpsn, c(monomial = "Flexithrix"), "flexible")
#' stopifnot(identical(fs1, fs2))
#'
#' # (3) all parts of the query given in list
#' fs3 <- request(lpsn, list(monomial = "Flexithrix"), "flexible")
#' stopifnot(identical(fs1, fs3))
#' # flexible search also allows for nested queries
#'
#' ## run advanced search
#' # (1) each part of the query given as separate argument
#' as1 <- request(lpsn, search = "advanced", `taxon-name` = "Flexithrix")
#' print(as1)
#' stopifnot(summary(as1)[["count"]] >= 4L)
#' # Advanced search uses the equivalent of a predefined set of
#' # keys (or combinations thereof) to query. The values are not
#' # matched exactly only; case is not distinguished and substrings
#' # are recognized. Compare flexible search.
#'
#' # (2) all parts of the query given in vector
#' as2 <- request(lpsn, c(`taxon-name` = "Flexithrix"), "advanced")
#' stopifnot(identical(as1, as2))
#'
#' # (3) all parts of the query given in list
#' as3 <- request(lpsn, list(`taxon-name` = "Flexithrix"), "advanced")
#' stopifnot(identical(as1, as3))
#' # advanced search does not permit nested queries
#'
#' ## run search + fetch in one step
#' # (a) via flexible search
#' fsf <- retrieve(lpsn, search = "flexible", monomial = "Flexithrix")
#' stopifnot(length(fsf) == summary(fs1)[["count"]])
#' # it may be more convenient in R to deal with a data frame
#' fsfd <- as.data.frame(fsf)
#' stopifnot(is.data.frame(fsfd), nrow(fsfd) == length(fsf))
#' print(dim(fsfd))
#'
#' # (b) via advanced search
#' asf <- retrieve(lpsn, search = "advanced", `taxon-name` = "Flexithrix")
#' stopifnot(length(asf) == summary(as1)[["count"]],
#'   identical(class(asf), class(fsf)))
#' # again conversion to data frame possible
#' asfd <- as.data.frame(asf)
#' stopifnot(is.data.frame(asfd), nrow(asfd) == length(asf))
#' print(dim(asfd))
#'
#' # (c) try a handler
#' asfh <- list()
#' retrieve(lpsn, search = "advanced", `taxon-name` = "Flexithrix",
#'   handler = function(x) asfh <<- c(asfh, x))
#' stopifnot(length(asfh) == length(asf))
#' # there are of course better ways to use a handler
#'
#' # (4) if nothing is found
#' nil <- retrieve(lpsn, search = "flexible", monomial = "unknown")
#' stopifnot(length(nil) == 0L,
#'   identical(class(nil), class(fsf)))
#' # again conversion to data frame possible
#' nild <- as.data.frame(nil)
#' stopifnot(is.data.frame(nild), nrow(nild) == length(nil))
#' print(dim(nild))
#'
#' # (5) and now for something huge
#' bac <- retrieve(lpsn, list(monomial = "Bacillus"),
#'   sleep = 0.1) # just for the sake of testing
#' stopifnot(length(bac) > 400L,
#'   identical(class(bac), class(fsf)))
#' # again conversion to data frame possible
#' bacdf <- as.data.frame(bac)
#' stopifnot(is.data.frame(bacdf),
#'   nrow(bacdf) == length(bac))
#'
#' ## and finally a refresh, whether needed or not
#' refresh(lpsn, TRUE)
#' # this is also done internally and automatically
#' # in some situations when apparently needed
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

#' @importFrom jsonlite toJSON
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
retrieve.lpsn_access <- function(object, query, search = "flexible",
    handler = NULL, sleep = 0.5, ...) {

  transfer <- length(handler) > 0L
  if (transfer && !is.function(handler))
    stop("'handler' is given but is not a function")

  ## conduct initial search, determine total count and react accordingly
  found <- request(object, query, search, ...)
  total <- c(found$count, 0L)[[1L]]
  if (transfer) {
    result <- 0L
  } else {
    result <- vector("list", total)
    class(result) <- "records"
  }
  if (!total)
    return(result)

  ## obtain and store/transfer the initial chunk
  # obtain the initial chunk
  if (length(found$results))
    outcome <- fetch(object, found$results)$results
  else # avoid call of fetch without IDs
    outcome <- NULL
  # store/transfer the initial chunk
  if (transfer) {
    handler(outcome)
    result <- result + 1L
  } else {
    size <- length(outcome)
    result[seq_len(size)] <- outcome
    offset <- size
  }

  if (assert_scalar(sleep) < 0.1)
    sleep <- 0.1

  ## obtain and store/transfer the remaining chunks, if any
  while (length(found$`next`)) {
    Sys.sleep(sleep)
    # obtain the next chunk
    found <- download_lpsn_json(object, found$`next`, NULL)
    if (length(found$results))
      outcome <- fetch(object, found$results)$results
    else # avoid call of fetch without IDs
      outcome <- NULL
    # store/transfer the chunk
    if (transfer) {
      handler(outcome)
      result <- result + 1L
    } else {
      size <- length(outcome)
      result[offset + seq_len(size)] <- outcome
      offset <- offset + size
    }
  }

  ## done
  if (transfer)
    result
  else if (offset < length(result)) # not sure whether this can happen
    result[seq_len(offset)] # but you never know
  else
    result

}


#' @importFrom utils upgrade
#' @rdname fetch
#' @method upgrade lpsn_access
#' @export
#'
upgrade.lpsn_access <- function(object, previous, keep = TRUE, ...) {
  if (!inherits(previous, "lpsn_result"))
    stop("'previous' must be an 'lpsn_result' object")
  if (length(previous$`next`))
    return(download_lpsn_json(object, previous$`next`, NULL))
  if (keep) {
    warning("object 'previous' lacks a 'next' entry")
    return(previous)
  }
  NULL
}


################################################################################
