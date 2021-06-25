################################################################################


# Non-public function that does the BacDive-specific download work.
#
download_bacdive_json <- function(object, endpoint, query) {
  internal <- get("dsmz_internal", object)
  url <- if (length(query))
      compose_url(if (internal)
          "http://api.bacdive-dev.dsmz.local"
        else
          "https://api.bacdive.dsmz.de", endpoint, query)
    else
      endpoint # here we assume that the full URL is already given
  result <- download_json_with_retry(url, object)
  class(result) <- c("bacdive_result", "dsmz_result")
  result
}


################################################################################


#' Creating \sQuote{bacdive_access} objects
#'
#' This package uses \sQuote{bacdive_access} objects for managing the access to
#' the \acronym{BacDive} \acronym{API}. These objects are variants of
#' \sQuote{dsmz_keycloak} objects.
#'
#' @param username Character vector of length 1.
#' @param password Character vector of length 1.
#'
#' @export
#' @return If login is successful, \code{open_bacdive} returns a new
#'   \sQuote{dsmz_keycloak} object, if otherwise an error results. Login is, of
#'   course, only successful if a user of the given name is already registered
#'   with the given password.
#' @details The actual usage of \sQuote{bacdive_access} objects is demonstrated
#'   by querying the \acronym{BacDive} \acronym{API}. See the examples given for
#'   the according functions, such as \code{\link{fetch}}.
#'
#'   The usage of the \acronym{BacDive} \acronym{API} requires registration,
#'   although registration is free and easy to accomplish. See the
#'   \acronym{BacDive} \acronym{API} web page.
#'
#'   The usage of the \acronym{BacDive} data is only permitted when in
#'   compliance with the \acronym{BacDive} terms and conditions, see the
#'   \acronym{BacDive} web page.
#'
#' @references \url{https://bacdive.dsmz.de/about}
#' @references \url{https://bacdive.dsmz.de/mailinglist}
#' @references \url{https://api.bacdive.dsmz.de/}
#' @references \url{https://www.keycloak.org/}
#'
#' @family query-functions
#' @seealso \code{\link{refresh}}
#' @keywords connection database
#' @examples
#' ## Examples are deliberately not given here.
#'
open_bacdive <- function(username, password) {
  create_dsmz_keycloak(assert_scalar(username), assert_scalar(password),
    "api.bacdive.public", "bacdive_access")
}


################################################################################


#' Querying the \acronym{BacDive} \acronym{API}
#'
#' This package uses \sQuote{bacdive_access} objects for managing the access to
#' the \acronym{BacDive} \acronym{API}. Once such an object has been created by
#' applying \code{\link{open_bacdive}}, the \acronym{API} can be queried. The
#' data are subject to the \acronym{BacDive} copyright (which is liberal, see
#' the \acronym{BacDive} web site).
#'
#' @param object Object of class \sQuote{bacdive_access}.
#' @param ids Numeric vector or list containing such vectors. If empty,
#'   \code{...} must contain at least one \code{ID}.
#' @param query Atomic vector or list containing such vectors or lists. If
#'   empty, \code{...} must yield a non-empty query. The conversion of
#'   \code{query} depends on the \code{search} argument.
#' @param search Character vector of length 1 determining which search method to
#'   apply to the query, i.e. which \acronym{API} endpoint to use. Each endpoint
#'   has two aliases. Processed by \code{match.arg}.
#' @param page Integer vector of length 1. Needed because the results of
#'   \code{request} are paginated. The first page has the number 0.
#' @param handler If empty, ignored. Otherwise a function to which each data
#'   chunk retrieved from the \acronym{API} is transferred in turn. The function
#'   should accept a single argument. \code{retrieve} and the handler function
#'   may thus best be called within a dedicated enclosing function.
#' @param sleep A waiting period in seconds between successive \acronym{API}
#'   requests, if any.
#' @param previous Object of class \sQuote{bacdive_result}.
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
#'   return an \sQuote{bacdive_result} object. In the case of \code{request}
#'   this object contains \acronym{BacDive} IDs. Each of them is used as a
#'   unique identifier by the \code{API}.
#'
#'   In contrast, \code{fetch} yields full data entries, given \acronym{BacDive}
#'   IDs.
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
#' @details The actual usage of \sQuote{bacdive_access} objects is demonstrated
#'   by querying the \acronym{BacDive} \acronym{API}. This is only possible for
#'   a user with a registered account. See \code{\link{open_bacdive}} for
#'   details.
#'
#'   A more detailed description of how to use advanced search and flexible
#'   search is given on the \acronym{BacDive} \acronym{API} web site. These
#'   search facilities may be augmented in the future without the need for
#'   changes to this client.
#'
#'   Forthcoming changes to the \acronym{BacDive} \acronym{API} are announced on
#'   the \acronym{BacDive} mailing list. Regular users of the \acronym{API} are
#'   advised to subscribe to this list.
#'
#' @references \url{https://api.bacdive.dsmz.de/}
#' @references \url{https://bacdive.dsmz.de/about}
#' @references \url{https://bacdive.dsmz.de/mailinglist}
#'
#' @family query-functions
#' @seealso \code{\link{summary.dsmz_result}}
#'   \code{\link{print.dsmz_result}} \code{\link{as.data.frame}}
#' @keywords connection database
#' @export
#' @examples
#' ## Registration for BacDive is required but free and easy to accomplish.
#' ## In real applications username and password could of course also be stored
#' ## within the R code itself, or read from a file.
#' credentials <- Sys.getenv(c("DSMZ_API_USER", "DSMZ_API_PASSWORD"))
#'
#' if (all(nzchar(credentials))) {
#'
#' ## create the BacDive access object
#' bacdive <- open_bacdive(credentials[[1L]], credentials[[2L]])
#' print(bacdive)
#' # it would be frustrating if the object was already expired on creation
#' stopifnot(!summary(bacdive)[c("expired", "refresh_expired")])
#'
#' ## fetch data, given some BacDive IDs
#' # (1) each ID given as separate argument
#' id1 <- fetch(bacdive, 624, 6583, 24493)
#' print(id1)
#' stopifnot(summary(id1)[["count"]] == 3L)
#' # conversion to data frame is possible
#' id1d <- as.data.frame(id1)
#' head(id1d)
#' stopifnot(is.data.frame(id1d), nrow(id1d) == 3L)
#'
#' # (2) all IDs in vector
#' id2 <- fetch(bacdive, c(624, 6583, 24493))
#' stopifnot(identical(id1, id2))
#'
#' # (3) as above, but simplifying a list
#' id3 <- fetch(bacdive, list(624, 6583, 24493))
#' stopifnot(identical(id1, id3))
#'
#' ## search for culture collection numbers
#' ccn1 <- request(bacdive, "DSM 26640", "deposit")
#' print(ccn1)
#' stopifnot(summary(ccn1)[["count"]] == 1L)
#' # conversion to data frame is possible
#' ccn1d <- as.data.frame(ccn1)
#' head(ccn1d)
#' stopifnot(is.data.frame(ccn1d), nrow(ccn1d) == 1L)
#'
#' ## search for 16S accession numbers
#' ssu1 <- request(bacdive, "AF000162", "16S")
#' print(ssu1)
#' stopifnot(summary(ssu1)[["count"]] == 1L)
#' # conversion to data frame is possible
#' ssu1d <- as.data.frame(ssu1)
#' head(ssu1d)
#' stopifnot(is.data.frame(ssu1d), nrow(ssu1d) == 1L)
#'
#' ## search for genome accession numbers
#' gen1 <- request(bacdive, "GCA_006094295", "genome")
#' print(gen1)
#' stopifnot(summary(gen1)[["count"]] == 1L)
#' # conversion to data frame is possible
#' gen1d <- as.data.frame(gen1)
#' head(gen1d)
#' stopifnot(is.data.frame(gen1d), nrow(gen1d) == 1L)
#'
#' ## search for taxon names
#' # (1) given as length-1 character vector
#' bac1 <- request(bacdive,
#'   "Bacillus subtilis subsp. subtilis", "taxon")
#' stopifnot(summary(bac1)[["count"]] > 200L)
#' # conversion to data frame is possible but does not yield all
#' # entries if the result has a non-empty 'next' component
#' bac1d <- as.data.frame(bac1)
#' head(bac1d)
#' stopifnot(is.data.frame(bac1d))
#'
#' # (2) given separately in character vector
#' bac2 <- request(bacdive,
#'   c("Bacillus", "subtilis", "subtilis"), "taxon")
#' stopifnot(identical(bac2, bac1))
#'
#' ## run search + fetch in one step
#' # (a) simple example for taxon names
#' bg1 <- retrieve(bacdive,
#'   "Bacillus subtilis subsp. subtilis",
#'   "taxon", NULL, 0.1)
#' stopifnot(summary(bac1)[["count"]] == length(bg1))
#' # conversion to data frame, here supposed to contain
#' # all entries
#' bg1d <- as.data.frame(bg1)
#' head(bg1d)
#' stopifnot(is.data.frame(bg1d),
#'   length(bg1) == nrow(bg1d))
#'
#' # (b) something big
#' bg2 <- retrieve(bacdive,
#'   "Bacillus", "taxon", NULL, 0.1)
#' stopifnot(length(bg2) > 1000L,
#'   identical(class(bg2), class(bg1)))
#'
#' # (c) try a handler
#' bg2h <- list()
#' retrieve(bacdive, "Bacillus", "taxon",
#'   function(x) bg2h <<- c(bg2h, x))
#' stopifnot(length(bg2h) == length(bg2))
#' # there are of course better ways to use a handler
#'
#' # (d) if nothing is found
#' nil <- retrieve(bacdive, "Thiscannotbefound", "taxon")
#' stopifnot(length(nil) == 0L,
#'   identical(class(nil), class(bg1)))
#' # conversion to data frame
#' nild <- as.data.frame(nil)
#' head(nild)
#' stopifnot(is.data.frame(nild),
#'   length(nil) == nrow(nild))
#'
#' ## and finally a refresh, whether needed or not
#' refresh(bacdive, TRUE)
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
#' @method fetch bacdive_access
#' @export
#'
fetch.bacdive_access <- function(object, ids, ...) {
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
  download_bacdive_json(object, "fetch", ids)
}

#' @rdname fetch
#' @export
#'
request <- function(object, ...) UseMethod("request")

#' @importFrom utils head
#' @rdname fetch
#' @method request bacdive_access
#' @export
#'
request.bacdive_access <- function(object, query,
    search = c(
      "taxonomy", "taxon",
      "deposit", "culturecollectionno",
      "16S", "sequence_16s",
      "genome", "sequence_genome"
      ), page = 0L, ...) {

  taxon_query <- function(x) {
    x <- unlist(strsplit(x, "\\W+", FALSE, TRUE), FALSE, FALSE)
    x <- setdiff(tolower(x[nzchar(x)]), "subsp")
    if (!length(x))
      stop("empty query after filtering for taxon-name components")
    paste0(head(x, 3L), collapse = "/")
  }

  if (missing(query))
    query <- NULL
  if (!missing(...))
    query <- c(query, ..., recursive = TRUE, use.names = TRUE)
  else if (!is.atomic(query))
    query <- unlist(query, TRUE, TRUE)
  if (!length(query))
    stop("empty query")

  switch(
    EXPR = match.arg(search),

    taxonomy =,
    taxon = {
      endpoint <- paste0("taxon/", taxon_query(query))
      query <- c(page = assert_scalar(page))
    },

    deposit =,
    culturecollectionno = {
      endpoint <- "culturecollectionno"
      query <- c(assert_scalar(query), page = assert_scalar(page))
    },

    `16S` =,
    sequence_16s = {
      endpoint <- "sequence_16s"
      query <- c(assert_scalar(query), page = assert_scalar(page))
    },

    genome =,
    sequence_genome = {
      endpoint <- "sequence_genome"
      query <- c(assert_scalar(query), page = assert_scalar(page))
    },

    stop("unexpected value for 'search'")
  )

  download_bacdive_json(object, endpoint, query)

}

#' @rdname fetch
#' @export
#'
retrieve <- function(object, ...) UseMethod("retrieve")

#' @rdname fetch
#' @method retrieve bacdive_access
#' @export
#'
retrieve.bacdive_access <- function(object, query, search = "flexible",
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
    found <- download_bacdive_json(object, found$`next`, NULL)
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
#' @method upgrade bacdive_access
#' @export
#'
upgrade.bacdive_access <- function(object, previous, keep = TRUE, ...) {
  if (!inherits(previous, "bacdive_result"))
    stop("'previous' must be an 'bacdive_result' object")
  if (length(previous$`next`))
    return(download_bacdive_json(object, previous$`next`, NULL))
  if (keep) {
    warning("object 'previous' lacks a 'next' entry")
    return(previous)
  }
  NULL
}


################################################################################
