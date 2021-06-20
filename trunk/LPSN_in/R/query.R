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
#' @return If login is successful, \code{refresh} returns a new
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
  create_dsmz_keycloak(username, password, "api.lpsn.public", "lpsn_access")
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
#'   \code{...} must contain at last one \code{ID}.
#' @param query Atomic vector or list containing such vectors or lists. If
#'   empty, \code{...} must yield a non-empty query.
#' @param page Integer vector of length 1. Needed because the results of
#'   \code{advanced_search} and \code{flexible_search} are paginated.
#' @param not Logical vector of length 1. In the case of \code{flexible_search}
#'   this negates the query if \code{TRUE}.
#' @param ... For \code{fetch}, additional objects like \code{ids}. These are
#'   mandatory if and only if \code{ids} is empty.
#'
#'   For \code{advanced_search}, additional arguments to be added to
#'   \code{query}. These are mandatory if and only if \code{query} is empty.
#'   When given, they must be named.
#'
#'   For \code{flexible_search}, additional arguments to be added to
#'   \code{query}. These are mandatory if and only if \code{query} is empty.
#' @export
#' @return The methods for \code{fetch}, \code{advanced_search} and
#'   \code{flexible_search} return an \sQuote{lpsn_result} object.
#' @details The actual usage of \sQuote{lpsn_access} objects is demonstrated by
#'   querying the \acronym{LPSN} \acronym{API}. This is only possible for a user
#'   with a registered account. See \code{\link{open_lpsn}} for details.
#'
#' @references \url{https://api.lpsn.dsmz.de/}
#'
#' @family query-functions
#' @keywords connection database
#' @examples
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
#' fs1 <- flexible_search(lpsn, monomial = "Flexithrix")
#' print(fs1)
#' stopifnot(summary(fs1)[["count"]] >= 2L)
#' ## Flexible search is flexible because one can query for
#' ## all key-value pairs available in some API entry. But
#' ## the values are matched exactly. Compare advanced
#' ## search.
#'
#' ## (2) all parts of the query given in vector
#' fs2 <- flexible_search(lpsn, c(monomial = "Flexithrix"))
#' stopifnot(identical(fs1, fs2))
#'
#' ## (3) all parts of the query given in list
#' ## flexible search allows for nested queries
#' fs3 <- flexible_search(lpsn, list(monomial = "Flexithrix"))
#' stopifnot(identical(fs1, fs3))
#'
#' ## run advanced search
#' ## (1) each part of the query given as separate argument
#' as1 <- advanced_search(lpsn, `taxon-name` = "Flexithrix")
#' print(as1)
#' stopifnot(summary(as1)[["count"]] >= 4L)
#' ## Advanced search uses the equivalent of a predefined set of
#' ## keys (or combinations thereof) to query. The values are not
#' ## matched exactly only; case is not distinguished and substrings
#' ## are recognized. Compare flexible search.
#'
#' ## (2) all parts of the query given in vector
#' as2 <- advanced_search(lpsn, c(`taxon-name` = "Flexithrix"))
#' stopifnot(identical(as1, as2))
#'
#' ## (3) all parts of the query given in list
#' ## advanced search allows for nested queries
#' as3 <- advanced_search(lpsn, list(`taxon-name` = "Flexithrix"))
#' stopifnot(identical(as1, as3))
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
advanced_search <- function(object, ...) UseMethod("advanced_search")

#' @rdname fetch
#' @method advanced_search lpsn_access
#' @export
#'
advanced_search.lpsn_access <- function(object, query, page = 0L, ...) {
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
}

#' @rdname fetch
#' @export
#'
flexible_search <- function(object, ...) UseMethod("flexible_search")

#' @rdname fetch
#' @method flexible_search lpsn_access
#' @export
#'
flexible_search.lpsn_access <- function(object, query, not = FALSE, page = 0L,
    ...) {
  if (missing(query))
    query <- list()
  if (!missing(...) || !is.list(query))
    query <- c(query, list(...))
  if (!length(query))
    stop("empty query")
  query <- toJSON(x = query, auto_unbox = TRUE)
  download_lpsn_json(object, "flexible_search",
    c(search = query, not = assert_scalar(not), page = assert_scalar(page)))
}


################################################################################
