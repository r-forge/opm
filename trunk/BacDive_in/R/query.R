base_url <- function(internal) {
  if (internal)
    "http://api.bacdive-dev.dsmz.local/v2"
  else
    "https://api.bacdive.dsmz.de/v2"
}

download_bacdive_json <- function(object, endpoint, query) {
  download_any_json(object, endpoint, query,
    c("bacdive_result", "dsmz_result"))
}


################################################################################


#' Creating \sQuote{bacdive_access} objects
#'
#' This package uses \sQuote{bacdive_access} objects for managing the access to
#' the \acronym{BacDive} \acronym{API}. 
#'
#' @param username Character vector of length 1.
#' @param password Character vector of length 1.
#'
#' @export
#' @return I\code{open_bacdive} returns a new \sQuote{bacdive_access} object
#'
#' @details The actual usage of \sQuote{bacdive_access} objects is demonstrated
#'   by querying the \acronym{BacDive} \acronym{API}. See the examples given for
#'   the according functions, such as \code{\link{fetch}}.
#'
#'   The usage of the \acronym{BacDive} data is only permitted when in
#'   compliance with the \acronym{BacDive} terms and conditions, see the
#'   \acronym{BacDive} web page.
#'
#' @references \url{https://bacdive.dsmz.de/about}
#' @references \url{https://bacdive.dsmz.de/mailinglist/subscribe}
#' @references \url{https://api.bacdive.dsmz.de/}
#' @references \url{https://www.keycloak.org/}
#'
#' @family query-functions
#' @seealso \code{\link{refresh}}
#' @keywords connection database
#' @examples
#' ## Examples are deliberately not given here.
#'

open_bacdive <- function(username=NULL, password=NULL) {
  result <- new.env()
  result$dsmz_internal <- FALSE
  class(result) = c("bacdive_access","dsmz")
  result
}

fetch <- function(object, ...) UseMethod("fetch")

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
  download_bacdive_json(object, "v2/fetch", ids)
}

request <- function(object, ...) UseMethod("request")

request.bacdive_access <- function(object, query,
    search = c("taxon", "deposit", "16S", "genome"), page = 0L, ...) {

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

    deposit = {
      endpoint <- "culturecollectionno"
      query <- c(assert_scalar(query), page = assert_scalar(page))
    },

    `16S` = {
      endpoint <- "sequence_16s"
      query <- c(assert_scalar(query), page = assert_scalar(page))
    },

    genome = {
      endpoint <- "sequence_genome"
      query <- c(assert_scalar(query), page = assert_scalar(page))
    },

    stop("unexpected value for 'search'")
  )

  download_bacdive_json(object, endpoint, query)

}

#' @rdname fetch
#' @method retrieve bacdive_access
#' @export
#'
retrieve.bacdive_access <- function(object, query, search = "taxon", ...) {
  NextMethod()
}

#' @importFrom utils upgrade
#' @rdname fetch
#' @method upgrade bacdive_access
#' @export
#'
upgrade.bacdive_access <- function(object, previous, keep = TRUE, ...) {
  if (!inherits(previous, "bacdive_result"))
    stop("'previous' must be a 'bacdive_result' object")
  if (length(previous$`next`))
    return(download_bacdive_json(object, previous$`next`, NULL))
  if (keep) {
    warning("object 'previous' lacks a 'next' entry")
    return(previous)
  }
  NULL
}

################################################################################
