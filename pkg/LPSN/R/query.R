base_url <- function(internal) {
  if (internal)
    "http://api.pnu-dev.dsmz.local"
  else
    "https://api.lpsn.dsmz.de"
}

download_lpsn_json <- function(object, endpoint, query) {
  download_any_json(object, endpoint, query,
    c("lpsn_result", "dsmz_result"))
}

open_lpsn <- function(username, password) {
  create_dsmz_keycloak(assert_scalar(username), assert_scalar(password),
    "api.lpsn.public", "lpsn_access")
}

fetch <- function(object, ...) UseMethod("fetch")

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

request <- function(object, ...) UseMethod("request")

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

retrieve.lpsn_access <- function(object, query, search = "flexible", ...) {
  NextMethod()
}

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

