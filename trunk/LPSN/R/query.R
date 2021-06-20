open_lpsn <- function(username, password) {
  create_dsmz_keycloak(username, password, "api.lpsn.public", "lpsn_access")
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

advanced_search <- function(object, ...) UseMethod("advanced_search")

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

flexible_search <- function(object, ...) UseMethod("flexible_search")

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

