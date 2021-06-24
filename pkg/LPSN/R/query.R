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

retrieve <- function(object, ...) UseMethod("retrieve")

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

