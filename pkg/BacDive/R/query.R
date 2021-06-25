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

open_bacdive <- function(username, password) {
  create_dsmz_keycloak(assert_scalar(username), assert_scalar(password),
    "api.bacdive.public", "bacdive_access")
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
  download_bacdive_json(object, "fetch", ids)
}

request <- function(object, ...) UseMethod("request")

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

retrieve <- function(object, ...) UseMethod("retrieve")

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

