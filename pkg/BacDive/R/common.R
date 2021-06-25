print_summary <- function(x, ...) {
  cat(formatDL(unlist(summary(object = x, ...))), sep = "\n")
  invisible(x)
}

any_to_message <- function(x, sep = ": ", collapse = "; ") {
  if (!is.atomic(x))
    x <- unlist(x)
  if (is.null(names(x)))
    paste0(x, collapse = collapse)
  else
    paste(names(x), x, sep = sep, collapse = collapse)
}

assert_scalar <- function(x) {
  if (!is.atomic(x))
    stop("value is not an atomic vector")
  if (length(x) != 1L)
    stop("vector is not of length 1")
  x
}

force_integer <- function(x) {
  if (!nzchar(x))
    return(0L)
  result <- type.convert(x, "", TRUE)
  if (is.integer(result))
    result
  else if (is.double(result) || is.logical(result))
    as.integer(result)
  else
    1L
}

compose_url <- function(base_url, endpoint, query) {
  if (!is.atomic(query))
    stop("query must be atomic vector")
  if (is.null(names(query))) {
    template <- "%s/%s/%s"
    query <- paste0(query, collapse = ";")
  } else if (identical(names(query), c("", "page"))) {
    template <- "%s/%s/%s"
    query <- sprintf("%s?page=%s",
      curl_escape(query[[1L]]), curl_escape(query[[2L]]))
  } else {
    template <- "%s/%s?%s"
    query <- paste(curl_escape(names(query)), curl_escape(query),
      sep = "=", collapse = "&")
  }
  sprintf(template, base_url, endpoint, query)
}

get_dsmz_keycloak <- function(client_id, classes, verbose, internal, ...) {

  url <- if (internal) "https://sso.dmz.dsmz.de" else "https://sso.dsmz.de"
  result <- POST(path = "auth/realms/dsmz/protocol/openid-connect/token",
    url = url, body = list(client_id = client_id, ...), encode = "form")
  if (status_code(result) != 200L)
    stop("[Keycloak] ", any_to_message(content(result)))

  # the rest of the code just puts a convenient object together
  result <- as.environment(content(result))
  result$dsmz_created_at <- Sys.time()
  result$dsmz_client_id <- client_id
  result$dsmz_verbose <- verbose
  result$dsmz_internal <- internal
  class(result) <- c(setdiff(classes, "dsmz_keycloak"), "dsmz_keycloak")
  result

}

create_dsmz_keycloak <- function(username, password, client_id, classes,
    verbose = force_integer(Sys.getenv("DSMZ_API_VERBOSE")),
    internal = force_integer(Sys.getenv("DSMZ_KEYCLOAK_INTERNAL"))) {
  get_dsmz_keycloak(username = username, password = password,
    grant_type = "password", client_id = client_id,
    classes = classes, verbose = verbose, internal = internal)
}

download_json <- function(url, access_token, verbose) {
  if (verbose > 0L)
    message(url, "\n")
  GET(url = url, add_headers(Accept = "application/json",
    Authorization = paste("Bearer", access_token)))
}

download_json_with_retry <- function(url, tokens) {
  result <- download_json(url, get("access_token", tokens),
    get("dsmz_verbose", tokens))
  # one could also check that the "message" entry is "Expired token" but the
  # exact spelling of messages may be unstable
  if (status_code(result) == 401L) {
    refresh(tokens, TRUE)
    result <- download_json(url, get("access_token", tokens),
      get("dsmz_verbose", tokens))
  }
  if (status_code(result) != 200L)
    warning("[API] ", any_to_message(content(result)))
  content(result)
}

refresh <- function(object, ...) UseMethod("refresh")

refresh.dsmz_keycloak <- function(object, self = TRUE, ...) {
  result <- get_dsmz_keycloak(refresh_token = get("refresh_token", object),
    client_id = get("dsmz_client_id", object), grant_type = "refresh_token",
    classes = class(object), verbose = get("dsmz_verbose", object),
    internal = get("dsmz_internal", object), ...)
  if (self)
    list2env(as.list.environment(result), object)
  else
    result
}

summary.dsmz_keycloak <- function(object, ...) {
  age <- get("dsmz_created_at", object)
  c(
    vapply(class(object), nzchar, NA), # a trick to keep the names
    mapply(function(key) age + get(key, object) < Sys.time(),
      c(expired = "expires_in", refresh_expired = "refresh_expires_in"))
  )
}

print.dsmz_keycloak <- function(x, ...) {
  print_summary(x)
}

records <- function(object, ...) UseMethod("records")

records.list <- function(object, ...) {
  bad <- sum(!vapply(object, is.list, NA))
  if (bad > 1L)
    stop(bad, " list elements are not themselves lists")
  else if (bad > 0L)
    stop("1 list element is not itself a list")
  bad <- sum(vapply(object, function(x) is.null(names(x)), NA))
  if (bad > 1L)
    stop(bad, " list elements do not have names")
  else if (bad > 0L)
    stop("1 list element does not have names")
  class(object) <- "records"
  object
}

records.dsmz_result <- function(object, ...) {
  convert_outcome <- function(x) {
    # empty in case of error or just no outcome
    if (!length(x))
      return(list())
    # if IDs were received
    if (all(lengths(x) == 1L) && all(vapply(x, is.numeric, NA)))
      return(lapply(x, function(e) list(ID = e)))
    x
  }
  records(convert_outcome(object$results), ...)
}

as.data.frame.records <- function(x, row.names = NULL, optional = TRUE, ...) {
  rectangle <- function(x, syntactic) {
    keys <- unique.default(unlist(lapply(x, names), FALSE, FALSE))
    if (syntactic)
      keys <- make.names(keys, TRUE)
    matrix(NA, length(x), length(keys), FALSE, list(names(x), keys))
  }
  all_length_one <- function(x) {
    size <- lengths(x, FALSE)
    x[!size] <- NA
    x[size > 1L] <- lapply(x[size > 1L], list)
    x
  }
  result <- as.data.frame(x = rectangle(x, !optional),
    row.names = row.names, optional = TRUE, ...)
  if (optional)
    for (i in seq_along(x))
      result[i, names(x[[i]])] <- all_length_one(x[[i]])
  else
    for (i in seq_along(x))
      result[i, make.names(names(x[[i]]), TRUE)] <- all_length_one(x[[i]])
  result
}

as.data.frame.dsmz_result <- function(x, row.names = NULL,
    optional = TRUE, ...) {
  as.data.frame(records(x), row.names, optional, ...)
}

summary.records <- function(object, ...) {
  total <- length(object)
  span <- if (total) range(lengths(object)) else rep_len(NA_integer_, 2L)
  list(
    class = paste0(class(object), collapse = " < "),
    records = total,
    minimum_size = span[[1L]],
    maximum_size = span[[2L]]
  )
}

summary.dsmz_result <- function(object, ...) {
  c(
    list(
      class = paste0(class(object), collapse = " < "),
      parts = paste0(names(object), collapse = ", ")
    ),
    lapply(object, function(x)
      if (is.numeric(x) && length(x) == 1L)
        x
      else
        length(x) > 0L
    )
  )
}

print.records <- function(x, ...) {
  print_summary(x, ...)
}

print.dsmz_result <- function(x, ...) {
  print_summary(x, ...)
}

