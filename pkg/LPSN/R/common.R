print_summary <- function(x, ...) {
  cat(formatDL(unlist(summary(object = x, ...))), sep = "\n")
  invisible(x)
}

object_to_message <- function(x, sep = ": ", collapse = "; ") {
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
  } else {
    template <- "%s/%s?%s"
    query <- paste(curl_escape(names(query)), curl_escape(query),
      sep = "=", collapse = "&")
  }
  sprintf(template, base_url, endpoint, query)
}

get_dsmz_keycloak <- function(client_id, internal, classes, ...) {

  url <- if (internal) "https://sso.dmz.dsmz.de" else "https://sso.dsmz.de"
  result <- POST(path = "auth/realms/dsmz/protocol/openid-connect/token",
    url = url, body = list(client_id = client_id, ...), encode = "form")
  if (status_code(result) != 200L)
    stop(object_to_message(content(result)))

  # the rest of the code just puts a convenient object together
  result <- as.environment(content(result))
  result$dsmz_created_at <- Sys.time()
  result$dsmz_client_id <- client_id
  result$dsmz_internal <- internal
  class(result) <- c(setdiff(classes, "dsmz_keycloak"), "dsmz_keycloak")
  result

}

create_dsmz_keycloak <- function(username, password, client_id, classes,
    internal = force_integer(Sys.getenv("DSMZ_KEYCLOAK_INTERNAL", ""))) {
  get_dsmz_keycloak(username = username, password = password, classes = classes,
    internal = internal, grant_type = "password", client_id = client_id)
}

download_json <- function(url, access_token, verbose) {
  if (verbose > 0L)
    message(url, "\n")
  GET(url = url, add_headers(Accept = "application/json",
    Authorization = paste("Bearer", access_token)))
}

download_json_with_retry <- function(url, tokens, verbose) {
  result <- download_json(url, get("access_token", tokens), verbose)
  # one could also check that the "message" entry is "Expired token" but the
  # exact spelling of messages may be unstable
  if (status_code(result) == 401L) {
    refresh(tokens, TRUE)
    result <- download_json(url, get("access_token", tokens), verbose)
  }
  if (status_code(result) != 200L)
    warning(object_to_message(content(result)))
  content(result)
}

refresh <- function(object, ...) UseMethod("refresh")

refresh.dsmz_keycloak <- function(object, self = TRUE, ...) {
  result <- get_dsmz_keycloak(refresh_token = get("refresh_token", object),
    client_id = get("dsmz_client_id", object), grant_type = "refresh_token",
    internal = get("dsmz_internal", object), classes = class(object), ...)
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

as.data.frame.records <- function(x, row.names = NULL,
    optional = TRUE, ...) {
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
  result <- as.data.frame(x = rectangle(x, !optional), row.names = row.names,
    optional = TRUE, make.names = FALSE, ..., stringsAsFactors = FALSE)
  if (optional)
    for (i in seq_along(x))
      result[i, names(x[[i]])] <- all_length_one(x[[i]])
  else
    for (i in seq_along(x))
      result[i, make.names(names(x[[i]]), TRUE)] <- all_length_one(x[[i]])
  result
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

print.records <- function(x, ...) {
  print_summary(x, ...)
}

