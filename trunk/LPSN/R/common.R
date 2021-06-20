get_dsmz_keycloak <- function(client_id, internal, classes, ...) {
  uri <- if (internal) "https://sso.dmz.dsmz.de" else "https://sso.dmz.dsmz.de"
  uri <- paste0(uri, "/auth/realms/dsmz/protocol/openid-connect/token")
  # this line is supposed to result in an error in case of unauthorized access
  result <- postForm(uri = uri, client_id = client_id, style = "post", ...)
  # the rest of the code just puts a convenient object together
  result <- as.environment(fromJSON(result, FALSE))
  result$dsmz_created_at <- Sys.time()
  result$dsmz_client_id <- client_id
  result$dsmz_internal <- internal
  class(result) <- c(setdiff(classes, "dsmz_keycloak"), "dsmz_keycloak")
  result
}

create_dsmz_keycloak <- function(username, password, client_id, classes,
    internal = nzchar(Sys.getenv("DSMZ_KEYCLOAK_INTERNAL", ""))) {
  get_dsmz_keycloak(username = username, password = password, classes = classes,
    internal = internal, grant_type = "password", client_id = client_id)
}

download_json <- function(base_url, endpoint, query, access_token, verbose) {
  if (!is.atomic(query))
    stop("query must be atomic vector")
  if (is.null(names(query))) {
    template <- "%s/%s/%s"
    query <- paste0(query, collapse = ";")
  } else {
    template <- "%s/%s?%s"
    query <- paste(curlEscape(names(query)), curlEscape(query),
      sep = "=", collapse = "&")
  }
  url <- sprintf(template, base_url, endpoint, query)
  if (verbose)
    message(url)
  fromJSON(getURLContent(url = url,
    httpheader = list(Accept = "application/json",
      Authorization = paste("Bearer", access_token))), TRUE, FALSE, FALSE)
}

refresh <- function(object, ...) UseMethod("refresh")

refresh.dsmz_keycloak <- function(object, ...) {
  get_dsmz_keycloak(refresh_token = get("refresh_token", object),
    client_id = get("dsmz_client_id", object), grant_type = "refresh_token",
    internal = get("dsmz_internal", object), classes = class(object), ...)
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
  cat(formatDL(summary(object = x, ...)), sep = "\n")
  invisible(x)
}

