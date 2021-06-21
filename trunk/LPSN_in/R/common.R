################################################################################


# Non-public function that asserts that 'x' is an atomic vector of length 1.
#
assert_scalar <- function(x) {
  if (!is.atomic(x))
    stop("value is not an atomic vector")
  if (length(x) != 1L)
    stop("vector is not of length 1")
  x
}


# Non-public function that assists in interpreting system environment
# variables.
#
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


# Non-public helper function for create_dsmz_keycloak and friends. Returns a
# 'create_dsmz_keycloak' object.
#
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


# Non-public function that initially creates a 'dsmz_keycloak' object.
#
create_dsmz_keycloak <- function(username, password, client_id, classes,
    internal = force_integer(Sys.getenv("DSMZ_KEYCLOAK_INTERNAL", ""))) {
  get_dsmz_keycloak(username = username, password = password, classes = classes,
    internal = internal, grant_type = "password", client_id = client_id)
}


# Non-public download/conversion function.
#
download_json <- function(url, access_token, verbose) {
  if (verbose > 0L)
    message(url, "\n")
  result <- getURLContent(url = url,
    httpheader = list(Accept = "application/json",
      Authorization = paste("Bearer", access_token)))
  if (verbose > 1L)
    message(result, "\n")
  fromJSON(result, TRUE, FALSE, FALSE)
}


# Non-public conversion function.
#
compose_url <- function(base_url, endpoint, query) {
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
  sprintf(template, base_url, endpoint, query)
}


################################################################################


#' Methods for \sQuote{dsmz_keycloak} objects
#'
#' This package uses \sQuote{dsmz_keycloak} objects for managing the access to
#' the \acronym{API} services provided by \acronym{DSMZ}.
#'
#' @param object Object of class \sQuote{dsmz_keycloak}.
#' @param self Logical vector of length 1 indicating whether \code{object}
#'   should itself be modified or a new object returned.
#' @param x Object of class \sQuote{dsmz_keycloak}.
#' @param ... Optional arguments passed to other methods.
#'
#' @export
#' @return \code{refresh} returns a new or modified \sQuote{dsmz_keycloak}
#'   object if \code{object} still permits refreshing. It is supposed to do so
#'   if \sQuote{refresh_expired} is \code{FALSE}, if otherwise an error results.
#'
#'   Refreshing is supposed to be necessary if \sQuote{expired} is \code{TRUE}
#'   (although the local estimate may deviate from the actions of the server).
#'   Both values are given by \code{summary}, which returns a logical vector.
#'
#'   The \code{print} method returns \code{x}, invisibly.
#' @details The actual usage of \sQuote{dsmz_keycloak} objects is demonstrated
#'   by querying a \acronym{DSMZ} \acronym{API}. See the examples for the
#'   according functions.
#'
#' @references \url{https://www.keycloak.org/}
#'
#' @family common-functions
#' @keywords connection database print
#' @examples
#' ## Examples are deliberately not given here.
#'
refresh <- function(object, ...) UseMethod("refresh")

#' @rdname refresh
#' @method refresh dsmz_keycloak
#' @export
#'
refresh.dsmz_keycloak <- function(object, self = TRUE, ...) {
  result <- get_dsmz_keycloak(refresh_token = get("refresh_token", object),
    client_id = get("dsmz_client_id", object), grant_type = "refresh_token",
    internal = get("dsmz_internal", object), classes = class(object), ...)
  if (self)
    list2env(as.list.environment(result), object)
  else
    result
}

#' @rdname refresh
#' @method summary dsmz_keycloak
#' @export
#'
summary.dsmz_keycloak <- function(object, ...) {
  age <- get("dsmz_created_at", object)
  c(
    vapply(class(object), nzchar, NA), # a trick to keep the names
    mapply(function(key) age + get(key, object) < Sys.time(),
      c(expired = "expires_in", refresh_expired = "refresh_expires_in"))
  )
}

#' @rdname refresh
#' @method print dsmz_keycloak
#' @export
#'
print.dsmz_keycloak <- function(x, ...) {
  cat(formatDL(summary(object = x, ...)), sep = "\n")
  invisible(x)
}


################################################################################
