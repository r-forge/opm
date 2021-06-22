################################################################################


# Non-public helper function for the various print methods.
#
print_summary <- function(x, ...) {
  cat(formatDL(unlist(summary(object = x, ...))), sep = "\n")
  invisible(x)
}


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


# Return a status code if given and useful, if otherwise return 0L.
#
status_code <- function(x, key = "code") {
  result <- x[[key]]
  if (length(result) != 1L || is.na(result))
    0L
  else if (is.integer(result))
    result
  else if (is.double(result) || is.logical(result))
    as.integer(result)
  else
    0L
}


################################################################################


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


# Non-public helper function for create_dsmz_keycloak and friends. Returns a
# 'create_dsmz_keycloak' object.
#
get_dsmz_keycloak <- function(client_id, internal, classes, ...) {
  uri <- if (internal) "https://sso.dmz.dsmz.de" else "https://sso.dsmz.de"
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


################################################################################


# Non-public download/conversion function.
#
download_json <- function(url, access_token, verbose) {
  if (verbose > 0L)
    message(url, "\n")
  result <- getURL(url = url,
    httpheader = list(Accept = "application/json",
      Authorization = paste("Bearer", access_token)))
  if (verbose > 1L)
    message(result, "\n")
  fromJSON(result, TRUE, FALSE, FALSE)
}


# Non-public download/conversion function that calls download_json.
#
download_json_with_retry <- function(url, tokens, verbose) {
  result <- download_json(url, get("access_token", tokens), verbose)
  # we assume that 401 indicates that the access token was expired
  if (status_code(result) != 401L)
    return(result)
  refresh(tokens, TRUE)
  download_json(url, get("access_token", tokens), verbose)
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
  print_summary(x)
}


################################################################################


#' Methods for nested lists
#'
#' Calls to a \acronym{JSON}-based \acronym{API} may yield nested lists whose
#' conversion to a data frame may not be straightforward. The
#' \sQuote{nested_records} class of objects can assist in such conversions.
#'
#' @param object List or object of class \sQuote{nested_records} (which is a
#'   special kind of list).
#' @param x Object of class \sQuote{nested_records}.
#' @param row.names \code{NULL} or a character vector with row names for the
#'   data frame.
#' @param optional Logical vector of length 1 indicating whether creation of
#'   syntactic and unique (column) names for the data frame is optional. Note
#'   that non-unique names within \code{x}, if any, could be corrected but could
#'   still yield unexpected assignments of values to columns. The presence of
#'   empty names yields an error, hence setting \code{optional} to \code{FALSE}
#'   is necessary in that case.
#' @param ... Optional arguments passed to other methods.
#'
#' @export
#' @return The \code{as.data.frame} method creates a data frame from a list of
#'   class \sQuote{nested_records}. The \code{nested_records} method creates on
#'   object of that class if the given object passes the tests (see the
#'   examples). The other methods yield or display basic information about a
#'   \sQuote{nested_records} object.
#'
#' @family common-functions
#' @keywords list manip print
#' @examples
#' print(nested_records(list()))
#'
#' x <- nested_records(list(list(A = 1, B = 2)))
#' print(x)
#'
#' # the list elements must be lists
#' x <- try(nested_records(list(A = 1, B = 2)))
#' stopifnot(inherits(x, "try-error"))
#'
#' # the list elements must be named lists
#' x <- try(nested_records(list(list(1, 2))))
#' stopifnot(inherits(x, "try-error"))
#'
#' # resulting data frame columns can be lists
#' x <- nested_records(list(list(A = -1, B = 2), list(B = 3:4)))
#' print(x)
#' print(as.data.frame(x))
#'
#' # missing keys yield missing data (NA values)
#' x <- nested_records(list(list(A = 1, B = 2), list(C = 3, D = 4)))
#' print(x)
#' print(as.data.frame(x))
#'
nested_records <- function(object, ...) UseMethod("nested_records")

#' @rdname nested_records
#' @method nested_records list
#' @export
#'
nested_records.list <- function(object, ...) {
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
  class(object) <- "nested_records"
  object
}

#' @rdname nested_records
#' @method as.data.frame nested_records
#' @export
#'
as.data.frame.nested_records <- function(x, row.names = NULL,
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


#' @rdname nested_records
#' @method summary nested_records
#' @export
#'
summary.nested_records <- function(object, ...) {
  total <- length(object)
  span <- if (total) range(lengths(object)) else rep_len(NA_integer_, 2L)
  list(
    class = paste0(class(object), collapse = " < "),
    records = total,
    minimum_size = span[[1L]],
    maximum_size = span[[2L]]
  )
}


#' @rdname nested_records
#' @method print nested_records
#' @export
#'
print.nested_records <- function(x, ...) {
  print_summary(x, ...)
}


################################################################################

