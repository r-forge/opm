################################################################################


# Non-public helper function for the various print methods.
#
print_summary <- function(x, ...) {
  cat(formatDL(unlist(summary(object = x, ...))), sep = "\n")
  invisible(x)
}


################################################################################


# Non-public helper function that returns a character vector of length 1.
#
any_to_message <- function(x, sep = ": ", collapse = "; ") {
  if (!is.atomic(x))
    x <- unlist(x)
  if (is.null(names(x)))
    paste0(x, collapse = collapse)
  else
    paste(names(x), x, sep = sep, collapse = collapse)
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
# variables. Assumes that 'x' is a character vector of length 1.
#
#' @importFrom utils type.convert
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


################################################################################


# Non-public conversion function specific for LPSN API URLs.
#
#' @importFrom curl curl_escape
#
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


################################################################################


# Non-public helper function for create_dsmz_keycloak and friends. Returns a
# 'dsmz_keycloak' object.
#
#' @importFrom httr POST content status_code
#
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


# Non-public function that initially creates a 'dsmz_keycloak' object.
#
create_dsmz_keycloak <- function(username, password, client_id, classes,
    verbose = force_integer(Sys.getenv("DSMZ_API_VERBOSE")),
    internal = force_integer(Sys.getenv("DSMZ_KEYCLOAK_INTERNAL"))) {
  get_dsmz_keycloak(username = username, password = password,
    grant_type = "password", client_id = client_id,
    classes = classes, verbose = verbose, internal = internal)
}


################################################################################


# Non-public download/conversion function. One needs to call 'content' to obtain
# the JSON (already converted to an R object).
#
#' @importFrom httr GET add_headers
#
download_json <- function(url, access_token, verbose) {
  if (verbose > 0L)
    message(url, "\n")
  GET(url = url, add_headers(Accept = "application/json",
    Authorization = paste("Bearer", access_token)))
}


# Non-public download/conversion function that calls download_json. A single
# retry is attempted if the access token is probably expired.
#
#' @importFrom httr content status_code
#
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


# Non-public download/conversion function that calls download_json_with_retry.
#
download_any_json <- function(object, endpoint, query, classes,
    base = base_url(get("dsmz_internal", object))) {
  url <- if (length(query))
      compose_url(base, endpoint, query)
    else
      endpoint # here we assume that the full URL is already given
  result <- download_json_with_retry(url, object)
  class(result) <- classes
  result
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
#' @param handler If empty, ignored. Otherwise a function to which each data
#'   chunk retrieved from the \acronym{API} is transferred in turn. The function
#'   should accept a single argument. \code{retrieve} and the handler function
#'   may thus best be called within a dedicated enclosing function.
#' @param sleep A waiting period in seconds between successive \acronym{API}
#'   requests, if any.
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
#'
#'   \code{retrieve} combines the functionality of \code{request}, \code{fetch}
#'   and \code{upgrade} to download all entries found in the \acronym{API},
#'   traversing all chunks of a paginated result in turn. The resulting list (of
#'   class \sQuote{records}) may be huge, hence care should be taken. It may be
#'   advisable to use \code{handler}. If this function is given, each chunk is
#'   passed to \code{handler} in turn. The \code{handler} function could then
#'   store the data in a database or in a file. If \code{handler} is given, the
#'   number of its calls is returned.
#'
#' @details The actual usage of \sQuote{dsmz_keycloak} objects is demonstrated
#'   by querying a \acronym{DSMZ} \acronym{API}. See the examples for the
#'   according functions.
#'
#'   When generating a \sQuote{dsmz_keycloak} object, the package responds to a
#'   system environment variable called \sQuote{DSMZ_API_VERBOSE}. Its primary
#'   interpretation is as an integer number. Non-empty values of
#'   \sQuote{DSMZ_API_VERBOSE} that cannot be interpreted as an integer number
#'   are treated like 1; the empty character string is treated like 0.
#'
#'   When downloading data from the \acronym{API}, a verbosity of 1 means that
#'   the \acronym{URL} of each \acronym{API} request is output; when 2 or
#'   larger, more intermediary results may be shown.
#'
#' @references \url{https://www.keycloak.org/}
#' @references \url{https://www.dsmz.de/privacy-statement}
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
    classes = class(object), verbose = get("dsmz_verbose", object),
    internal = get("dsmz_internal", object), ...)
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

#' @rdname refresh
#' @export
#'
retrieve <- function(object, ...) UseMethod("retrieve")

#' @rdname refresh
#' @method retrieve dsmz_keycloak
#' @export
#'
retrieve.dsmz_keycloak <- function(object, ...,
    handler = NULL, sleep = 0.5) {

  transfer <- length(handler) > 0L
  if (transfer && !is.function(handler))
    stop("'handler' is given but is not a function")

  ## conduct initial search, determine total count and react accordingly
  found <- request(object, ...)
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
    found <- download_any_json(object, found$`next`, NULL, class(found))
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
  if (transfer) {
    result
  } else if (offset < length(result)) {
    small <- result[seq_len(offset)]
    class(small) <- "records"
    small
  } else {
    result
  }

}

################################################################################


#' Methods for nested lists and \sQuote{dsmz_result} objects
#'
#' Calls to a \acronym{JSON}-based \acronym{API} may yield nested lists whose
#' conversion to a data frame may not be straightforward. The \sQuote{records}
#' class of objects can assist in such conversions.
#'
#' @param object List or object of class \sQuote{records} (which is a special
#'   kind of list), or a list or other object to be converted to a
#'   \sQuote{records} object.
#' @param x Object of class \sQuote{records} or other object that can be
#'   converted to such an object.
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
#'   class \sQuote{records}. The \code{records} method creates an object of that
#'   class if the given object passes the tests (see the examples). The other
#'   methods yield or display basic information about a \sQuote{records} object.
#'
#'   The \code{summary} function returns a list or character vector. The
#'   \code{print} method returns \code{x}, invisibly.
#'
#' @details
#'   This package uses \sQuote{dsmz_result} objects for storing the direct
#'   results of querying the \acronym{API} and \sQuote{records} objects for
#'   storing compiled results (created from \sQuote{dsmz_result} objects).
#'
#'   Note that each \sQuote{dsmz_result} object also responds to \code{`$`}
#'   and \code{`[[`}. The most important key is probably \sQuote{results} as it
#'   yields the \code{API} entries (one per taxon name). \code{summary} and
#'   \code{print} show an overview of all keys.
#'
#'   When the server signals that the \code{API} query was erroneous (as opposed
#'   to just yielding zero results), the structure of the returned
#'   \sQuote{dsmz_result} object is different. While a \sQuote{results} entry
#'   should be missing, entries such as \sQuote{code} (giving the \acronym{HTTP}
#'   status code), \sQuote{message} and \sQuote{title} should be present in such
#'   a case and should indicate the kind of problem.
#'
#'   The compiled \acronym{API} results as returned by \code{retrieve} are of
#'   class \sQuote{records}. A dedicated \code{as.data.frame} method can convert
#'   such objects to a data frame.
#'
#' @references \url{https://www.restapitutorial.com/httpstatuscodes.html}
#'
#' @family common-functions
#' @keywords list manip print database
#' @examples
#' print(records(list()))
#'
#' x <- records(list(list(A = 1, B = 2)))
#' print(x)
#'
#' # the list elements must be lists
#' x <- try(records(list(A = 1, B = 2)))
#' stopifnot(inherits(x, "try-error"))
#'
#' # the list elements must be named lists
#' x <- try(records(list(list(1, 2))))
#' stopifnot(inherits(x, "try-error"))
#'
#' # resulting data frame columns can be lists
#' x <- records(list(list(A = -1, B = 2), list(B = 3:4)))
#' stopifnot(inherits(x, "records"))
#' print(x)
#' y <- as.data.frame(x)
#' print(y)
#' stopifnot(is.data.frame(y), anyNA(y),
#'   "list" %in% sapply(y, class), dim(y) == c(2L, 2L))
#'
#' # missing keys yield missing data (NA values)
#' x <- records(list(list(A = 1, B = 2), list(C = 3, D = 4)))
#' stopifnot(inherits(x, "records"))
#' print(x)
#' y <- as.data.frame(x)
#' print(y)
#' stopifnot(is.data.frame(y), anyNA(y),
#'   dim(y) == c(2L, 4L))
#'
records <- function(object, ...) UseMethod("records")

#' @rdname records
#' @method records list
#' @export
#'
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

#' @rdname records
#' @method records dsmz_result
#' @export
#'
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


#' @rdname records
#' @method as.data.frame records
#' @export
#'
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

#' @rdname records
#' @method as.data.frame dsmz_result
#' @export
#'
as.data.frame.dsmz_result <- function(x, row.names = NULL,
    optional = TRUE, ...) {
  as.data.frame(records(x), row.names, optional, ...)
}


#' @rdname records
#' @method summary records
#' @export
#'
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

#' @rdname records
#' @method summary dsmz_result
#' @export
#'
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


#' @rdname records
#' @method print records
#' @export
#'
print.records <- function(x, ...) {
  print_summary(x, ...)
}

#' @rdname records
#' @method print dsmz_result
#' @export
#'
print.dsmz_result <- function(x, ...) {
  print_summary(x, ...)
}


################################################################################

