################################################################################


#' Methods for \sQuote{lpsn_result} objects
#'
#' This package uses \sQuote{lpsn_result} objects for storing the direct results
#' of querying the \acronym{LPSN} \acronym{API} and \sQuote{records} objects for
#' storing compiled results (created from \sQuote{lpsn_result} objects).
#'
#' @param object Object of class \sQuote{lpsn_result}.
#' @param x Object of class \sQuote{lpsn_result}.
#' @param ... Optional arguments passed to other methods.
#'
#' @export
#' @return The \code{summary} function returns a list or character vector.
#'   The \code{print} method returns \code{x}, invisibly.
#' @details These methods yield or display basic information about the result of
#'   a download attempt. For real examples of their usage see
#'   \code{\link{fetch}} and friends.
#'
#'   Note that each \sQuote{lpsn_result} object also responds to \code{`$`} and
#'   \code{`[[`}. The most important key is probably \sQuote{results} as it
#'   yields the \code{API} entries (one per taxon name). \code{summary} and
#'   \code{print} show an overview of all keys.
#'
#'   When the server signals that the \code{API} query was erroneous (as opposed
#'   to just yielding zero results), the structure of the returned
#'   \sQuote{lpsn_result} object is different. While a \sQuote{results} entry
#'   should be missing, entries such as \sQuote{code} (giving the \acronym{HTTP}
#'   status code), \sQuote{message} and \sQuote{title} should be present in such
#'   a case and should indicate the kind of problem.
#'
#'   The compiled \acronym{LPSN} \acronym{API} results as returned by
#'   \code{retrieve} are of class \sQuote{records}. A dedicated
#'   \code{as.data.frame} method can convert such objects to a data frame.
#'
#'   A detailed description of the content of the \acronym{API} entries is given
#'   on the \acronym{LPSN} web site.
#'
#' @references \url{https://api.lpsn.dsmz.de/}
#' @references \url{https://lpsn.dsmz.de/text/lpsn-api}
#' @references \url{https://www.restapitutorial.com/httpstatuscodes.html}
#'
#' @family result-functions
#' @seealso \code{\link{fetch}} \code{\link{request}} \code{\link{retrieve}}
#'   \code{\link{upgrade}} \code{\link{records}}
#' @keywords print database
#' @rdname summary
#' @method summary lpsn_result
#' @export
#' @examples
#' ## Examples are deliberately not given here.
#'
summary.lpsn_result <- function(object, ...) {
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

#' @rdname summary
#' @method print lpsn_result
#' @export
#'
print.lpsn_result <- function(x, ...) {
  print_summary(x, ...)
}


################################################################################
