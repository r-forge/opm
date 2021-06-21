

#' Methods for \sQuote{lpsn_result} objects
#'
#' This package uses \sQuote{lpsn_result} objects for storing the results of a
#' query to the \acronym{LPSN} \acronym{API}.
#'
#' @param object Object of class \sQuote{lpsn_result}.
#' @param x Object of class \sQuote{lpsn_result}.
#' @param ... Optional arguments passed to other methods.
#'
#' @export
#' @return The \code{summary} function returns a list or character vector.
#'   The \code{print} method returns \code{x}, invisibly.
#' @details These methods yield or display basic information about the result of
#'   the download attempt.
#'
#'   Note that each \sQuote{lpsn_result} object also responds to \code{`$`} and
#'   \code{`[[`}. The most important key is probably \sQuote{results} as it
#'   yields the \code{API} entries (one per taxon name). \code{summary} and
#'   \code{print} show an overview of all keys.
#'
#' @references \url{https://api.lpsn.dsmz.de/}
#'
#' @family result-functions
#' @seealso \code{\link{fetch}} \code{\link{request}} \code{\link{retrieve}}
#'   \code{\link{upgrade}}
#' @keywords print database
#' @rdname summary
#' @name summary
#' @aliases summary.lpsn_result summary
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
#' @aliases print.lpsn_result print
#' @export
#'
print.lpsn_result <- function(x, ...) {
  cat(formatDL(unlist(summary(object = x, ...))), sep = "\n")
  invisible(x)
}

