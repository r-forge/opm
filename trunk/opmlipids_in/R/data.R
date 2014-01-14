

################################################################################


#' Example data set
#'
#' Fatty-acid measurements exported from the \acronym{MIDI} system in
#' \acronym{RTF} format for a deposit of \emph{Corynebacterium efficiens},
#' \acronym{DSM} 44549. A total of nine measurements of the same strain are
#' included, obtained under partially distinct experimental conditions.
#'
#' @docType data
#' @name DSM_44549
#' @format Object of class \code{\link{FAMES}}.
#' @source \acronym{DSMZ} fatty-acid data collection.
#' @references
#' \url{http://www.dsmz.de/catalogues/details/culture/DSM-44549.html}
#' @keywords datasets
#' @seealso \link{read_rtf}
#' @examples
#' data(DSM_44549)
#' DSM_44549 # prints a summary
#' stopifnot(is(DSM_44549, "FAMES"))
#'
NULL


################################################################################

