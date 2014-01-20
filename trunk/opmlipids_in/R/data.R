

################################################################################


#' Example data sets
#'
#' Fatty-acid measurements exported from the \acronym{MIDI} system in
#' \acronym{RTF} format and input with \code{\link{read_rtf}} for a deposit of
#' \emph{Corynebacterium efficiens}, \acronym{DSM} 44549, and one of \emph{C.
#' urealyticum}, \acronym{DSM} 7109. A total of nine measurements of
#' \acronym{DSM} 44549 and two of \acronym{DSM} 7109 are included, obtained
#' under partially distinct experimental conditions.
#'
#' @docType data
#' @name DSM_44549
#' @aliases DSM_7109
#' @format Objects of class \code{\link{FAMES}} and plate type \code{"MIDI"}.
#' @source \acronym{DSMZ} fatty-acid data collection.
#' @references
#' \url{http://www.dsmz.de/catalogues/details/culture/DSM-44549.html}
#' @references
#' \url{http://www.dsmz.de/catalogues/details/culture/DSM-7109.html}
#' @keywords datasets
#' @seealso \link{read_rtf}
#' @examples
#' data(DSM_44549)
#' DSM_44549 # prints a summary
#' stopifnot(is(DSM_44549, "FAMES"))
#'
#' data(DSM_7109)
#' DSM_7109
#' stopifnot(is(DSM_7109, "FAMES"))
#'
NULL


################################################################################

