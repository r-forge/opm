################################################################################
################################################################################
#
# K-means helper functions
#


#' Prepare the k for k-means
#'
#' Auxiliary function for checking and slightly adapting \code{k} for k-means
#' partitioning.
#'
#' @param object Numeric vector.
#' @return Named integer vector.
#' @keywords internal
#'
prepare_k <- function(k) {
  k <- sort.int(unique.default(must(as.integer(k))))
  if (length(k) < 1L || anyNA(k) || any(k < 1L))
    stop("'k' must contain positive numbers throughout")
  names(k) <- k
  k
}


################################################################################


