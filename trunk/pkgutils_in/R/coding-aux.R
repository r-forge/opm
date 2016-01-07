################################################################################


#' Prepare class names
#'
#' Ensure that a vector of class names contains only unique values and
#' \sQuote{character}. Reduce it to \sQuote{ANY} if \sQuote{ANY} is contained.
#' See \code{\link{map_values}} for a use.
#'
#' @param x Character vector.
#' @return Character vector.
#' @keywords internal
#'
prepare_class_names <- function(x) UseMethod("prepare_class_names")

#' @rdname prepare_class_names
#' @method prepare_class_names character
#' @export
#'
prepare_class_names.character <- function(x) {
  x <- unique.default(c("character", x))
  if ("ANY" %in% x)
    "ANY"
  else
    x
}


