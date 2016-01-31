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


################################################################################


# Used by map_values, character/numeric method and factor/numeric method.
adist2map <- function(x, max.distance = 0.1, ignore.case = TRUE,
    exclude = "", partial = FALSE, useBytes = FALSE, ...) {
  single_linkage <- function(x) {
    result <- seq_len(nrow(x))
    for (i in result) {
      j <- result %in% result[x[i, ]]
      result[j] <- max(result[j])
    }
    result
  }
  complete_linkage <- function(x) {
    result <- seq_len(nrow(x))
    for (i in rev.default(result[-1L])) {
      group <- result == result[[i]]
      for (j in rev.default(which(x[i, seq_len(i - 1L)])))
        if (all(x[j, group])) {
          result[[j]] <- result[[i]]
          group[[j]] <- TRUE
        }
    }
    result
  }
  if (nzchar(exclude))
    x <- grep(exclude, x, FALSE, TRUE, TRUE, FALSE, useBytes, TRUE)
  if (!length(x))
    return(structure(.Data = character(), names = character()))
  s <- table(x[!is.na(x) & nzchar(x)])
  s <- s[order(s, nchar(names(s)))]
  d <- adist(x = names(s), y = NULL, ignore.case = ignore.case,
    useBytes = useBytes, partial = partial, ...)
  n <- nchar(names(s), if (useBytes) "bytes" else "chars")
  f <- if (partial) pmin else pmax
  for (i in seq_len(nrow(d)))
    d[i, ] <- d[i, ] / f(n[[i]], n)
  f <- if (partial) complete_linkage else single_linkage
  d <- f(d <= max.distance)
  n <- names(s)
  n[seq_along(d)] <- n[d]
  names(n) <- names(s)
  n[names(n) != n]
}



