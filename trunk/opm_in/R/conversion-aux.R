################################################################################


#' Factor from flattened data
#'
#' Extract all plate-specifying information from a data frame as created by
#' \code{\link{flatten}}. If metadata have been included, these will be joined
#' together; otherwise the plate identifiers (basically numbers) themselves are
#' used.
#'
#' @param object Object as returned by \code{\link{flatten}}.
#' @param sep Character scalar. Separator used for joining the columns together.
#' @return Factor with one entry per plate.
#' @keywords internal
#'
setGeneric("flattened_to_factor",
  function(object, ...) standardGeneric("flattened_to_factor"))

setMethod("flattened_to_factor", "data.frame", function(object, sep = " ") {
  LL(plate.pos <- which(colnames(object) == RESERVED_NAMES[["plate"]]), sep)
  if (plate.pos == 1L)
    return(unique(object[, plate.pos]))
  result <- aggregate(object[, seq_len(plate.pos)],
    by = list(object[, plate.pos]), FUN = `[[`, i = 1L)
  result <- as.list(result[, seq.int(2L, ncol(result) - 1L), drop = FALSE])
  as.factor(do.call(paste, c(result, sep = sep)))
}, sealed = SEALED)


