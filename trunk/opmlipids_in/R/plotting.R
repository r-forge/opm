

################################################################################


#' Summarise \acronym{FAME} or \acronym{FAMES} objects
#'
#' Generate a summary (which also prints nicely to the screen), or display an
#' \code{\link{FAME}} or \code{\link{FAMES}} object on screen.
#'
#' @param object \code{\link{FAME}} or \code{\link{FAMES}} object.
#' @param ... Optional arguments to be included in the output.
#' @export
#' @return For the \code{\link{FAME}} method, a named list of the class
#'   \code{FAME_Summary}, returned invisibly. The \sQuote{Metadata} entry is
#'   the number of non-list elements in \code{\link{metadata}}. For the
#'   \code{\link{FAMES}} method, a list of such lists (one per plate), also
#'   returned invisibly, with the class set to \code{FAMES_Summary}.
#' @details Currently the \code{show} methods are just wrappers for the
#'   \code{summary} methods for these objects with an additional call to
#'   \code{print}.
#' @family plotting-functions
#' @keywords attribute
#' @seealso base::summary base::formatDL methods::show base::print
#' @examples
#'
#' # FAME method
#' (x <- summary(DSM_44549[[1]]))
#' stopifnot(is.list(x))
#' DSM_44549[[1]] # calls show()
#'
#' # FAMES method
#' (x <- summary(DSM_44549))
#' stopifnot(is.list(x))
#' DSM_44549 # calls show()
#'
setGeneric("summary")

setMethod("summary", FAME, function(object, ...) {
  result <- list(Class = class(object), `Plate type` = plate_type(object),
    Measurements = dim(object@measurements),
    Metadata = sum(rapply(object@metadata, function(item) 1L)), ...)
  class(result) <- "FAME_Summary"
  result
}, sealed = SEALED)

setMethod("summary", FAMES, function(object, ...) {
  structure(lapply(X = object@plates, FUN = summary, ...), of = class(object),
    class = "FAMES_Summary")
}, sealed = SEALED)

#= show summary

setMethod("show", FAME, function(object) {
  print(summary(object))
  invisible(NULL)
}, sealed = SEALED)

setMethod("show", FAMES, function(object) {
  print(summary(object))
  invisible(NULL)
}, sealed = SEALED)


################################################################################


#' Print
#'
#' Print \code{\link{FAME}} or \code{\link{FAMES}} summaries to the screen.
#'
#' @param x Object of class \sQuote{FAME_Summary} or \sQuote{FAMES_Summary}.
#' @param omit Logical scalar indicating whether printing of \code{\link{FAME}}
#'   class and \code{\link{plate_type}} shall be omitted.
#' @param ... Optional arguments passed to \code{formatDL}.
#' @return \code{x} is returned invisibly.
#' @keywords internal
#' @name print
#'
NULL

#' @rdname print
#' @method print FAME_Summary
#' @export
#'
print.FAME_Summary <- function(x, omit = FALSE, ...) {
  do_print <- function(x, ...) {
    x <- formatDL(x = names(x), y = vapply(x, paste0, "", collapse = "/"), ...)
    lapply(x, cat, sep = "\n")
  }
  do_print(if (omit)
      x[setdiff(names(x), c("Class", "Plate type"))]
    else
      x)
  invisible(x)
}

#' @rdname print
#' @method print FAMES_Summary
#' @export
#'
print.FAMES_Summary <- function(x, omit = TRUE, ...) {
  if (size <- length(x)) {
    plate.type <- sprintf("'%s'", x[[1L]][["Plate type"]])
    for (i in seq_along(x)) {
      cat(i, sep = "\n")
      print(x[[i]], omit, ...)
      cat("\n")
    }
  } else {
    plate.type <- "unknown"
  }
  cat(sprintf("=> %s object with %i plates of %s type.", attr(x, "of"), size,
    plate.type), sep = "\n")
  invisible(x)
}


################################################################################


#' Heat map
#'
#' A method adapted to \code{\link{FAMES}} objects for \code{heat_map} from the
#' \pkg{opm} package. See there for details on most arguments.
#'
#' @param object \code{\link{FAMES}} object. Should contain at least two entries
#'   to sensibly apply this plotting method.
#' @param as.labels Character, numeric or logical vector indicating the
#'   positions of the columns to be joined and used as row labels. If
#'   \code{NULL} or empty, the row names of \code{object} are used. See
#'   \code{\link{extract}} for details.
#' @param as.groups Character, numeric or logical vector indicating the
#'   positions of the columns to be joined and used as group indicators. If
#'   \code{NULL} or empty, groups are ignored.
#' @param sep Character scalar determining how to join row and group names. See
#'   \code{\link{extract}} for details.
#' @param dups Character scalar passed \code{\link{extract}}.
#' @param exact Logical scalar passed \code{\link{extract}}.
#' @param strict Logical scalar passed \code{\link{extract}}.
#' @param ... Optional arguments passed to the matrix method of \code{heat_map}
#'   from the \pkg{opm} package. See particularly the \code{asqr} argument,
#'   which is often useful for proportion data.
#' @return See \code{heat_map} from the \pkg{opm} package. The main effect is,
#'   of course, to produce a plot in the current graphics device.
#'
#' @export
#' @family plotting-functions
#' @seealso opm::heat_map
#' @keywords hplot
#'
#' @examples
#' ## introduce better metadata entries before plotting
#' x <- DSM_44549
#' metadata(x) <- Strain ~ sub("[(].+", "", `Sample ID`)
#' heat_map(x, "Strain", dups = "ignore") # not very useful (constant)
#'
#' metadata(x) <- Time ~ tolower(substr(sub("[^(]+[(]", "", `Sample ID`), 1, 3))
#' metadata(x) <- Cultivation ~ sub("[)]", "", sub("[^,]+,", "", `Sample ID`))
#' grp <- heat_map(x, "Cultivation", "Time", dups = "ignore")
#' grp$rowColMap
#' # 'vdk' and 'ndk' means before/after conservation, respectively
#' # so after conservation the strains are the same as before
#'
#' ## use re-scaling of the data
#' heat_map(x, "Cultivation", "Time", asqr = NA, dups = "ignore")
#'
setMethod("heat_map", FAMES, function(object, as.labels, as.groups = NULL,
    sep = " ", dups = "warn", exact = TRUE, strict = TRUE, ...) {
  heat_map(extract(object = object, as.labels = as.labels, sep = sep,
    as.groups = as.groups, dups = dups, exact = exact, strict = strict), ...)
}, sealed = SEALED)


################################################################################

