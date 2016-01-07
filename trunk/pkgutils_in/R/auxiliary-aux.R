################################################################################
################################################################################
#
# Internal helper functions
#


#' Enclose text in Rd commands
#'
#' Enclose text in Rd commands after escaping it appropriately.
#'
#' @inheritParams pack_desc
#' @param x Character vector of Rd command names (outermost first). It is an
#'   error if an entry in \code{x} contains special characters from Rd syntax,
#'   but empty character strings and \code{NA} values are just ignored.
#' @param text Character vector or convertible to such; text to be enclosed.
#'   Needs not be appropriately escaped beforehand. Must not contain \code{NA}.
#' @return Character vector of the same length than \code{text}.
#' @keywords internal
#' @seealso tools::toRd
#'
rd_quote <- function(x, ...) UseMethod("rd_quote")

#' @rdname rd_quote
#' @method rd_quote character
#' @export
#'
rd_quote.character <- function(x, text, ...) {
  text <- toRd(na.fail(text))
  x <- x[nzchar(na.fail(x))]
  if (length(bad <- x[x != toRd(x)]))
    stop(sprintf("command name '%s' contains special characters", bad[1L]))
  for (item in rev(x))
    text <- sprintf("\\%s{%s}", item, text)
  text
}


################################################################################


#' Subset an Rd or pack_desc object
#'
#' Create a subset of an \sQuote{Rd} object based on the attributes of its
#' elements. The \sQuote{pack_desc} method reduces the input object to a list
#' containing certain relevant components.
#'
#' @inheritParams pack_desc
#' @param x Object of class \sQuote{Rd}, \sQuote{pack_desc} or
#'   \sQuote{pack_descs}.
#' @param subset Character vector with names of attributes of elements to
#'   keep.
#' @param values Logical scalar indicating whether list elements or a logical
#'   vector indicating the positions within \code{x} should be returned.
#' @return Logical vector or object of class \sQuote{Rd}. For the
#'   \sQuote{pack_descs} and \sQuote{pack_desc} method, a (nested) list.
#' @name subset
#' @keywords internal
#'
NULL

#' @rdname subset
#' @method subset Rd
#' @export
#'
subset.Rd <- function(x, subset, values = FALSE, ...) {
  prepend <- !grepl("^\\\\", subset, FALSE, TRUE)
  subset[prepend] <- sprintf("\\%s", subset[prepend])
  y <- vapply(x, attr, "", which = "Rd_tag") %in% subset
  if (L(values)) {
    x[!y] <- NULL
    x
  } else
    y
}

#' @rdname subset
#' @method subset pack_desc
#' @export
#'
subset.pack_desc <- function(x, ...) {
  result <- c("Depends", "Imports", "Suggests", "Enhances", "Collate")
  result <- structure(vector("list", length(result)), names = result)
  for (name in c("Depends", "Imports", "Suggests", "Enhances"))
    if (!is.null(y <- x[[name]])) {
      y <- unlist(strsplit(y, "\\s*,\\s*", FALSE, TRUE), FALSE, FALSE)
      y <- sub("\\s+$", "", sub("^\\s+", "", y, FALSE, TRUE), FALSE, TRUE)
      y <- sub("\\s*\\([^)]*\\)$", "", y, FALSE, TRUE)
      result[[name]] <- y[y != "R"]
    }
  for (name in "Collate")
    if (!is.null(y <- x[[name]])) {
      y <- unlist(strsplit(y, "\\s+", FALSE, TRUE))
      result[[name]] <- gsub('"', "", gsub("'", "", y, FALSE, FALSE, TRUE),
        FALSE, FALSE, TRUE)
    }
  result
}

#' @rdname subset
#' @method subset pack_descs
#' @export
#'
subset.pack_descs <- function(x, ...) {
  lapply(X = x, FUN = subset, ...)
}


################################################################################


#' Run a Ruby script provided by the package
#'
#' Run Ruby with one of the Ruby scripts that come with the \pkg{pkgutils}
#' package, and a selection of input files.
#'
#' @inheritParams repair_S4_docu
#' @param script Character scalar indicating the name of the script to use
#'   (without directory name).
#' @param sargs Character vector with optional arguments passed to the script.
#' @param ... Optional arguments passed to \code{\link{run_ruby}} as
#'   \code{args} argument.
#' @return Result of a call to \code{\link{run_ruby}}.
#' @keywords internal
#'
run_pkgutils_ruby <- function(x, ...) UseMethod("run_pkgutils_ruby")

#' @rdname run_pkgutils_ruby
#' @method run_pkgutils_ruby character
#'
run_pkgutils_ruby.character <- function(x, script, ignore, sargs = NULL, ...) {
  aux.file <- pkg_files("pkgutils", what = "auxiliary")
  aux.file <- L(aux.file[tolower(basename(aux.file)) == tolower(script)])
  x <- pkg_files(x, what = "R", installed = FALSE, ignore = ignore)
  errs <- run_ruby(x = c(aux.file, x, prepare_options(sargs)), ...)
  if (is.integer(errs) && !identical(errs, 0L))
    run_ruby(x = 1.9, ...) # to show Ruby version problems, if any
  errs
}


################################################################################


#' Prepare command-line options
#'
#' Simply routine to add leading dashes to strings for use as command-line
#' options. Works with options arguments, but only if these are separated from
#' the option by \sQuote{=}.
#'
#' @param x Character vector or \code{NULL}.
#' @return Character vector.
#' @keywords internal
#'
prepare_options <- function(x) UseMethod("prepare_options")

#' @rdname prepare_options
#' @method prepare_options NULL
#'
prepare_options.NULL <- function(x) {
  character()
}

#' @rdname prepare_options
#' @method prepare_options character
#'
prepare_options.character <- function(x) {
  x <- sub("^-+", "", x, FALSE, TRUE)
  len1 <- nchar(sub("=.*$", "", x, FALSE, TRUE)) == 1L
  x[len1] <- sprintf("-%s", x[len1])
  x[!len1] <- sprintf("--%s", x[!len1])
  x
}


################################################################################


#' Output some R object
#'
#' Generic function for outputting \R objects. Currently defined for some of
#' the classes relevant in this package.
#'
#' @inheritParams pack_desc
#' @param x Object of class \sQuote{class_desc}, \sQuote{classes_desc},
#'   \sQuote{pack_desc}, \sQuote{pack_descs} or \sQuote{Rd}.
#' @param file Character vector of file names to which the data in \code{x}
#'   shall be written.
#' @return \code{x}, returned invisibly.
#' @seealso base::cat base::write
#' @keywords internal
#'
puts <- function(x, file, ...) UseMethod("puts")

#' @rdname puts
#' @method puts classes_desc
#' @export
#'
puts.classes_desc <- function(x, file, ...) {
  invisible(structure(mapply(FUN = puts, x = x, file = file,
    MoreArgs = list(...), SIMPLIFY = FALSE), class = oldClass(x)))
}

#' @rdname puts
#' @method puts class_desc
#' @export
#'
puts.class_desc <- function(x, file, ...) {
  cat(unlist(x), file = file, sep = "\n", ...)
  invisible(x)
}

#' @rdname puts
#' @method puts Rd
#' @export
#'
puts.Rd <- function(x, file, ...) {
  cat(as.character(x, deparse = TRUE), file = file, sep = "", ...)
  invisible(x)
}

#' @rdname puts
#' @method puts pack_desc
#' @export
#'
puts.pack_desc <- function(x, file, ...) {
  write.dcf(x = unclass(x), file = file, ...)
  invisible(x)
}

#' @rdname puts
#' @method puts pack_descs
#' @export
#'
puts.pack_descs <- function(x, file, ...) {
  invisible(structure(mapply(FUN = puts, x = x, file = file,
    MoreArgs = list(...), SIMPLIFY = FALSE), class = oldClass(x)))
}


################################################################################


#' Print message indicating problem
#'
#' Simple utility to print a message indicating a certain problem. Optionally
#' also note an input file in which the problem occurs.
#'
#' @param x Character vector.
#' @param infile Name of input file. Use \code{NULL} to ignore it.
#' @param line Number of input line. Use \code{NULL} to ignore it.
#' @return Character vector.
#' @keywords internal
#'
problem <- function(x, ...) UseMethod("problem")

#' @rdname problem
#' @method problem character
#'
problem.character <- function(x, infile = NULL, line = NULL, ...) {
  infile <- sprintf(" '%s'", infile)
  line <- sprintf(" (line %i)", line)
  msg <- "PROBLEM in file"
  msg <- paste0(msg, infile, line, ": ", x, collapse = "\n")
  message(msg)
  if (nzchar(logfile <- get("logfile", PKGUTILS_OPTIONS)))
    cat(msg, sep = "\n", file = logfile, append = TRUE)
  invisible(NULL)
}


################################################################################


#' Load R files with source
#'
#' Load \R code files with \code{source} from the \pkg{base} package (which
#' does only handle one file at a time). This is mainly useful for loading
#' entire packages with \code{source}; see the \sQuote{what} argument of
#' \code{\link{pack_desc}} for an example.
#'
#' @param x Character vector of file names, or object of class
#'   \sQuote{pack_desc} or \sQuote{pack_descs}.
#' @param demo Logical scalar. See \code{\link{pack_desc}}.
#' @param ... Optional additional arguments passed between the methods and
#'   finally to \code{sys.source}.
#' @return List of lists with the results of calling \code{source}. For the
#'   action of the \sQuote{pack_desc} or \sQuote{pack_descs} methods, see
#'   \code{\link{pack_desc}}.
#' @keywords internal
#'
source_files <- function(x, ...) UseMethod("source_files")

#' @method source_files character
#' @rdname source_files
#' @export
#'
source_files.character <- function(x, ...) {
  doit <- function(file) sys.source(file = file, ...)
  invisible(sapply(x, doit, simplify = FALSE))
}

#' @method source_files pack_descs
#' @rdname source_files
#' @export
#'
source_files.pack_descs <- function(x, demo = FALSE, ...) {
  result <- sapply(X = x, FUN = source_files.pack_desc, demo = demo, ...,
    simplify = FALSE)
  if (L(demo))
    result
  else
    invisible(result)
}

#' @method source_files pack_desc
#' @rdname source_files
#' @export
#'
source_files.pack_desc <- function(x, demo = FALSE, ...) {
  y <- subset(x)[c("Depends", "Imports", "Collate")]
  y$Collate <- file.path(dirname(attr(x, "file")), "R", y$Collate)
  if (L(demo))
    return(y)
  for (pkg in unlist(y[c("Depends", "Imports")]))
    suppressPackageStartupMessages(require(pkg, character.only = TRUE,
      quietly = TRUE, warn.conflicts = FALSE))
  invisible(source_files.character(x = y$Collate, ...))
}


################################################################################
