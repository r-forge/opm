

################################################################################


#' Input \acronym{FAMES} objects from files
#'
#' Read data from \acronym{RTF} files output by the \acronym{MIDI} system and
#' convert them to \code{\link{FAMES}} object.
#'
#' @param names Character vector with names of files and/or directories. Passed
#'   to \code{explode_dir} from the \pkg{opm} package.
#' @param include Character scalar with wildcard (default) or regular-expression
#'   pattern for file selection (after expansion of directories) also passed to
#'   \code{explode_dir}.
#' @param ... Optional argument also passed to \code{explode_dir}. Most of them
#'   are for the fine-tuning of file selection and removal options.
#' @param warn Passed to \code{readLines} from the \pkg{base} package.
#' @param encoding Also passed to \code{readLines}.
#' @param demo Logical scalar indicating whether the final selection of files
#'   should not be tried to input but only their file names shown.
#' @return \code{\link{FAMES}} object, its length depending on the input size
#'   (number of input \acronym{MIDI} entries: a single one, several ones, or
#'   none).
#' @details The input of \acronym{RTF} files is \strong{not} guaranteed to work
#'   if these files have been opened with some word processing software and
#'   saved again after exporting them from the \acronym{MIDI} system. It makes
#'   even less sense to attempt to read other kinds of \acronym{RTF} files with
#'   this function.
#' @family io-functions
#' @keywords IO
#' @seealso opm::explode_dir
#' @export
#' @examples
#' ## show the files that would be read from the working directory
#' read_rtf(getwd(), demo = TRUE)
#'
#' (x <- read_rtf(character())) # no files/dirs => empty object
#' stopifnot(is(x, "FAMES"), length(x) == 0)
#'
#' ## read one of the example input files that come with the package
#' (files <- pkgutils::pkg_files("opmlipids", "testdata"))
#' if (length(files)) {
#'   (x <- read_rtf(files[1], include = NULL)) # pattern not necessary here
#'   stopifnot(is(x, "FAMES"), plate_type(x) == "MIDI", length(x) == 2)
#' } else {
#'   warning("'opmlipids' example input files not found")
#' }
#'
read_rtf <- function(names, include = "*.rtf", ..., warn = FALSE,
    encoding = "unknown", demo = FALSE) {
  names <- explode_dir(names = names, include = include, ...)
  if (demo) {
    message(paste0(names, collapse = "\n"))
    return(invisible(names))
  }
  if (!length(names)) # because unlist(list()) is NULL and new() would crash
    return(new("FAMES", plates = list()))
  new("FAMES", plates = unlist(lapply(X = names, FUN = read_midi_rtf,
    warn = warn, encoding = encoding), FALSE, FALSE))
}


################################################################################
