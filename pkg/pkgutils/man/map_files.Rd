\name{map_files}
\alias{clean_filenames}
\alias{clean_filenames.character}
\alias{map_filenames}
\alias{map_filenames.character}
\alias{map_files}
\alias{map_files.character}
\title{Map files or file names}
\usage{
  map_files(x, ...)

  \method{map_files}{character} (x, mapfun, ...,
    .attr = ".filename", .encoding = "", .sep = NULL,
    .warn = FALSE)

  map_filenames(x, ...)

  \method{map_filenames}{character} (x, out.ext,
    append = "", out.dir = ".", groups = 1L,
    assort = c("lst", "rlst", "ext", "rext", "grp", "rgrp"),
    normalize = TRUE, ...)

  clean_filenames(x, ...)

  \method{clean_filenames}{character} (x,
    overwrite = FALSE, demo = FALSE,
    empty.tmpl = "__EMPTY__\%05i__", ...)
}
\arguments{
  \item{x}{Character vector of input (and potentially
  output) file names.  Names of directories are not
  supported.}

  \item{mapfun}{Mapping function, receives character vector
  with the lines per file as first argument, with the name
  of the file added as attribute with the name given using
  \code{.attr}.}

  \item{...}{Optional additional arguments passed to
  \code{fun} (in the case of \code{map_files} or between
  other methods.}

  \item{.attr}{Character scalar. See description to
  \code{mapfun}.}

  \item{.encoding}{Passed to \code{readLines} as
  \sQuote{encoding} argument.}

  \item{.sep}{\code{NULL} or character scalar. If empty,
  ignored. Otherwise used as output line separator, causing
  output files to always be written unless \code{mapfun}
  returns \code{NULL}. Can be used to just change line
  breaks if \code{mapfun} is \code{identity}.}

  \item{.warn}{Logical scalar passed as \code{warn}
  argument to \code{readLines}.}

  \item{out.ext}{Character vector with one to several
  output file extensions.  Recycled if necessary.}

  \item{append}{Character vector appended after the base
  name of the input file name (separated from ti with an
  underscore) but before the output file extension.
  Recycled if necessary but ignored where equal the empty
  string.}

  \item{out.dir}{Character vector with one to several names
  of output directories. Recycled if necessary.}

  \item{groups}{Integer scalar indicating the number of
  input file names to be assumed in one group. Used in
  conjunction with the next argument unless \code{groups}
  is negative. If so, \code{assort} is ignored and all
  pairs of names found in \code{x} are generated.}

  \item{assort}{Character scalar indicating how to assort
  input file names. \describe{ \item{lst}{All files of the
  first kind first, then all of the second kind, etc.,
  sorted increasingly.} \item{rlst}{All files of the first
  kind first, then all of the second kind, etc., sorted
  decreasingly.} \item{ext}{Assort according to the file
  extensions, assume increasing order.} \item{rext}{Assort
  according to the file extensions, assume decreasing
  order.} \item{grp}{Assume one set of file after each
  other, each sorted increasingly.} \item{rgrp}{Assume one
  set of file after each other, each sorted decreasingly.}
  }}

  \item{normalize}{Logical scalar indicating whether
  \code{normalizePath} from the \pkg{base} package shall be
  applied. Eases the recognition of duplicate file names.}

  \item{overwrite}{Logical scalar. Overwrite already
  existing files, and do not care for duplicate names
  created by cleaning the file names?}

  \item{empty.tmpl}{Character scalar. The template to use
  for file names that become empty after cleaning. Should
  include an integer placeholder to enable incrementing an
  index for creating unique file names. (Empty file names
  should occur rarely anyway.)}

  \item{demo}{Logical scalar. For \code{clean_filenames},
  \code{TRUE} means to not rename files but just return the
  usual result indicating the renaming actions that would
  be attempted? (Note that this does not indicate whether
  the renaming would also by successful.)}
}
\value{
  \code{map_files} returns a logical vector using \code{x}
  as names, with \code{TRUE} indicating a successfully
  modified file, \code{FALSE} a file that yielded no errors
  but needed not to be modified, and \code{NA} a file name
  that caused an error. An attribute \sQuote{errors} is
  provided, containing a character vector with error
  messages (empty strings if no error occurred).

  \code{map_filenames} returns a matrix of mode
  \code{character}. Each row contains a set of one to
  several input file names and its associated set of one to
  several output file names constructed from these input
  file names and the arguments \code{out.ext},
  \code{append} and \code{out.dir}.

  \code{clean_filenames} yields a character vector, its
  names corresponding to the renamed old files, values
  corresponding to the novel names, returned invisibly.
}
\description{
  Read lines from a file, modify the lines using a given
  function, and write the lines back to the input file
  unless the result of applying the function is identical
  to the lines read. Alternatively, map (sets of) input
  file names to (sets of) output file names and check for
  duplicates.
}
\details{
  These function are mainly of use in non-interactive
  scripts.

  If \code{mapfun} returns \code{NULL}, it is ignored by
  \code{map_files}. Otherwise is it an error if
  \code{mapfun} does not return a character vector. If this
  vector is identical to the lines read from the file, it
  is not printed to this file unless \code{.sep} is
  non-empty. Otherwise the file is attempted to be
  overwritten with the result of \code{mapfun}.

  The purpose of \code{map_filenames} is to ease the
  generation of output file names from input file names and
  to assort these input file names. This in turn helps
  converting sets of input file names to sets of output
  file names.
}
\examples{
## map_files

tmpfile <- tempfile()
write(letters, file = tmpfile)
(x <- map_files(tmpfile, identity))
stopifnot(!x)
# now enforce other output line separator
(x <- map_files(tmpfile, identity, .sep = "\n"))
stopifnot(x)
(x <- map_files(tmpfile, toupper))
stopifnot(x)
x <- readLines(tmpfile)
stopifnot(x == LETTERS)
(x <- map_files(tmpfile, as.null))
stopifnot(!x)

## clean_filenames

# Example with temporary files
(x <- tempfile(pattern = "cb& ahi+ si--")) # bad file name
write("test", x)
stopifnot(file.exists(x))
(y <- clean_filenames(x)) # file renamed
stopifnot(!file.exists(x), file.exists(y))
unlink(y) # tidy up

## map_filenames
(x <- map_filenames(letters, out.ext = c("txt", "csv"),
  normalize = FALSE))
stopifnot(is.matrix(x), dim(x) == c(26, 3))
(x <- map_filenames(letters, out.ext = c("txt", "csv"),
  out.dir = LETTERS, normalize = FALSE))
stopifnot(is.matrix(x), dim(x) == c(26, 3))

# Several sets of input files
infiles <- paste0(letters, c(".txt", ".csv"))
(x <- map_filenames(infiles, "tmp", normalize = FALSE,
  groups = 2, assort = "ext"))
stopifnot(is.matrix(x), dim(x) == c(13, 3), grepl("csv", x[, 1]),
  grepl("txt", x[, 2]))
}
\seealso{
  base::readLines base::writeLines base::identity

  \code{clean_filenames} modifies file names by removing
  anything else then word characters, dashes, and dots.
  Also remove trailing and leading dashes and underscores
  (per part of a file name, with dots separating these
  parts) and reduce adjacent dashes and underscores to a
  single one. Note that directory parts within the file
  names, if any, are not affected.

  Other character-functions: \code{\link{sections}},
    }
\keyword{IO}

