


################################################################################
################################################################################
#
# File splitting and name cleaning
#


## NOTE: no S4 methods because conversion is done


#' Helper functions for file input and output
#'
#' Batch-collect information from a series of input files or batch-convert data
#' from input files to data in output files. Alternatively, turn a mixed
#' file/directory list into a list of files or create a regular expression
#' matching certain file extensions, or convert a wildcard pattern to a regular
#' expression, or split files. These functions are not normally directly called
#' by an \pkg{opm} user but by the other IO functions of the package such as
#' \code{\link{collect_template}} or \code{\link{batch_opm}}. One can use their
#' \code{demo} argument directly for testing the results of the applied file
#' name patterns.
#'
#' @inheritParams process_io
#'
#' @param names Character vector containing file names or directories, or
#'   convertible to such.
#'
#' @param object Character vector or factor.
#'
#' @param include If a character scalar, used as regular expression or wildcard
#'   (see the \code{wildcard} argument) for selecting from the input files. If
#'   \code{NULL}, ignored. If a list, used as arguments of \code{file_pattern}
#'   and its result used as regular expression. Note that selection is done
#'   \strong{after} expanding the directory names to file names.
#'
#'   For \code{split_files} a logical scalar. \code{TRUE} means to also include
#'   the separator lines in the output files.
#' @param exclude Like \code{include}, but for excluding matching input files.
#'   Note that exclusion is done \strong{after} applying \code{include}.
#'
#' @param ignore.case Logical scalar. Ignore differences between uppercase and
#'   lowercase when using \code{include} and \code{exclude}? Has no effect for
#'   \code{NULL} values for \code{include} or \code{exclude}, respectively.
#' @param wildcard Logical scalar. Are \code{include}, \code{exclude} or
#'   \code{pattern} wildcards (as used by UNIX shells) that first need to be
#'   concerted to regular expressions? Has no effect if lists are used for
#'   \code{include} or \code{exclude}, respectively. See below for details on
#'   such wildcards (a.k.a. globbing patterns).
#'
#' @param recursive Logical scalar. Traverse directories recursively and also
#'   consider all subdirectories? See \code{list.files} from the \pkg{base}
#'   package for details.
#' @param missing.error Logical scalar. If a file/directory does not exist,
#'   raise an error or only a warning?
#'
#' @param remove.dups Logical scalar. Remove duplicates from \code{names}? Note
#'   that if requested this is done \strong{before} expanding the names of
#'   directories, if any.
#'
#' @param fun Collecting function. Should use the file name as first argument.
#' @param fun.args Optional list of arguments to \code{fun} or \code{io.fun}.
#' @param ... Optional further arguments passed from \code{batch_process} or
#'   \code{batch_collect} to \code{explode_dir}. For \code{split_files},
#'   optional arguments passed to \code{grepl}, which is used for matching the
#'   separator lines. See also \code{invert} listed above.
#' @param proc Integer scalar. The number of processes to spawn. Cannot be set
#'   to more than 1 core if running under Windows. See the \code{cores}
#'   argument of \code{\link{do_aggr}} for details.
#' @param simplify Logical scalar. Should the resulting list be simplified to a
#'   vector or matrix if possible?
#' @param use.names Logical scalar. Should \code{names} be used for naming the
#'   elements of the result?
#'
#' @param out.ext Character scalar. The extension of the output file names
#'   (without the dot).
#' @param outdir Character vector. Directories in which to place the output
#'   files. If empty  or only containing empty strings, the directory of
#'   each input file is used.
#' @param in.ext Character scalar. Passed through \code{file_pattern}, then used
#'   for the replacement of old file extensions with new ones.
#'
#' @param type Character scalar indicating the file types to be matched by
#'   extension. For historical reasons, \kbd{both} means either \acronym{CSV} or
#'   \acronym{YAML} \emph{or} \acronym{JSON}, whereas \kbd{yorj} means either
#'   \acronym{YAML} or \acronym{JSON}. \acronym{CSV} also includes the
#'   \acronym{LIMS} \acronym{CSV} format introduced in 2014, which can be
#'   specifically selected using \kbd{lims} or excluded using \kbd{nolims}.
#'   Alternatively, directly the extension or extensions, or a list of file
#'   names (not \code{NA}).
#' @param compressed Logical scalar. Shall compressed files also be matched?
#'   This affects the returned pattern as well as the pattern used for
#'   extracting file extensions from complete file names (if \code{literally}
#'   is \code{TRUE}).
#'
#'   \code{split_files} passes this argument to \code{\link{file_pattern}}, but
#'   here it only affects the way file names are split in extensions and base
#'   names. Should only be set to \code{FALSE} if input files are not compressed
#'   (and have according file extensions).
#' @param literally Logical scalar. Interpret \code{type} literally? This also
#'   allows for vectors with more than a single element, as well as the
#'   extraction of file extensions from file names.
#' @param demo Logical scalar. In the case of \code{batch_process}, if
#'   \code{TRUE} do not convert files, but print the attempted input file-output
#'   file conversions and invisibly return a matrix with input files in the
#'   first and output files in the second column? For the other functions, the
#'   effect is equivalent.
#'
#'   For \code{split_files}, do not create files, just return the usual list
#'   containing all potentially created files. Note that in contrast to the
#'   \code{demo} arguments of other IO functions, this requires the input files
#'   to be read.
#' @param files Character vector or convertible to such. Names of the files to
#'   be split. In contrast to functions such as \code{\link{read_opm}}, names of
#'   directories are not supported (will not be expanded to lists of files).
#' @param pattern Regular expression or shell globbing pattern for matching the
#'   separator lines if \code{invert} is \code{FALSE} (the default) or matching
#'   the non-separator lines if otherwise.
#'
#'   Conceptually each of the sections into which a file is split comprises a
#'   separator line followed by non-separator lines. That is, separator lines
#'   followed by another separator line are ignored. Non-separator lines not
#'   preceded by a separator line are treated as a section of their own,
#'   however.
#' @param single Logical scalar. If there is only one group per file, i.e. only
#'   one output file would result from the splitting, create this file anyway?
#'   Such cases could be recognised by empty character vectors as values of the
#'   returned list (see below).
#' @param invert Logical scalar. Invert pattern matching, i.e. treat all lines
#'   that \strong{not} match \code{pattern} as separators?
#' @param format Character scalar determining the output file name format. It is
#'   passed to \code{sprintf} and expects three placeholders: \itemize{
#'   \item the base name of the file;
#'   \item the index of the section;
#'   \item the file extension.
#'   }
#'   Getting \code{format} wrong might result in non-unique file names and thus
#'   probably in overwritten files; accordingly, it should be used with care.
#' @return
#' \code{explode_dir} returns a character vector (which would be empty if all
#' existing files, if any, had been unselected).
#'
#' \code{batch_collect} returns a list, potentially simplified to a vector,
#' depending on the output of \code{fun} and the value of \code{simplify}. See
#' also \code{demo}.
#'
#' In normal mode, \code{batch_process} creates an invisibly returned character
#' matrix in which each row corresponds to a named character vector with the
#' keys \code{infile}, \code{outfile}, \code{before} and \code{after}. The
#' latter two describe the result of the action(s) before and after attempting
#' to convert \code{infile} to \code{outfile}. The \code{after} entry is the
#' empty string if no conversion was tried (see \code{overwrite}), \code{ok} if
#' conversion was successful and a message describing the problems otherwise.
#' For the results of the \code{demo} mode see above.
#'
#' \code{file_pattern} yields a character scalar, holding a regular expression.
#' \code{glob_to_regex} yields a vector of regular expressions.
#'
#' \code{split_files} creates a list of character vectors, each vector
#' containing the names of the newly generated files. The names of the list are
#' the input file names. The list is returned invisibly.
#'
#' @details
#' Other functions that call \code{explode_dir} have a \code{demo} argument
#' which, if set to \code{TRUE}, caused the respective function to do no real
#' work but print the names of the files that it would process in normal running
#' mode.
#'
#' \code{glob_to_regex} changes a shell globbing wildcard into a regular
#' expression. This is just a slightly extended version of \code{glob2rx} from
#' the \pkg{utils} package, but more conversion steps might need to be added
#' here in the future. Particularly \code{\link{explode_dir}} and the IO
#' functions calling that function internally use \code{glob_to_regex}. Some
#' hints when using globbing patterns are given in the following.
#'
#' The here used globbing search patterns contain only two special characters,
#' \sQuote{?} and \sQuote{*}, and are thus more easy to master than regular
#' expressions. \sQuote{?} matches a single arbitrary character, whereas
#' \sQuote{*} matches zero to an arbitrary number of arbitrary characters.
#' Some examples:
#'   \describe{
#'     \item{a?c}{Matches \verb{abc}, \verb{axc}, \verb{a c} etc. but not
#'     \verb{abbc}, \verb{abbbc}, \verb{ac} etc.}
#'     \item{a*c}{Matches \verb{abc}, \verb{abbc}, \verb{ac} etc. but not
#'     \verb{abd} etc.}
#'     \item{ab*}{Matches \verb{abc}, \verb{abcdefg}, \verb{abXYZ} etc. but not
#'     \verb{acdefg} etc.}
#'     \item{?bc}{Matches \verb{abc}, \verb{Xbc}, \verb{ bc} etc. but not
#'     \verb{aabc}, \verb{abbc}, \verb{bc} etc.}
#' }
#' Despite their simplicity, globbing patterns are often sufficient for
#' selecting file names.
#'
#' \code{split_files} subdivides each file into sections which are written
#' individually to newly generated files. Sections are determined with patterns
#' that match the start of a section. This function might be useful for
#' splitting OmniLog\eqn{\textsuperscript{\textregistered}}{(R)} multiple-plate
#' \acronym{CSV} files before inputting them with \code{\link{read_opm}}, even
#' though that function could also input such files directly. It is used in one
#' of the running modes of by \code{\link{batch_opm}} for splitting files. See
#' also the \sQuote{Examples}. The newly generated files are numbered
#' accordingly; they are \emph{not} named after any \code{\link{csv_data}} entry
#' because there is no guarantee that it is present.
#' @export
#' @seealso base::list.files base::Sys.glob utils::glob2rx base::regex
#'   base::split base::strsplit base::file.rename
#' @family io-functions
#' @keywords IO character utilities
#' @examples
#'
#' # explode_dir()
#' # Example with temporary directory
#' td <- tempdir()
#' tf <- tempfile()
#' (x <- explode_dir(td))
#' write(letters, tf)
#' (y <- explode_dir(td))
#' stopifnot(length(y) > length(x))
#' unlink(tf)
#' (y <- explode_dir(td))
#' stopifnot(length(y) == length(x))
#'
#' # Example with R installation directory
#' (x <- explode_dir(R.home(), include = "*/doc/html/*"))
#' (y <- explode_dir(R.home(), include = "*/doc/html/*", exclude = "*.html"))
#' stopifnot(length(x) == 0L || length(x) > length(y))
#'
#' # batch_collect()
#' # Read the first line from each of the OPM test data set files
#' f <- opm_files("testdata")
#' if (length(f) > 0) { # if the files are found
#'   x <- batch_collect(f, fun = readLines, fun.args = list(n = 1L))
#'   # yields a list with the input files as names and the result from each
#'   # file as values (exactly one line)
#'   stopifnot(is.list(x), identical(names(x), f))
#'   stopifnot(sapply(x, is.character), sapply(x, length) == 1L)
#' } else {
#'   warning("test files not found")
#' }
#' # For serious tasks, consider to first try the function in 'demo' mode.
#'
#' # batch_process()
#' # Read the first line from each of the OPM test data set files and store it
#' # in temporary files
#' pf <- function(infile, outfile) write(readLines(infile, n = 1), outfile)
#' infiles <- opm_files("testdata")
#' if (length(infiles) > 0) { # if the files are found
#'   x <- batch_process(infiles, out.ext = "tmp", io.fun = pf,
#'     outdir = tempdir())
#'   stopifnot(is.matrix(x), identical(x[, 1], infiles))
#'   stopifnot(file.exists(x[, 2]))
#'   unlink(x[, 2])
#' } else {
#'   warning("test files not found")
#' }
#' # For serious tasks, consider to first try the function in 'demo' mode.
#'
#' # file_pattern()
#' (x <- file_pattern())
#' (y <- file_pattern(type = "csv", compressed = FALSE))
#' stopifnot(nchar(x) > nchar(y))
#' # constructing pattern from existing files
#' (files <- list.files(pattern = "[.]"))
#' (x <- file_pattern(I(files))) # I() causes 'literally' to be TRUE
#' stopifnot(grepl(x, files, ignore.case = TRUE))
#'
#' # glob_to_regex()
#' x <- "*what glob2rx() can't handle because a '+' is included*"
#' (y <- glob_to_regex(x))
#' (z <- glob2rx(x))
#' stopifnot(!identical(y, z))
#' # Factor method
#' (z <- glob_to_regex(as.factor(x)))
#' stopifnot(identical(as.factor(y), z))
#'
#' ## split_files()
#'
#' # Splitting an old-style CSV file containing several plates
#' (x <- opm_files("multiple"))
#' if (length(x) > 0) {
#'   outdir <- tempdir()
#'   # For old-style CSV, use either "^Data File" as pattern or "Data File*"
#'   # with 'wildcard' set to TRUE:
#'   (result <- split_files(x, pattern = "^Data File", outdir = outdir))
#'   stopifnot(is.list(result), length(result) == length(x))
#'   stopifnot(sapply(result, length) == 3)
#'   result <- unlist(result)
#'   stopifnot(file.exists(result))
#'   unlink(result) # tidy up
#' } else {
#'   warning("opm example files not found")
#' }
#' ## One could split new-style CSV as follows (if x is a vector of file names):
#' # split_files(x, pattern = '^"Data File",')
#' ## note the correct setting of the quotes
#' ## A pattern that covers both old and new-style CSV is:
#' # split_files(x, pattern = '^("Data File",|Data File)')
#' ## This is used by batch_opm() in 'split' mode any by the 'run_opm.R' script
#'
explode_dir <- function(names,
    include = NULL, exclude = NULL, ignore.case = TRUE, wildcard = TRUE,
    recursive = TRUE, missing.error = TRUE, remove.dups = TRUE) {
  extended_file_pattern <- function(arg, wildcard) {
    if (is.list(arg))
      return(do.call(file_pattern, arg))
    result <- as.character(arg)
    if (wildcard)
      result <- glob_to_regex(result)
    result
  }
  explode_names <- function(names, recursive) {
    is.dir <- file.info(names)$isdir
    if (any(no.info <- is.na(is.dir))) {
      msg <- sprintf("File or directory not found: '%s'",
        paste0(names[no.info], collapse = " "))
      if (missing.error)
        stop(msg)
      else
        warning(msg)
    }
    is.dir <- is.dir[!no.info]
    names <- as.list(names[!no.info]) # use of a list ensures input order
    names[is.dir] <- lapply(names[is.dir], FUN = list.files, full.names = TRUE,
      recursive = recursive)
    unlist(names)
  }
  select_files <- function(data, pattern, invert) {
    if (is.null(pattern))
      return(data)
    pattern <- extended_file_pattern(pattern, wildcard)
    grep(pattern, data, ignore.case = ignore.case, value = TRUE,
      invert = invert)
  }
  names <- as.character(names)
  if (remove.dups)
    names <- unique(names)
  result <- explode_names(names, recursive = recursive)
  result <- select_files(result, include, invert = FALSE)
  select_files(result, exclude, invert = TRUE)
}

#' @rdname explode_dir
#' @export
#'
batch_collect <- function(names, fun, fun.args = list(), proc = 1L, ...,
    use.names = TRUE, simplify = FALSE, demo = FALSE) {
  names <- explode_dir(names, ...)
  if (demo) {
    message(paste0(names, collapse = "\n"))
    return(invisible(names))
  }
  fun.args <- as.list(fun.args)
  mcmapply(FUN = fun, names, MoreArgs = as.list(fun.args), SIMPLIFY = simplify,
    USE.NAMES = use.names, mc.cores = proc)
}

#' @rdname explode_dir
#' @export
#'
batch_process <- function(names, out.ext, io.fun, fun.args = list(), proc = 1L,
    outdir = NULL, overwrite = c("yes", "older", "no"), in.ext = "any",
    compressed = TRUE, literally = inherits(in.ext, "AsIs"), ...,
    verbose = TRUE, demo = FALSE) {
  create_outfile_names <- function(infiles, outdir, out.ext) {
    if (length(outdir) == 0L || all(!nzchar(outdir)))
      outdir <- dirname(infiles)
    result <- sub(in.ext, "", basename(infiles), TRUE, TRUE)
    result <- paste(result, sub("^\\.+", "", out.ext), sep = ".")
    file.path(outdir, result)
  }
  LL(demo, verbose, compressed)
  in.ext <- file_pattern(in.ext, compressed, literally)
  overwrite <- match.arg(overwrite)
  infiles <- explode_dir(names, ...)
  outfiles <- create_outfile_names(infiles, outdir, out.ext)
  if (demo) {
    message(paste(infiles, outfiles, sep = "\n  => ", collapse = "\n"))
    return(invisible(cbind(infiles, outfiles)))
  }
  fun.args <- as.list(fun.args)
  data <- mapply(c, infiles, outfiles, SIMPLIFY = FALSE, USE.NAMES = FALSE)
  result <- mclapply(X = data, FUN = process_io, mc.cores = proc,
    io.fun = io.fun, fun.args = fun.args, overwrite = overwrite,
    verbose = verbose)
  invisible(do.call(rbind, result))
}

#' @rdname explode_dir
#' @export
#'
file_pattern <- function(
    type = c("both", "csv", "yaml", "json", "yorj", "lims", "nolims", "any",
      "empty"),
    compressed = TRUE, literally = inherits(type, "AsIs")) {
  make_pat <- function(x, compressed, enclose = "\\.%s$") {
    if (compressed)
      x <- sprintf("%s(\\.(bz2|gz|lzma|xz))?", x)
    sprintf(enclose, x)
  }
  LL(literally, compressed)
  result <- if (literally) {
    x <- make_pat("([^.]+)", compressed, "^.*?\\.%s$")
    x <- sub(x, "\\1", type, FALSE, TRUE)
    if (all(same <- x == basename(type))) { # assuming extensions
      type <- x
      bad <- "^\\w+$"
    } else { # assuming file names
      type <- x[!same]
      bad <- "^\\w+(\\.\\w+)?$"
    }
    if (any(bad <- !grepl(bad, type <- unique.default(type), FALSE, TRUE)))
      stop("'type' must contain word characters (only): ", type[bad][1L])
    case(length(type), stop("'type' must be non-empty"), type,
      sprintf("(%s)", paste0(type, collapse = "|")))
  } else
    case(match.arg(type), both = "(csv|exl|ya?ml|json)", csv = "(csv|exl)",
      yaml = "ya?ml", json = "json", yorj = "(ya?ml|json)", lims = "exl",
      nolims = "csv", any = "[^.]+", empty = "")
  make_pat(result, compressed)
}

#' @rdname explode_dir
#' @export
#'
split_files <- function(files, pattern, outdir = "", demo = FALSE,
    single = TRUE, wildcard = FALSE, invert = FALSE, include = TRUE,
    format = opm_opt("file.split.tmpl"), compressed = TRUE, ...) {

  create_outnames <- function(files, compressed, outdir) {
    file.pat <- file_pattern("any", compressed = compressed, literally = FALSE)
    out.base <- sub(file.pat, "", files, TRUE, TRUE)
    out.ext <- substr(files, nchar(out.base) + 2L, nchar(files))
    if (compressed)
      out.ext <- sub("\\.[^.]+$", "", out.ext, FALSE, TRUE)
    if (length(outdir) && all(nzchar(outdir)))
      out.base <- file.path(outdir, basename(out.base))
    list(base = out.base, ext = out.ext)
  }

  LL(pattern, outdir, demo, single, wildcard, invert, include,
    format, compressed)
  files <- unique(as.character(files))
  out <- create_outnames(files, compressed = compressed, outdir = outdir)
  if (wildcard)
    pattern <- glob_to_regex(pattern)

  invisible(mapply(function(infile, out.base, out.ext) {
    con <- file(description = infile, encoding = opm_opt("file.encoding"))
    on.exit(close(con))
    data <- readLines(con = con, warn = FALSE)
    data <- sections(x = data, pattern = pattern, invert = invert,
      include = include, ...)
    if ((len <- length(data)) == 0L || (!single && len == 1L))
      return(character())
    outnames <- sprintf(format, out.base, seq_along(data), out.ext)
    if (demo)
      message(listing(structure(outnames, names = seq_along(outnames)),
        header = infile))
    else
      mapply(write, data, outnames, USE.NAMES = FALSE, SIMPLIFY = FALSE)
    outnames
  }, files, out$base, out$ext, SIMPLIFY = FALSE))
}

#' @rdname explode_dir
#' @export
#'
glob_to_regex <- function(object) UseMethod("glob_to_regex")

#' @rdname explode_dir
#' @method glob_to_regex character
#' @export
#'
glob_to_regex.character <- function(object) {
  # TODO: one should perhaps also check for '|'
  x <- glob2rx(gsub("([+^$])", "\\\\\\1", object, FALSE, TRUE))
  attributes(x) <- attributes(object)
  x
}

#' @rdname explode_dir
#' @method glob_to_regex factor
#' @export
#'
glob_to_regex.factor <- function(object) {
  levels(object) <- glob_to_regex(levels(object))
  object
}


################################################################################
################################################################################
#
# Input of multiple OPM files
#


## No S4 methods because conversion is done


#' Read multiple \acronym{PM} files at once or read single \acronym{PM} file
#'
#' Read OmniLog\eqn{\textsuperscript{\textregistered}}{(R)} or \pkg{opm} data
#' file(s) in one of four possible formats: either new- or old-style
#' OmniLog\eqn{\textsuperscript{\textregistered}}{(R)} \acronym{CSV},
#' OmniLog\eqn{\textsuperscript{\textregistered}}{(R)} \acronym{LIMS} format or
#' \pkg{opm} \acronym{YAML} (including \acronym{JSON}) format.
#' MicroStation\eqn{\textsuperscript{\texttrademark}}{(TM)} \acronym{CSV} is
#' also understood, as well as files compressed  using \command{gzip},
#' \command{bzip2}, \command{lzma} or \command{xz}. (Files can be specifically
#' excluded using \code{include} and/or \code{exclude}).
#'
#' @param names Character vector with names of files in one of the formats
#'   accepted by \code{\link{read_single_opm}}, or names of directories
#'   containing such files, or both; or convertible to such a vector. See the
#'   \code{include} argument of \code{\link{read_opm}} and
#'   \code{\link{explode_dir}} for how to select subsets from the input files or
#'   directories.
#'
#' @param filename Character scalar, or convertible to such, with the obvious
#'   meaning.
#'
#' @param convert Character scalar with one of the following values:
#'   \describe{
#'   \item{no}{Always return a list of \code{\link{OPM}} objects. (This list is
#'   rather a \code{\link{MOPMX}} object than a plain list.)}
#'   \item{yes}{Convert to \code{NULL}, \code{\link{OPM}} object, or
#'   \code{\link{OPMS}} object, depending on the number of files read (0, 1, or
#'   more).}
#'   \item{try}{Behave like \sQuote{yes} but do not result in an error message
#'   if conversion to \code{\link{OPMS}} is impossible; return a list
#'   (\code{\link{MOPMX}} object) in that case.}
#'   \item{sep}{Return a nested list, each partial list (\code{\link{MOPMX}}
#'   object) containing \code{\link{OPM}} objects of the same plate type.}
#'   \item{grp}{Also split into such contained lists but convert them to
#'   \code{\link{OPMS}} objects if more than one plate is encountered. An error
#'   is raised if this is impossible (in contrast to \sQuote{try}). Return a
#'   list (\code{\link{MOPMX}} object).}
#'   }
#' @param gen.iii Logical or character scalar. If \code{TRUE}, invoke
#'   \code{\link{gen_iii}} on each plate. This is automatically done with
#'   \acronym{CSV} input if the plate type is given as \acronym{OTH} (which is
#'   usually the case for plates run in ID mode). If a character scalar, it is
#'   used as the \code{to} argument of \code{\link{gen_iii}} to set other plate
#'   types unless it is empty.
#'
#' @param include Pattern for selecting from the input files. The default value
#'   results in the output of \code{\link{file_pattern}}, which should be
#'   sufficient in most cases. See \code{\link{explode_dir}} for details on
#'   other possibilities.
#' @param ... Optional further arguments passed to \code{\link{explode_dir}}.
#'
#' @param force Logical scalar passed to \code{\link{gen_iii}} (if that function
#'   is called, see the \code{gen.iii} argument).
#' @param demo Logical scalar. Do not read files, but print a vector with the
#'   names of the files that would be (attempted to) read, and return them
#'   invisibly?
#'
#' @return
#'   \code{read_opm} returns an \code{\link{OPM}} object (maybe derived classes
#'   such as \code{\link{OPMA}} in case of \acronym{YAML} input), or list
#'   (\code{\link{MOPMX}} object) of such objects, or a single
#'   \code{\link{OPMS}} object. If \code{demo} is \code{TRUE}, a character
#'   vector instead.
#'
#'   \code{read_single_opm} also returns an\code{\link{OPM}} object. In the case
#'   of \acronym{YAML} and multiple-plate \acronym{CSV} input, the result might
#'   instead be a list of such objects, but \strong{not} an \code{\link{OPMS}}
#'   object. Use \code{\link{opm_opt}(warn.mult = TRUE)} if you want to be
#'   warned in such cases.
#'
#' @details The expected \acronym{CSV} format is what is output by an
#'   OmniLog\eqn{\textsuperscript{\textregistered}}{(R)} instrument, one plate
#'   per file. The data import section of the \pkg{opm} tutorial provides a
#'   detailed description of how to generate such files.
#'
#'   The output of a MicroStation\eqn{\textsuperscript{\texttrademark}}{(TM)}
#'   instrument, with one to many plates per file, can also be read. The same
#'   holds for the OmniLog\eqn{\textsuperscript{\textregistered}}{(R)}
#'   \acronym{LIMS} format, which was introduced in 2014 and uses the file
#'   extension \code{EXL}.
#'
#'   Other formats, or OmniLog\eqn{\textsuperscript{\textregistered}}{(R)} files
#'   re-saved with distinct \acronym{CSV} settings, are not understood. For this
#'   reason, if any editing of the files was necessary at all, it is advisable
#'   to do this in an editor for plain text, not in a spreadsheet program.
#'
#'   Plates run in ID mode are automatically detected as such (their plate type
#'   is changed from \acronym{OTH} to the internally used spelling of
#'   \sQuote{Generation III}). A generation-III or other plate type can also be
#'   forced later on by using \code{\link{gen_iii}}.
#'
#'   It is possible, but not recommended, to read \acronym{CSV} files that
#'   contain more than one plate. The data import section of the \pkg{opm}
#'   tutorial provides a detailed description of how to batch-generate many
#'   files with one plate per file. In contrast, input \acronym{YAML} files can
#'   always contain data from more than one plate. The format (which includes
#'   \acronym{JSON}) is described in detail under \code{\link{batch_opm}}.
#'
#'   For splitting lists of \code{\link{OPM}} objects according to the plate
#'   type, see \code{\link{plate_type}}, and consider the plate-type selection
#'   options of \code{\link{opms}}.
#'
#'   The order in which it is tried to read distinct formats of \acronym{CSV}
#'   files can be modified using the \sQuote{input.try.order} key of
#'   \code{\link{opm_opt}}. The value is an integer vector whose elements have
#'   the following meaning:
#'   \enumerate{
#'     \item New-style OmniLog\eqn{\textsuperscript{\textregistered}}{(R)}
#'       \acronym{CSV}.
#'     \item Old-style OmniLog\eqn{\textsuperscript{\textregistered}}{(R)}
#'       \acronym{CSV}.
#'     \item MicroStation\eqn{\textsuperscript{\texttrademark}}{(TM)}
#'       \acronym{CSV}.
#'   }
#'   For instance, \code{opm_opt(input.try.order = 2:1} would change the order
#'   in which OmniLog\eqn{\textsuperscript{\textregistered}}{(R)} formats are
#'   tried and deselect MicroStation\eqn{\textsuperscript{\texttrademark}}{(TM)}
#'   files entirely. Negative indexes can be used, but non-negative values not
#'   within the range listed above will result in an error. If it known in
#'   advance which formats are (not) to be expected, subset creation or just
#'   changing the order can be used to accelerate data input.
#' @export
#' @family io-functions
#' @references \url{http://www.yaml.org/}
#' @references \url{http://www.json.org/}
#' @references \url{http://www.biolog.com/}
#' @seealso utils::read.csv yaml::yaml.load_file
#' @keywords IO
#' @examples
#' test.files <- opm_files("omnilog")
#' if (length(test.files) > 0) { # if the folder is found
#'   # check first what you *would* get
#'   x <- read_opm(names = test.files, demo = TRUE)
#'   stopifnot(identical(test.files, x))
#'   # these two have the same plate type
#'   x <- read_opm(names = test.files[1:2], convert = "try")
#'   class(x)
#'   dim(x)
#'   summary(x)
#'   stopifnot(is(x, "OPMS"), identical(dim(x), c(2L, 384L, 96L)))
#' } else {
#'   warning("test files not found")
#' }
#' # This can be repeated for the other input test files. Instead of a several
#' # file names one can also provide a single one, one to several directory
#' # names, or mixture of file and directory names.
#'
#' \dontrun{
#'
#' # Reading all files from the current working directory is also easy:
#' x <- read_opm(getwd())
#' # or
#' x <- read_opm(".")
#' # or just
#' x <- read_opm()
#' }
#'
#' # read_single_opm()
#' test.files <- opm_files("omnilog")
#' if (length(test.files) > 0) { # if the folder is found
#'   (x <- read_single_opm(test.files[1])) # => 'OPM' object
#'   class(x)
#'   dim(x)
#'   stopifnot(is(x, "OPM"), identical(dim(x), c(384L, 96L)))
#' } else {
#'   warning("test-file folder not found")
#' }
#' test.files <- opm_files("multiple")
#' if (length(test.files) > 0) { # if the folder is found
#'   (x <- read_single_opm(test.files[1])) # => list
#'   class(x)
#'   stopifnot(is.list(x), length(x) > 1, sapply(x, is, "OPM"))
#' } else {
#'   warning("test-file folder not found")
#' }
#'
read_opm <- function(names = getwd(),
    convert = c("grp", "try", "no", "yes", "sep"), gen.iii = opm_opt("gen.iii"),
    include = list(), ..., force = FALSE, demo = FALSE) {
  do_split <- function(x) split(x, vapply(x, plate_type, ""))
  do_opms <- function(x) case(length(x), , x[[1L]], new("OPMS", plates = x))
  convert <- match.arg(convert)
  LL(gen.iii, demo)
  names <- explode_dir(names = names, include = include, ...)
  if (demo) {
    message(paste0(names, collapse = "\n"))
    return(invisible(names))
  }
  # The c() call is necessary to flatten lists from YAML/JSON input.
  result <- c(lapply(names, read_single_opm), recursive = TRUE)
  switch(mode(gen.iii),
    logical = if (gen.iii)
      result <- lapply(result, gen_iii),
    character = if (nzchar(gen.iii))
      result <- lapply(result, gen_iii, to = gen.iii, force = force),
    stop("'gen.iii' must either be logical or character scalar")
  )
  case(length(result),
    case(convert,
      no =,
      grp = new("MOPMX"),
      sep = list(),
      yes =,
      try = NULL
    ),
    case(convert,
      no = new("MOPMX", result),
      grp = new("MOPMX", structure(result, names = plate_type(result[[1L]]))),
      sep = structure(list(new("MOPMX", result)),
        names = plate_type(result[[1L]])),
      yes =,
      try = result[[1L]]
    ),
    case(convert,
      no = new("MOPMX", result),
      yes = new("OPMS", plates = result),
      grp = new("MOPMX", lapply(do_split(result), do_opms)),
      sep = lapply(do_split(result), new, Class = "MOPMX"),
      try = tryCatch(new("OPMS", plates = result), error = function(e) {
        warning("the data from distinct files could not be converted to a ",
          "single OPMS object and will be returned as a list")
        new("MOPMX", result)
      })
    )
  )
}


FILE_NOT_CSV <- file_pattern(type = "yorj", compressed = TRUE)

FILE_LIMS <- file_pattern(type = "lims", compressed = TRUE)


#' @rdname read_opm
#' @export
#'
read_single_opm <- function(filename) {
  if (!file.exists(filename <- as.character(L(filename))))
    stop(sprintf("file '%s' does not exist", filename))
  routines <- list(`New CSV` = read_new_opm, `Old CSV` = read_old_opm,
    `MicroStation CSV` = read_microstation_opm)
  routines <- if (grepl(FILE_NOT_CSV, filename, TRUE, TRUE))
      c(YAML = read_opm_yaml, routines, `LIMS CSV` = read_lims_opm)
    else if (grepl(FILE_LIMS, filename, TRUE, TRUE))
      c(`LIMS CSV` = read_lims_opm, routines, YAML = read_opm_yaml)
    else
      c(routines[get("input.try.order", OPM_OPTIONS)],
        `LIMS CSV` = read_lims_opm, YAML = read_opm_yaml)
  errs <- character(length(routines))
  for (i in seq_along(routines)) {
    result <- tryCatch(routines[[i]](filename), error = conditionMessage)
    if (!is.character(result))
      return(result)
    errs[i] <- result
  }
  names(errs) <- paste(names(routines), "error")
  stop(listing(c(errs, Filename = filename), header = "Unknown file format:"))
}


################################################################################
################################################################################
#
# Metadata IO
#


#' Input metadata
#'
#' Either collect a metadata template from
#' OmniLog\eqn{\textsuperscript{\textregistered}}{(R)} \acronym{CSV} comments
#' assisting in later on adding metadata using \code{\link{include_metadata}},
#' or create a data frame containing potential \code{\link{OPM}} or
#' \code{\link{OPMS}} object metadata.
#'
#' @param object Character vector or \code{\link{OPM}}, \code{\link{OPMS}}
#'   or \code{\link{MOPMX}} object.
#'
#'   If a character vector is provided to \code{\link{collect_template}}, it
#'   acts like the \code{names} argument of \code{\link{read_opm}}. That is, if
#'   it is a directory name, this is automatically scanned for all \acronym{CSV}
#'   and \acronym{YAML} files it contains (unless restrictions with patterns are
#'   made). One can also provide file names, or a mixture of file and directory
#'   names. Regarding the supported input file formats, see
#'   \code{\link{read_single_opm}}. The \code{\link{OPM}}, \code{\link{OPMS}}
#'   and \code{\link{MOPMX}} methods collect a data frame from their input
#'   object.
#'
#'   \code{to_metadata} needs the name of an input file (unnamed character
#'   scalar), or any object convertible to a data frame. Might also be
#'   \code{\link{WMD}} or \code{\link{OPMS}} object. If a named character vector
#'   with more than a single element, it is used as the first row of the
#'   resulting data frame. This behaviour is mainly intended for using this
#'   function after a call to the \code{\link{OPM}} method of \code{csv_data}.
#' @param outfile Character scalar. Ignored if \code{NULL}, empty or empty
#'   string. Otherwise, interpreted as the name of a \acronym{CSV} output file.
#'   If metadata have already been collected in an older file with the same
#'   name, old metadata will be kept, identifiers for novel files will be
#'   included, their so far empty entries set to \code{NA}. Users who wish to
#'   keep the old version can use two distinct names for novel and old files;
#'   see \code{previous}.
#' @param sep Character scalar used for output (\acronym{CSV} field separator
#'   for \code{outfile}). For the input file of \code{to_metadata} (which
#'   ignores the argument unless \code{object} is interpreted as input file)
#'   a non-empty character vector. Each element is used as \acronym{CSV} field
#'   separator in turn, and the first one yielding more than as single column
#'   succeeds.
#' @param previous Ignored if empty. Otherwise passed to
#'   \code{\link{to_metadata}}. If it is a file name different from
#'   \code{outfile}, it is an error if the file does not exist.
#' @param md.args List of other arguments passed to the \sQuote{to_metadata}
#'   methods.
#' @param add.cols Optional character vector with the names of columns to be
#'   added to the result, or \code{NULL}. If not empty, names of columns to be
#'   added, initially containing \code{NA}.
#' @param selection Elements to be extracted from the \acronym{CSV} comments
#'   contained in each file. Character vector passed to \code{\link{csv_data}}.
#' @param normalize Logical scalar also passed to \code{\link{csv_data}}. The
#'   same value must be chosen for subsequent calls of
#'   \code{\link{collect_template}} and \code{include_metadata}.
#' @param instrument Logical scalar or scalar convertible to integer, or empty.
#'   Ignored if empty. If logical and \code{TRUE},
#'   \code{\link{opm_opt}("machine.id")} is inserted as additional column.
#'   Otherwise, \code{instrument} is used directly.
#' @param include File inclusion pattern (or generator for a pattern). Passed to
#'   \code{\link{batch_collect}}.
#' @param ... Other arguments passed to \code{\link{batch_collect}}, or
#'   to \code{read.delim} or \code{as.data.frame}.
#' @param demo Logical scalar. Run in \sQuote{demo} mode? Also passed to
#'   \code{\link{batch_collect}}. If \code{TRUE}, file input and output would be
#'   omitted and only the respective file names shown.
#' @param stringsAsFactors Logical scalar passed to \code{as.data.frame}.
#' @param optional Logical scalar passed to \code{as.data.frame} or used after
#'   negation as \sQuote{check.names} argument of \code{read.delim}.
#' @param strip.white Logical scalar. For the file-name method, passed to
#'   \code{read.delim} (and set to \code{TRUE} if it is \code{NA}). It is often
#'   advisable to set this to \code{FALSE} if \acronym{CSV} input is done for a
#'   later call to \code{collect_template}. For a character vector not
#'   interpreted as file name, set to \code{FALSE} if \code{NA}.
#' @export
#' @return
#'   \code{to_metadata} yields a data frame. The output of
#'   \code{collect_template}, in the case of the character method, is a data
#'   frame, returned invisibly if \code{outfile} is given; if \code{demo} is
#'   \code{TRUE}, a character vector of file names instead, returned invisibly.
#'   The \code{\link{OPM}} method returns a data frame with one row and the
#'   number of columns equal to the sum of the lengths of \code{selection} and
#'   \code{add.cols}. The \code{\link{OPM}} method returns such a data frame
#'   with one row per contained plate.
#' @details The \code{collect_template} character method batch-collects
#'   templates for meta-information from files and optionally adds these data as
#'   novel rows to previously collected data. It writes the collected template
#'   to a file for use with an external editor, and/or creates a data frame for
#'   editing the data directly in \R with the \code{edit} function.
#'
#'   The \code{to_metadata} character method reads metadata from an input file
#'   and is only a thin wrapper for \code{read.delim} but contains some useful
#'   adaptations (such as \emph{not} converting strings to factors, and
#'   \emph{not} modifying column names). The default method reads metadata from
#'   an object convertible to a data frame and is only a thin wrapper of
#'   \code{as.data.frame} but contains the same useful adaptations as the
#'   file-name method.
#'
#'   The \code{\link{WMD}} and \code{\link{OPMS}} methods create a data frame
#'   from the contained metadata, where necessary converting nested metadata
#'   entries to data-frame columns of mode \sQuote{list}. The number of rows
#'   of the resulting data frame corresponds to the length of \code{object}, the
#'   number of columns to the size of the set created from all valid names at
#'   the top level of the metadata entries.
#' @seealso base::default.stringsAsFactors base::as.data.frame
#' @seealso utils::edit utils::read.delim
#' @family io-functions
#' @references \url{http://www.biolog.com/}
#' @keywords IO attribute manip
#' @examples
#'
#' ## collect_template()
#'
#' # Character method
#' test.files <- opm_files("omnilog")
#' if (length(test.files) > 0) { # if the files are found
#'
#'   # Without writing to a file
#'   (x <- collect_template(test.files))
#'   stopifnot(is.data.frame(x), identical(x[, "File"], test.files))
#'   # now proceed with e.g.
#'   # x <- edit(x)
#'
#'   # Write to file
#'   outfile <- tempfile()
#'   stopifnot(!file.exists(outfile))
#'   # This results in a CSV outfile which could be used as a starting point
#'   # for including the metadata of interest together with the plate
#'   # identifiers in a single file. include_metadata() can then be used to
#'   # integrate the metadata in OPMX objects.
#'   x <- collect_template(test.files, outfile = outfile)
#'   stopifnot(file.exists(outfile))
#'   unlink(outfile)
#' } else {
#'   warning("test files not found")
#' }
#'
#' # OPM method
#' (x <- collect_template(vaas_1)) # => data frame, one row per plate
#' stopifnot(dim(x) == c(1, 3))
#' (x <- collect_template(vaas_1, instrument = TRUE))
#' stopifnot(dim(x) == c(1, 4))
#' (x <- collect_template(vaas_1, add.cols = c("A", "B")))
#' stopifnot(dim(x) == c(1, 5)) # => data frame with more columns
#' # see include_metadata() for how to use this to add metadata information
#'
#' # OPMS method
#' (x <- collect_template(vaas_4)) # => data frame, one row per plate
#' stopifnot(identical(dim(x), c(4L, 3L)))
#' (x <- collect_template(vaas_4, add.cols = c("A", "B")))
#' stopifnot(identical(dim(x), c(4L, 5L))) # => data frame with more columns
#' # again see include_metadata() for how to use this to add metadata
#' # information
#'
#' ## to_metadata()
#'
#' # Character method
#' (x <- to_metadata(list(a = 7:8, `b c` = letters[1:2])))
#' tmpfile <- tempfile()
#' write.table(x, tmpfile, row.names = FALSE, sep = "\t")
#' (x1 <- read.delim(tmpfile)) # comparison with base R function
#' (x2 <- to_metadata(tmpfile))
#' stopifnot(identical(names(x2), names(x)), !identical(names(x1), names(x)))
#'
#' # Default method
#' x <- list(a = 7:8, `b c` = letters[1:2])
#' (x1 <- as.data.frame(x))
#' (x2 <- to_metadata(x))
#' stopifnot(!identical(names(x), names(x1)), identical(names(x), names(x2)))
#'
#' # WMD method
#' (x <- to_metadata(vaas_1)) # one row per OPM object
#' stopifnot(is.data.frame(x), nrow(x) == length(vaas_1), ncol(x) > 0)
#'
#' # OPMS method
#' (x <- to_metadata(vaas_4)) # one row per OPM object
#' stopifnot(is.data.frame(x), nrow(x) == length(vaas_4), ncol(x) > 0)
#' copy <- vaas_4
#' metadata(copy) <- x
#' stopifnot(identical(copy, vaas_4))
#' # ... this works only in the special case of non-nested metadata that
#' # have the same set of entries in all OPMS elements
#'
setGeneric("collect_template",
  function(object, ...) standardGeneric("collect_template"))

setMethod("collect_template", "character", function(object, outfile = NULL,
    sep = "\t", previous = outfile, md.args = list(),
    selection = opm_opt("csv.selection"), add.cols = NULL, normalize = -1L,
    instrument = NULL, include = list(), ..., demo = FALSE) {
  do_collect <- function(infile)
    if (is.list(x <- read_single_opm(infile))) # possible in case of YAML input
      do.call(rbind, lapply(X = x, FUN = collect_template,
        selection = selection, normalize = normalize, add.cols = add.cols,
        instrument = instrument, outfile = NULL, previous = NULL, sep = sep,
        md.args = md.args))
    else
      collect_template(object = x, selection = selection, normalize = normalize,
        add.cols = add.cols, instrument = instrument, outfile = NULL,
        previous = NULL, sep = sep, md.args = md.args)
  result <- batch_collect(names = object, fun = do_collect, include = include,
    ..., simplify = FALSE, demo = demo)
  if (!demo)
    result <- do.call(rbind, result)
  rownames(result) <- NULL # if 'previous' was given, row names lacked anyway
  finish_template(result, outfile, sep, previous, md.args, demo)
}, sealed = SEALED)

setMethod("collect_template", "OPM", function(object, outfile = NULL,
    sep = "\t", previous = outfile, md.args = list(),
    selection = opm_opt("csv.selection"), add.cols = NULL, normalize = -1L,
    instrument = NULL, ..., demo = FALSE) {
  result <- csv_data(object = object, keys = selection, normalize = normalize)
  result <- as.list(result)
  if (length(instrument)) {
    if (!is.logical(L(instrument)))
      result[[INSTRUMENT]] <- must(as.integer(instrument))
    else if (instrument)
      result[[INSTRUMENT]] <- L(get("machine.id", OPM_OPTIONS))
  }
  result <- as.data.frame(x = result, stringsAsFactors = FALSE, optional = TRUE)
  if (length(add.cols)) {
    add.cols <- matrix(NA_character_, nrow(result), length(add.cols), FALSE,
      list(NULL, add.cols))
    result <- cbind(result, add.cols, stringsAsFactors = FALSE)
  }
  finish_template(result, outfile, sep, previous, md.args, demo)
}, sealed = SEALED)

setMethod("collect_template", "OPMS", function(object, outfile = NULL,
    sep = "\t", previous = outfile, md.args = list(),
    selection = opm_opt("csv.selection"), add.cols = NULL, normalize = -1L,
    instrument = NULL, ..., demo = FALSE) {
  result <- lapply(X = object@plates, FUN = collect_template, md.args = md.args,
    add.cols = add.cols, sep = sep, selection = selection, previous = NULL,
    normalize = normalize, instrument = instrument, outfile = NULL, ...)
  finish_template(do.call(rbind, result), outfile, sep, previous, md.args, demo)
}, sealed = SEALED)

setMethod("collect_template", "MOPMX", function(object,
    outfile = NULL, sep = "\t", previous = outfile, md.args = list(),
    selection = opm_opt("csv.selection"), add.cols = NULL,
    normalize = -1L, instrument = NULL, ..., demo = FALSE) {
  result <- lapply(X = object, FUN = collect_template, selection = selection,
    add.cols = add.cols, normalize = normalize, instrument = instrument,
    outfile = NULL, previous = NULL, sep = sep, md.args = md.args, ...)
  finish_template(do.call(rbind, result), outfile, sep, previous, md.args, demo)
}, sealed = SEALED)


#= to_metadata collect_template

#' @rdname collect_template
#' @export
#'
setGeneric("to_metadata",
  function(object, ...) standardGeneric("to_metadata"))

setMethod("to_metadata", "character", function(object, stringsAsFactors = FALSE,
    optional = TRUE, sep = c("\t", ",", ";"), strip.white = NA, ...) {
  if (length(object) > 1L && !is.null(names(object))) {
    if (is.na(L(strip.white)))
      strip.white <- FALSE
    return(to_metadata(object = vector2row(object), strip.white = strip.white,
      sep = sep, stringsAsFactors = stringsAsFactors, optional = optional, ...))
  }
  if (!length(sep))
    stop("empty 'sep' argument")
  if (is.na(L(strip.white)))
    strip.white <- TRUE
  for (char in sep) {
    x <- read.delim(file = object, sep = char, check.names = !optional,
      strip.white = strip.white, stringsAsFactors = stringsAsFactors, ...)
    if (ncol(x) > 1L)
      break
  }
  x
}, sealed = SEALED)

setMethod("to_metadata", "ANY", function(object, stringsAsFactors = FALSE,
    optional = TRUE, sep = "\t", strip.white = FALSE, ...) {
  x <- as.data.frame(x = object, stringsAsFactors = stringsAsFactors,
    optional = optional, ...)
  if (L(strip.white))
    x <- strip_whitespace(x)
  x
}, sealed = SEALED)

setMethod("to_metadata", "WMD", function(object, stringsAsFactors = FALSE,
    optional = TRUE, sep = "\t", strip.white = FALSE, ...) {
  x <- collect(x = list(object@metadata), what = "values",
    optional = optional, stringsAsFactors = stringsAsFactors,
    dataframe = TRUE, keep.unnamed = NA, ...)
  if (L(strip.white))
    x <- strip_whitespace(x)
  x
}, sealed = SEALED)

setMethod("to_metadata", "WMDS", function(object, stringsAsFactors = FALSE,
    optional = TRUE, sep = "\t", strip.white = FALSE, ...) {
  x <- collect(x = metadata(object), what = "values",
    optional = optional, stringsAsFactors = stringsAsFactors,
    dataframe = TRUE, keep.unnamed = NA, ...)
  if (L(strip.white))
    x <- strip_whitespace(x)
  x
}, sealed = SEALED)

setMethod("to_metadata", "MOPMX", function(object, stringsAsFactors = FALSE,
    optional = TRUE, sep = "\t", strip.white = FALSE, ...) {
  collect_rows(lapply(X = object, FUN = to_metadata, optional = optional,
    sep = sep, stringsAsFactors = stringsAsFactors, strip.white = FALSE, ...))
}, sealed = SEALED)


################################################################################
################################################################################
#
# Batch IO with OPM objects
#


## NOTE: not an S4 method because conversion is done

#' Batch-convert \acronym{PM} data
#'
#' Batch-convert from OmniLog\eqn{\textsuperscript{\textregistered}}{(R)}
#' \acronym{CSV} (or previous \pkg{opm} \acronym{YAML} or \acronym{JSON}) to
#' \pkg{opm} \acronym{YAML} (or \acronym{JSON}). It is possible to add metadata
#' to each set of raw data and to aggregate the curves; these additional data
#' will then be included in the output files.
#'
#' @inheritParams read_opm
#' @inheritParams batch_process
#' @param md.args If not \code{NULL} but a list, passed as arguments to
#'   \code{\link{include_metadata}} with the data read from each individual file
#'   as additional argument \sQuote{object}. If \code{NULL}, metadata are not
#'   included (but may already be present in the case of \acronym{YAML} input).
#' @param aggr.args If not \code{NULL} but a list, passed as arguments to
#'   \code{\link{do_aggr}} with the data read from each individual file as
#'   additional argument \code{object}.  If \code{NULL}, aggregation takes not
#'   place (but aggregated data may already be present in case of \acronym{YAML}
#'   input).
#' @param force.aggr Logical scalar. If \code{FALSE}, do not aggregate already
#'   aggregated data (which can be present in \acronym{YAML} input).
#' @param disc.args If not \code{NULL} but a list, passed as arguments to
#'   \code{\link{do_disc}} with the data read from each individual file as
#'   additional argument \code{object}.  If \code{NULL}, discretisation takes
#'   not place (but discretised data may already be present in case of
#'   \acronym{YAML} input).
#' @param force.disc Logical scalar. If \code{FALSE}, do not discretise already
#'   discretised data (which can be present in \acronym{YAML} input).
#' @param force.plate Logical scalar passed as \code{force} argument to
#'   \code{\link{read_opm}}.
#' @param device Character scalar describing the graphics device used for
#'   outputting plots. See \code{Devices} from the \pkg{grDevices} package and
#'   \code{mypdf} from the \pkg{pkgutils} package for possible values. The
#'   extension of the output files is created from the device name after a few
#'   adaptations (such as converting \kbd{postscript} to \kbd{ps}).
#' @param dev.args List. Passed as additional arguments to \code{device}.
#' @param plot.args List. Passed as additional arguments to the plotting
#'   function used.
#' @param csv.args If not \code{NULL} but a list, used for specifying ways to
#'   use \code{\link{csv_data}} entries directly as \code{\link{metadata}}. The
#'   list can contain character vectors used for selecting and optionally
#'   renaming \acronym{CSV} entries or functions that can be applied to an
#'   entire data frame containing all \acronym{CSV} entries. Note that this
#'   argument has nothing to do with \kbd{csv} output.
#' @param table.args Passed to \code{write.table} from the \pkg{utils} package
#'   if \code{output} is set to \kbd{csv}. Do not confuse this with
#'   \code{csv.args}.
#' @param ... Optional arguments passed to \code{\link{batch_process}} in
#'   addition to \code{verbose} and \code{demo}. Note that \code{out.ext},
#'   \code{fun} and \code{fun.args} are set automatically. Alternatively,
#'   these are parameters passed to \code{\link{batch_collect}}.
#' @param output Character scalar determining the main output mode. \describe{
#'   \item{clean}{Apply \code{clean_filenames} from the \pkg{pkgutils} package.}
#'   \item{csv}{Create \acronym{CSV} files, by default one per input file.}
#'   \item{json}{Create \acronym{JSON} files, by default one per input file.}
#'   \item{levelplot}{Create graphics files, by default one per input file,
#'     containing the output of \code{\link{level_plot}}.}
#'   \item{split}{Split multiple-plate new style or old style \acronym{CSV}
#'     files with \code{\link{split_files}}.}
#'   \item{yaml}{Create \acronym{YAML} files, by default one per input file.}
#'   \item{xyplot}{Create graphics files, by default one per input file,
#'     containing the output of \code{\link{xy_plot}}.}
#'   }
#'   The \code{clean} mode might be useful for managing
#'   OmniLog\eqn{\textsuperscript{\textregistered}}{(R)} \acronym{CSV} files,
#'   which can contain a lot of special characters.
#' @param combine.into Empty or character scalar modifying the output mode
#'   unless it is \sQuote{clean} or \sQuote{split}. If non-empty, causes the
#'   creation of a single output file named per plate type encountered in the
#'   input, instead of one per input file (the default). Thus
#'   \code{combine.into} should be given as a template string for \code{sprintf}
#'   from the \pkg{base} package with one placeholder for the plate-type, and
#'   without a file extension.
#' @export
#' @return The function invisibly returns a matrix which describes each
#'   attempted file conversion. See \code{\link{batch_process}} for details.
#' @family io-functions
#' @references \url{http://www.yaml.org/}
#' @references \url{http://www.json.org/}
#' @references \url{http://www.biolog.com/}
#' @references Caspi, R., Billington, R., Ferrer, L., Foerster, H., Fulcher, C.
#'   A., Keseler, I. M., Kothari, A., Krummenacker, M., Latendresse, M.,
#'   Mueller, L. A., Ong, Q., Paley, S., Subhraveti, P., Weaver, D. S., Karp, P.
#'   D. 2016 The MetaCyc database of metabolic pathways and enzymes and the
#'   BioCyc collection of pathway/genome databases. \emph{Nucleic Acids
#'   Research} \strong{44}, D471--D480 [opm YAML usage example].
#' @seealso utils::read.csv yaml::yaml.load_file grDevices::Devices
#' @seealso pkgutils::mypdf
#' @keywords IO
#'
#' @details
#'   This function is for batch-converting many files; for writing a single
#'   object to a \acronym{YAML} file (or string), see \code{\link{to_yaml}}.
#'
#'   A \acronym{YAML} document can comprise \emph{scalars} (single values of
#'   some type), \emph{sequences} (ordered collections of some values, without
#'   names) and \emph{mappings} (collections assigning a name to each value),
#'   in a variety of combinations (e.g., mappings of sequences). The output
#'   of \code{batch_opm} is one \acronym{YAML} document \emph{per plate}
#'   which represents a mapping with the following components (key-value pairs):
#'   \describe{
#'     \item{metadata}{Arbitrarily nested mapping of arbitrary metadata
#'       entries. Empty if no metadata have been added.}
#'     \item{csv_data}{Non-nested mapping containing the
#'       OmniLog\eqn{\textsuperscript{\textregistered}}{(R)} run information
#'       read from the input \acronym{CSV} file (character scalars) together
#'       with the measurements. The most important entry is most likely the
#'       plate type.}
#'     \item{measurements}{A mapping whose values are sequences of
#'       floating-point numbers of the same length and in the appropriate order.
#'       The keys are \sQuote{hours}, which refers to the time points, and the
#'       well coordinates, ranging between \sQuote{A01} and \sQuote{H12}.}
#'     \item{aggregated}{A mapping, only present if curve parameters have been
#'       estimated. Its keys correspond to those of \sQuote{measurements} with
#'       the exception of \sQuote{hours}. The values are themselves mappings,
#'       whose keys indicate the respective curve parameter and whether this is
#'       the point estimate or the upper or lower confidence interval. The
#'       values of these secondary mappings are floating-point numbers.}
#'     \item{aggr_settings}{A mapping, only present if curve parameters have
#'       been estimated. Its keys are \sQuote{software}, \sQuote{version} and
#'       \sQuote{options}. The value of the former two is a character scalar.
#'       The value of \sQuote{options} is an arbitrarily nested mapping with
#'       arbitrary content.}
#'     \item{discretized}{A mapping, only present if curve parameters have been
#'       estimated and also discretised. Its keys correspond to those of
#'       \sQuote{measurements} with the exception of \sQuote{hours}. The values
#'       are logical scalars.}
#'     \item{disc_settings}{A mapping, only present if curve parameters have
#'       been estimated and also discretised. Its keys are \sQuote{software},
#'       \sQuote{version} and \sQuote{options}. The value of the former two is a
#'       character scalar. The value of \sQuote{options} is an arbitrarily
#'       nested mapping with arbitrary content.}
#'   }
#'   Details of the contents should be obvious from the documentation of the
#'   classes of the objects from which the \acronym{YAML} output is generated.
#'   In the case of \acronym{YAML} input with several plates per file,
#'   \code{batch_opm} generates \acronym{YAML} output files containing a
#'   sequence of mappings as described above, one per plate, to keep a 1:1
#'   relationship between input and output files.
#'
#'   Attempting to generate \acronym{YAML} from input data with a wrong
#'   character encoding might cause \R to crash or hang. This problem was
#'   observed with \acronym{CSV} files that were generated on a distinct
#'   operating system and contained special characters such as German umlauts.
#'   It is then necessary to explicitly (and correctly) specify the encoding
#'   used in these files; see the \sQuote{file.encoding} option of
#'   \code{\link{opm_opt}} for how to do this.
#'
#'   \acronym{JSON}, which is almost a subset of \acronym{YAML}, can also be
#'   generated, but has more restrictions. It is only recommended if a
#'   \acronym{YAML} parser is unavailable. It is also more delicate regarding
#'   the encoding of character strings.
#'
#'   When inputting \acronym{YAML} files generated with the help of the
#'   \pkg{yaml} package (on which the \pkg{opm} implementation is based), or
#'   \acronym{JSON} files generated with the help of the \pkg{rjson} package,
#'   using other programming languages, a potential problem is that they, and
#'   \acronym{YAML} in general, lack a native representation of \code{NA}
#'   values. Such entries are likely to be misunderstood as \sQuote{NA}
#'   character scalars (if the \pkg{json} package or the \pkg{yaml} package
#'   prior to version 2.1.7 are used) or as \code{.na}, \code{.na.real},
#'   \code{.na.logical} or \code{.na.character} character scalars (if more
#'   recent versions of the \pkg{yaml} package are used). Input functions in
#'   other programming languages should conduct according conversions. \pkg{opm}
#'   translates these values when converting a list to a \code{\link{OPM}}
#'   object.
#'
#'   See \code{\link{as.data.frame}} regarding the generated \acronym{CSV}.
#'
#' @examples
#' test.files <- opm_files("omnilog")
#' if (length(test.files) > 0) { # if the files are found
#'   num.files <- length(list.files(outdir <- tempdir()))
#'   x <- batch_opm(test.files[1], outdir = outdir)
#'   stopifnot(length(list.files(outdir)) == num.files + 1, is.matrix(x))
#'   stopifnot(file.exists(x[, "outfile"]))
#'   stopifnot(test.files[1] == x[, "infile"])
#'   unlink(x[, "outfile"])
#' } else {
#'   warning("opm example data files not found")
#' }
#'
batch_opm <- function(names, md.args = NULL, aggr.args = NULL,
    force.aggr = FALSE, disc.args = NULL, force.disc = FALSE,
    gen.iii = opm_opt("gen.iii"), force.plate = FALSE, device = "mypdf",
    dev.args = NULL, plot.args = NULL, csv.args = NULL,
    table.args = list(sep = "\t", row.names = FALSE),
    ..., proc = 1L, outdir = "", overwrite = "no",
    output = c("yaml", "json", "csv", "xyplot", "levelplot", "split", "clean"),
    combine.into = NULL, verbose = TRUE, demo = FALSE) {

  csv2md <- function(x, spec) {
    if (!is.matrix(x)) # OPM objects yield only a character vector
      x <- t(as.matrix(x))
    x <- to_metadata(x)
    spec <- flatten(list(spec))
    spec <- rapply(spec, as.character, "factor", NULL, "replace")
    if (any(!vapply(spec, inherits, NA, c("character", "function"))))
      stop("can only apply character vector, factor or function to CSV data")
    for (approach in spec)
      if (is.character(approach)) {
        for (name in approach[!approach %in% colnames(x)])
          x[, name] <- seq_len(nrow(x))
        x <- x[, approach, drop = FALSE]
        if (!is.null(names(approach)))
          colnames(x) <- names(approach)
      } else {
        x <- approach(x)
        if (!is.data.frame(x)) # wrong no. of rows should yield error later on
          stop("function applied to CSV data must yield data frame")
      }
    x
  }

  convert_dataset <- function(data) {
    switch(mode(gen.iii),
      logical = if (gen.iii) {
        if (verbose)
          message("conversion: changing to 'Generation III'...")
        data <- gen_iii(data, force = force.plate)
      },
      character = if (nzchar(gen.iii)) {
        if (verbose)
          message(sprintf("conversion: changing to '%s'...", gen.iii))
        data <- gen_iii(data, to = gen.iii, force = force.plate)
      },
      stop("'gen.iii' must either be a logical or a character scalar")
    )
    if (length(csv.args)) {
      if (verbose)
        message("conversion: using CSV data as metadata...")
      metadata(data, 1L) <- csv2md(csv_data(data), csv.args)
    }
    if (length(md.args)) {
      if (verbose)
        message("conversion: including metadata...")
      data <- do.call(include_metadata, c(object = data, md.args))
    }
    if (length(aggr.args)) {
      if (force.aggr || !has_aggr(data)) {
        if (verbose)
          message("conversion: aggregating data...")
        data <- do.call(do_aggr, c(list(object = data), aggr.args))
      } else if (verbose)
        message("conversion: previously aggregated data present, ",
          "skipping that step")
    }
    if (length(disc.args)) {
      if (force.aggr || !has_disc(data)) {
        if (verbose)
          message("conversion: discretizing data...")
        data <- do.call(do_disc, c(list(data), disc.args))
      } else if (verbose)
        message("conversion: previously discretized data present, ",
          "skipping that step")
    }
    data
  }

  read_file <- function(infile) {
    data <- read_single_opm(infile)
    if (is.list(data)) # YAML input can result in lists of several OPM objects
      lapply(data, convert_dataset)
    else
      convert_dataset(data)
  }

  create_yaml <- function(x, outfile) {
    if (is.list(x)) # would be more elegant if to_yaml() could handle that
      x <- lapply(x, as, "list")
    write(to_yaml(x, json = json), outfile)
  }
  convert_to_yaml <- function(infile, outfile) {
    create_yaml(read_file(infile), outfile)
  }

  create_csv <- function(x, outfile) {
    if (is.list(x))
      x <- do.call(rbind, lapply(x, as.data.frame))
    else
      x <- as.data.frame(x)
    do.call(write.table, c(list(x = x, file = outfile), as.list(table.args)))
  }
  convert_to_csv <- function(infile, outfile) {
    create_csv(read_file(infile), outfile)
  }

  create_plot <- function(x, outfile) {
    do.call(device, c(list(file = outfile), dev.args))
    print(do.call(plot.type, c(list(x = x), plot.args)))
    dev.off()
  }
  create_plot_from_file <- function(infile, outfile) {
    data <- read_file(infile)
    if (is.list(data))
      data <- opms(data, group = TRUE)
    create_plot(data, outfile)
  }

  convert_to_single_file <- function(names, outfile.template, out.ext, demo,
      verbose, ..., proc) {
    x <- read_opm(names = names, convert = "grp", ..., demo = demo)
    if (demo) {
      if (verbose)
        message(paste0(x, collapse = "\n"))
      return(invisible(x))
    }
    out.names <- gsub(" ", "-", names(x), FALSE, FALSE, TRUE)
    out.names <- paste(sprintf(outfile.template, out.names), out.ext, sep = ".")
    x <- mclapply(x, convert_dataset, mc.cores = proc)
    mcmapply(create_single_file, x, out.names, mc.cores = proc)
    names(out.names) <- names(x)
    if (verbose)
      message(listing(out.names))
    out.names
  }

  graphics_format_map <- function() c(bitmap = "bmp", mypdf = "pdf",
    postscript = "ps", cairo_pdf = "pdf", cairo_ps = "ps")

  LL(force.aggr, force.disc, gen.iii, force.plate, device, overwrite)

  # If a metadata file name is given, read it into data frame right now to
  # avoid opening the file each time in the batch_process() loop
  if (length(md.args) && is.character(md.args$md)) {
    tmp <- md.args
    names(tmp)[names(tmp) == "md"] <- "object"
    tmp$replace <- NULL
    md.args$md <- do.call(to_metadata, tmp)
  }

  case(output <- match.arg(output),
    yaml = {
      collect <- FALSE
      io.fun <- convert_to_yaml
      create_single_file <- create_yaml
      json <- FALSE
      in.ext <- "both"
      out.ext <- "yml"
    },
    json = {
      collect <- FALSE
      io.fun <- convert_to_yaml
      create_single_file <- create_yaml
      json <- TRUE
      in.ext <- "both"
      out.ext <- "json"
    },
    csv = {
      collect <- FALSE
      io.fun <- convert_to_csv
      create_single_file <- create_csv
      json <- NULL
      in.ext <- "both"
      out.ext <- "tab"
    },
    levelplot = {
      collect <- FALSE
      io.fun <- create_plot_from_file
      create_single_file <- create_plot
      json <- disc.args <- aggr.args <- NULL
      in.ext <- "both"
      out.ext <- map_values(device, graphics_format_map())
      plot.type <- level_plot
    },
    xyplot = {
      collect <- FALSE
      io.fun <- create_plot_from_file
      create_single_file <- create_plot
      json <- disc.args <- aggr.args <- NULL
      in.ext <- "both"
      out.ext <- map_values(device, graphics_format_map())
      plot.type <- xy_plot
    },
    split = {
      collect <- TRUE
      io.fun <- split_files
      in.ext <- "csv"
      fun.args <- list(pattern = '^("Data File",|Data File)', outdir = outdir,
        demo = demo)
    },
    clean = {
      collect <- TRUE
      io.fun <- clean_filenames
      in.ext <- "both"
      fun.args <- list(demo = demo, overwrite = overwrite == "yes")
    }
  )

  if (collect) # the functions have their own 'demo' argument
    invisible(batch_collect(names = names, fun = io.fun, fun.args = fun.args,
      proc = proc, ..., demo = FALSE))
  else if (length(combine.into))
    invisible(convert_to_single_file(names = names, out.ext = out.ext, ...,
      outfile.template = combine.into, demo = demo, verbose = verbose,
      proc = proc))
  else
    batch_process(names = names, out.ext = out.ext, io.fun = io.fun,
      in.ext = in.ext, compressed = TRUE, literally = FALSE, ..., proc = proc,
      overwrite = overwrite, outdir = outdir, verbose = verbose, demo = demo)
}


