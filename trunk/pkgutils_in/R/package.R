

################################################################################


#' List or modify package (description) files
#'
#' \code{pack_desc} reads the \sQuote{DESCRIPTION} file of an \R package and
#' makes use of its content in several possible ways.
#' \code{pkg_files} lists files within given subdirectories of a package. It
#' works on either installed packages or package source folders.
#' \code{is_pkg_dir} determines whether names refer to such package directories.
#'
#' @param pkg Name(s) of one to several package directories. The package name
#'   alone does \strong{not} suffice unless the package is a subdirectory
#'   of the working directory.
#' @param action Character scalar determining the output mode.
#' @param version Logical scalar. Also update the version? Only relevant if
#'   \code{action} is \sQuote{update}. Note that this updating would only
#'   affect the last part of the version string separated by \sQuote{-}; if
#'   this does not exist, it is ignored. Updating is \strong{not} done if the
#'   old date is identical or newer to the new one. Note that this comparison
#'   only works properly if \code{data.format} is correctly specified.
#' @param demo Logical scalar. Do not update or source files, just return a
#'   description of the result?
#' @param date.format Character scalar. The format used and expected for the
#'   date of the package.
#' @param envir Environment used when reading the files with \code{source}. Only
#'   relevant if \code{action} is set to \sQuote{source}.
#'
#' @param x Character vector. For \code{pkg_files}, if \code{installed} is
#'   \code{TRUE}, the names of installed packages. Otherwise names of package
#'   directories, which are expanded automatically, and/or directly the names of
#'   files, which may or may not reside within a package. The directory must be
#'   recognisable by \code{is_pkg_dir} for the expansion to work. For
#'   \code{is_pkg_dir}, an arbitrary character vector.
#' @param what Character vector. The subdirectories to list.
#' @param installed Logical scalar. If \code{TRUE}, the package(s) are searched
#'   using \code{find.package}. Otherwise \code{pkg} is treated as list of
#'   directories and/or file names, distinguished using
#'   \code{is_pkg_dir}.
#' @param ignore \code{NULL} or a character vector of file names (without their
#'   directory-name parts) to remove from the result, or a list. Ignored if
#'   empty. If a non-empty character vector, matching is done case-
#'   insensitively. If a list, used as arguments passed to \code{grep} (except
#'   for \code{x}, \code{value} and \code{invert}). Enclosing \code{ignore} in
#'   \code{I()} reverts the action.
#'
#' @param ... Optional arguments passed to and from other methods, or between
#'   the methods.
#' @export
#' @details
#' \code{pack_desc} optionally sets the \sQuote{Date} entry to the current date,
#' and if requested increments the subversion number of the package version, if
#' any, and writes the data back to each input file. Alternatively, it calls
#' \code{source} on all \R code files of a package as listed in the
#' \sQuote{DESCRIPTION} file. Spell checking is another option.
#' @return The value returned by \code{pack_desc} depends on the value of
#'   \code{action}:
#'   \describe{
#'     \item{read}{Object of class \code{pack_descs}, basically a nested
#'       list with \code{pkg} as names. The values are objects of class
#'       \code{pack_desc}.}
#'     \item{source}{This loads the \R code files of the package(s) using
#'       \code{source} from the \pkg{base} package in the correct
#'       order, and call \code{library} on all the package names given under
#'       \sQuote{Depends} and \sQuote{Imports}. Thus a list of
#'       \code{source} results is obtained, returned invisibly. \code{\dots} is
#'       passed to \code{source} if \code{demo} is \code{FALSE}.}
#'     \item{spell}{Check the spelling of using \code{aspell} and an appropriate
#'       filter. This yields an object of class \code{aspell}. Package names
#'       marked as misspelled, if any, are automatically discarded.}
#'     \item{update}{One- or two-column character matrix with one row per entry
#'       in \code{pkg}, showing the updated date and optionally the version
#'       string. \code{\dots} is passed to \code{write.dcf} if \code{demo} is
#'       \code{FALSE}.}
#'   }
#'
#'   \code{pkg_files} yields a character vector of file names (empty if no such
#'   files are found).
#'
#'   \code{is_pkg_dir} yields a logical vector with the same length than
#'   \code{x}.
#'
#' @family package-functions
#' @keywords package
#' @seealso base::read.dcf base::write.dcf base::source
#'   base::list.files base::find.package base::system.file
#'   utils::packageDescription
#' @examples
#'
#' pkg <- find.package(c("tools", "utils"), quiet = TRUE)
#'
#' # Reading
#' (x <- pack_desc(pkg, "read")) # should look similar to packageVersion()
#' stopifnot(is.list(x), names(x) == pkg, inherits(x, "pack_descs"))
#' stopifnot(sapply(x, is.list), sapply(x, inherits, what = "pack_desc"))
#'
#' # Updating (in demo mode, of course)
#' (x <- pack_desc(pkg, "update", demo = TRUE, date.format = "%Y/%m/%d"))
#' stopifnot(is.character(x), is.matrix(x), rownames(x) == pkg,
#'   colnames(x) == c("Date", "Version"))
#' (x <- pack_desc(pkg, "update", demo = TRUE, version = FALSE))
#' stopifnot(is.character(x), is.matrix(x), rownames(x) == pkg,
#'   colnames(x) == "Date")
#'
#' # Source'ing (in demo mode, of course)
#' (x <- pack_desc(pkg, "source", demo = TRUE))
#' stopifnot(is.list(x), names(x) == pkg, sapply(x, is.list))
#' stopifnot(sapply(x, names) == c("Depends", "Imports", "Collate"))
#'
#' # See also the 'docu.R' script, options '--format' and '--keep'.
#'
#' ## pkg_files()
#' pkg <- find.package(c("tools", "utils"), quiet = TRUE)
#' (x <- pkg_files(pkg, "R"))
#' stopifnot(is.character(x), length(x) > 0)
#'
#' ## is_pkg_dir()
#' (x <- is_pkg_dir(c("foo", "bar", "baz")))
#' stopifnot(!x)
#' (x <- is_pkg_dir(find.package(c("tools", "utils"), quiet = TRUE)))
#' stopifnot(x)
#'
pack_desc <- function(pkg, ...) UseMethod("pack_desc")

#' @rdname pack_desc
#' @method pack_desc character
#' @export
#'
pack_desc.character <- function(pkg,
    action = c("read", "update", "source", "spell"),
    version = TRUE, demo = FALSE, date.format = "%Y-%m-%d",
    envir = globalenv(), ...) {
  LL(version, demo, date.format)
  action <- match.arg(action)
  x <- normalizePath(file.path(pkg, "DESCRIPTION"))
  x <- lapply(x, function(file) {
      stopifnot(nrow(y <- read.dcf(file)) == 1L)
      structure(as.list(y[1L, ]), file = file,
        class = c("pack_desc", "packageDescription"))
    })
  x <- structure(x, names = pkg, class = "pack_descs")
  case(action,
    read = x,
    update = {
      x <- update(object = x, version = version, date.format = date.format)
      if (!demo)
        puts(x = x, file = vapply(x, attr, "", which = "file"), ...)
      wanted <- "Date"
      if (version)
        wanted <- c(wanted, "Version")
      x <- lapply(wanted, function(i) vapply(x, `[[`, "", i = i))
      x <- do.call(cbind, x)
      colnames(x) <- wanted
      x
    },
    spell = {
      res <- aspell(files = vapply(x, "attr", "", "file"), filter = "dcf", ...)
      remove <- !logical(nrow(res))
      for (desc in x) {
        ok <- subset(desc)[c("Depends", "Imports", "Enhances", "Suggests")]
        ok <- unlist(c(ok, desc$Package), TRUE, FALSE)
        ok <- sub("\\d+$", "", ok, FALSE, TRUE)
        remove <- remove &
          !(res[, "Original"] %in% ok & res[, "File"] == attr(desc, "file"))
      }
      res[remove, , drop = FALSE]
    },
    source = source_files(x = x, demo = demo, envir = envir, ...)
  )
}

#' @rdname pack_desc
#' @export
#'
pkg_files <- function(x, ...) UseMethod("pkg_files")

#' @rdname pack_desc
#' @method pkg_files character
#' @export
#'
pkg_files.character <- function(x, what, installed = TRUE, ignore = NULL,
    ...) {
  filter <- function(x) {
    if (!length(ignore))
      return(x)
    if (is.list(ignore))
      x <- do.call(grep, c(list(x = x, value = TRUE,
        invert = !inherits(ignore, "AsIs")), ignore))
    else if (inherits(ignore, "AsIs"))
      x <- x[tolower(basename(x)) %in% tolower(ignore)]
    else
      x <- x[!tolower(basename(x)) %in% tolower(ignore)]
    x
  }
  if (L(installed)) {
    result <- find.package(basename(x), quiet = TRUE, verbose = FALSE)
    if (!length(result))
      return(character())
    result <- list.files(path = file.path(result, what), full.names = TRUE, ...)
    normalizePath(filter(result), winslash = "/")
  } else if (any(is.pack.dir <- is_pkg_dir(x))) {
    result <- as.list(x)
    result[is.pack.dir] <- lapply(x[is.pack.dir], function(name) {
      list.files(path = file.path(path = name, what), full.names = TRUE, ...)
    })
    filter(unlist(result))
  } else
    filter(x)
}

#' @rdname pack_desc
#' @export
#'
is_pkg_dir <- function(x) UseMethod("is_pkg_dir")

#' @rdname pack_desc
#' @method is_pkg_dir character
#' @export
#'
is_pkg_dir.character <- function(x) {
  result <- file_test("-d", x)
  result[result] <- file_test("-f", file.path(x[result], "DESCRIPTION"))
  result
}


################################################################################


#' Run \command{R CMD}
#'
#' Externally call \command{R CMD}, e.g. for checking or installing \R packages,
#' or conduct some postprocessing after checking or installation.
#'
#' @inheritParams pkg_files
#'
#' @param x For \code{run_R_CMD}, a character vector with arguments passed to
#'   \command{R CMD <what>}. If these are the names of one to several package
#'   directories, the package name alone does \strong{not} suffice unless the
#'   package is a subdirectory of the working directory.
#'
#'   For \code{copy_pkg_files} and \code{delete_o_files}, a character vector
#'   passed as eponymous argument to \code{\link{pkg_files}}.
#'
#' @param what Character scalar with the name of the command used by \command{R
#'   CMD}, or (for \code{copy_pkg_files}) a character vector with the names of
#'   subdirectories to copy.
#' @param sudo Logical scalar. Prepend a \command{sudo} call to the command?
#'   Probably makes only sense on UNIX-like systems.
#' @param system.args Optional list of arguments passed to \code{system} from
#'   the \pkg{base} package.
#'
#' @param to Character vector indicating the target folder(s) or file(s).
#' @param overwrite Logical scalar passed to \code{file.copy} from the
#'   \pkg{base} package (in addition to \code{from} and \code{to}).
#'
#' @param ext Character vector with file extensions to consider. Need not
#'   contain the leading dot. Content of \code{.Platform$dynlib.ext} is added
#'   automatically.
#'
#' @param ... For \code{run_R_CMD}, optional command-line switches passed to
#'   \command{R CMD <what>}. In contrast to \code{x}, leading dashes are
#'   automatically prepended as necessary. For \code{copy_pkg_files}, optional
#'   arguments passed to \code{file.copy} from the \pkg{base} package.
#'
#' @return The return value of \code{run_R_CMD} is the one of the call of
#'   \command{R CMD}, depending on \code{system.args}, by default an integer
#'   indicating success or failure.
#'
#'   \code{copy_pkg_files} returns a logical vector. See \code{file.copy} from
#'   the \pkg{base} package for details.
#'
#'   \code{delete_o_files} also returns a logical vector, this time indicating
#'   whether deletion succeeded. See \code{file.remove} from the \pkg{base}
#'   package.
#'
#' @details Windows users might need to install \pkg{Rtools} for
#'   \code{run_R_CMD} to work, see
#'   \url{https://cran.r-project.org/bin/windows/Rtools/}.
#'
#'   \code{copy_pkg_files} copies package files after installation. This is
#'   mainly intended for script files, which should often be placed in a
#'   directory for executable files. The \command{docu.R} and \command{merge.R}
#'   scripts that come with the \pkg{pkgutils} package are examples for such
#'   files.
#'
#'   \code{delete_o_files} removes object files in the \file{src} subdirectory
#'   of a package remaining from previous compilation attempts, if any.
#'
#' @export
#' @family package-functions
#' @keywords package
#' @seealso base::system base::file.copy base::file.remove
#' @examples
#' # Running R CMD <what>: see the 'docu.R' script provided with this package,
#' # options '--check', # '--install' and '--yes'.
#'
#' # Copying files: see the 'docu.R' script provided with this package, options
#' # '--target' and '--exclude'.
#'
#' # Deleting object files: see the 'docu.R' script provided with this package,
#' # option '--zapoff'.
#'
run_R_CMD <- function(x, ...) UseMethod("run_R_CMD")

#' @rdname run_R_CMD
#' @method run_R_CMD character
#' @export
#'
run_R_CMD.character <- function(x, what = "check", ...,
    sudo = identical(what, "INSTALL"), system.args = list()) {
  r.exe <- Sys.which("R")
  r.exe <- r.exe[nzchar(r.exe)]
  LL(what, sudo, r.exe)
  pat <- "%s --vanilla CMD %s %s %s"
  if (sudo)
    pat <- paste("sudo", pat)
  args <- paste0(prepare_options(unlist(c(...))), collapse = " ")
  cmd <- sprintf(pat, r.exe, what, args, paste0(x, collapse = " "))
  do.call(system, c(list(command = cmd), system.args))
}

#' @rdname run_R_CMD
#' @export
#'
copy_pkg_files <- function(x, ...) UseMethod("copy_pkg_files")

#' @rdname run_R_CMD
#' @method copy_pkg_files character
#' @export
#'
copy_pkg_files.character <- function(x, what = "scripts",
    to = file.path(Sys.getenv("HOME"), "bin"), installed = TRUE,
    ignore = NULL, overwrite = TRUE, ...) {
  files <- pkg_files(x, what, installed = installed, ignore = ignore)
  file.copy(from = files, to = to, overwrite = overwrite, ...)
}

#' @rdname run_R_CMD
#' @export
#'
delete_o_files <- function(x, ...) UseMethod("delete_o_files")

#' @rdname run_R_CMD
#' @method delete_o_files character
#' @export
#'
delete_o_files.character <- function(x, ext = "o", ignore = NULL, ...) {
  ext <- tolower(c(ext, .Platform$dynlib.ext))
  ext <- unique.default(tolower(sub("^\\.", "", ext, FALSE, TRUE)))
  ext <- sprintf("\\.(%s)$", paste0(ext, collapse = "|"))
  x <- pkg_files(x, what = "src", installed = FALSE, ignore = ignore, ...)
  if (length(x <- x[grepl(ext, x, TRUE, TRUE)]))
    file.remove(x)
  else
    logical()
}


################################################################################


#' Preprocess R files
#'
#' Preprocess \R code files using a simple swapping algorithm. Files are
#' modified in-place, hence this is potentially dangerous and should best be
#' applied if the package directory to document, check and/or install is not
#' the one in which coding is done but a copy of it.
#'
#' @inheritParams repair_S4_docu
#' @return Currently the return value of the call to \code{\link{run_ruby}}.
#' @details The code preprocessing works simply as follows: Lines are split at
#'   the first occurrence of \sQuote{#||}, if any, the parts reversed and joined
#'   again with a space character, including the separator. Leading whitespace
#'   is kept. Whitespace around the parts, if any, is removed (effectively
#'   transformed to a single space). There is \strong{no} check done to ensure
#'   that the part moved in front of the comment character is syntactically
#'   valid in its context, or correct or useful code. For instance, the line
#'   \samp{  SEALED <- FALSE #|| SEALED <- TRUE} would be modified to
#'   \samp{  SEALED <- TRUE #|| SEALED <- FALSE}, i.e. this could be used to
#'   change a package constant before conducting any checking. Note, however,
#'   that lines starting with a \pkg{roxygen2} comment will not be modified.
#'
#'   This preprocessing is currently implemented in a Ruby script
#'   that comes with the package. It is automatically found in the installation
#'   directory but fails if a suitable version of Ruby, i.e. \eqn{\ge 1.9.0},
#'   is unavailable. See \code{\link{run_ruby}} for details.
#' @export
#' @family package-functions
#' @keywords package
#' @examples
#' # See the 'docu.R' script provided with the package, option '--preprocess'.
#'
swap_code <- function(x, ...) UseMethod("swap_code")

#' @rdname swap_code
#' @method swap_code character
#' @export
#'
swap_code.character <- function(x, ..., ignore = NULL) {
  run_pkgutils_ruby(x = x, script = "swap_code.rb", ignore = ignore, ...)
}


################################################################################


#' Check R (or Sweave) code files
#'
#' Check certain aspects of the format of \R code files in the \sQuote{R}
#' subdirectory of a package, or of any other kinds of files. \R code can also
#' be extracted from Sweave files. Optionally write descriptions of problems to
#' the logfile used by \pkg{pkgutils}, which can be set using \code{logfile}.
#' Alternatively, check the labels used in the lines that start a Sweave code
#' chunk.
#'
#' @param x For \code{check_R_code}, a character vector of names of input files,
#'   or names of \R package directories. The latter will be expanded as
#'   appropriate. \code{x} is passed to \code{\link{pkg_files}} with the
#'   \sQuote{installed} argument set to \code{FALSE}. See there for further
#'   details.
#'
#'   For \code{logfile}, a character scalar for setting the logfile, or
#'   \code{NULL} for getting the current value. Use an empty string to turn
#'   logging off.
#' @param lwd Numeric scalar. Maximum line width allowed. Set this to a
#'   reasonably large number to effectively turn checking off.
#' @param indention Numeric scalar. Number of spaces used for one unit of
#'   indention. Set this to 1 to effectively turn checking off.
#' @param roxygen.space Numeric scalar. Number of spaces expected after
#'   \pkg{roxygen2} comments (which start with \sQuote{#} followed by a single
#'   quote).
#' @param comma Logical scalar indicating whether it should be checked that
#'   each comma is not preceded by a space but followed by a space.
#' @param ops Logical scalar indicating whether it should be checked that
#'   operators are surrounded by spaces.
#' @param parens Logical scalar indicating whether it should be checked that
#'   control-flow constructs are not directly followed by parentheses, that
#'   opening parentheses (and brackets) are not followed by a space, and that
#'   closing parentheses (and brackets) are followed by appropriate characters
#'   only and are not preceded by a space.
#' @param assign Logical scalar indicating that it should be checked that
#'   there is no line break within named function-argument assignments.
#' @param modify Logical scalar indicating whether the source code should be
#'   modified (non-destructively, of course) and input files overwritten (if
#'   changes were possible). The modifications currently only comprise the
#'   removal of whitespace from the ends of the lines and optionally the
#'   replacement of each tabulator by \code{indention} numbers of spaces (see
#'   also the next argument).
#' @param accept.tabs Logical scalar indicating whether tabulators are
#'   accepted.
#' @param three.dots Logical scalar indicating whether \code{:::} operators
#'   should result in a warning.
#' @param what Character vector naming the subdirectories to consider; passed
#'   to \code{\link{pkg_files}}
#' @param encoding Character scalar passed as \sQuote{.encoding} argument to
#'   \code{\link{map_files}}.
#' @param ignore Passed to \code{\link{pkg_files}}. See there for details. A
#'   logical scalar is used for selecting or discarding a default filter
#'   suitable for the target files.
#' @param filter Character scalar indicating the filter to use. The
#'   \code{"roxygen"} filter extracts the examples from \pkg{roxygen2}-style
#'   comments, assuming that the \sQuote{#} character that start such a comment
#'   are not preceded by whitespace. (Lines in which this is the case are
#'   detected if no filtering is used.)
#' @param ... Optional other arguments passed to \code{\link{pkg_files}}.
#'
#' @return \code{check_R_code} yields a logical vector; see
#'   \code{\link{map_files}} for details. Here the result is returned invisibly.
#'   As a side effect, problem messages are printed to \code{stderr}. See
#'   \code{\link{logfile}} for how to send these messages to a file.
#'
#'   \code{logfile} returns a character scalar with the name of the current
#'   logfile.
#'
#' @details \code{check_R_code} is intended to ensure a consistent and readable
#'   \R coding style. Not all problems can be checked, however. For instance,
#'   \code{+} and \code{-} are binary as well as unary operators and should
#'   either be followed by spaces or not, respectively. Yielding no problem
#'   messages is thus just a minimum requirement for a good coding style. For
#'   instance, indentation checking does \strong{not} check whether continuation
#'   lines have the correct number of leading spaces. Rather, checks are made
#'   line-per-line throughout. In addition to such false negatives,
#'   \code{check_R_code} falsely complains about numbers in exponential
#'   notation.
#'
#'   Functions such as \code{\link{check_keywords}} print detected problems, if
#'   any, using \code{message}. These character vectors can also be saved by
#'   appending to a logfile.
#'
#' @keywords package IO
#' @family package-functions
#' @export
#' @examples
#'
#' # Checking the R scripts that come with the package
#' (scripts <- pkg_files("pkgutils", "scripts"))
#' if (length(scripts)) {
#'   result <- check_R_code(scripts) # should not yield any messages
#'   stopifnot(is.logical(result), names(result) == names(scripts))
#' } else {
#'   warning("scripts not found")
#' }
#'
#' # See also the 'docu.R' script provided with this package, options
#' # '--blank', '--jspaces', '--width', '--assignoff', '--commaoff',
#' # '--opsoff', '--modify', '--good', '--parensoff', '--Rcheck', '--tabs' and
#' # '--untidy'. Checking can be turned off generally or specifically.
#'
#' ## logfile()
#' old <- logfile()
#' new <- tempfile()
#' logfile(new)
#' stopifnot(new == logfile())
#' logfile(old)
#' stopifnot(old == logfile())
#'
check_R_code <- function(x, ...) UseMethod("check_R_code")

#' @rdname check_R_code
#' @method check_R_code character
#' @export
#'
check_R_code.character <- function(x, lwd = 80L, indention = 2L,
    roxygen.space = 1L, comma = TRUE, ops = TRUE, parens = TRUE,
    assign = TRUE, modify = FALSE, ignore = NULL, accept.tabs = FALSE,
    three.dots = TRUE, what = "R", encoding = "",
    filter = c("none", "sweave", "roxygen"), ...) {
  spaces <- function(n) paste0(rep.int(" ", n), collapse = "")
  LL(lwd, indention, roxygen.space, modify, comma, ops, parens, assign,
    accept.tabs, three.dots)
  filter <- match.arg(filter)
  roxygen.space <- sprintf("^#'%s", spaces(roxygen.space))
  check_fun <- function(x) {
    infile <- attr(x, ".filename")
    complain <- function(text, is.bad) {
      if (any(is.bad))
        problem(text, infile = infile, line = which(is.bad))
    }
    bad_ind <- function(text, n) {
      attr(regexpr("^ *", text, FALSE, TRUE), "match.length") %% n != 0L
    }
    unquote <- function(x) {
      x <- gsub(QUOTED, "QUOTED", x, FALSE, TRUE)
      x <- sub(QUOTED_END, "QUOTED", x, FALSE, TRUE)
      x <- sub(QUOTED_BEGIN, "QUOTED", x, FALSE, TRUE)
      gsub("%[^%]+%", "%%", x, FALSE, TRUE)
    }
    code_check <- function(x) {
      x <- sub("^\\s+", "", x, FALSE, TRUE)
      complain("semicolon contained", grepl(";", x, FALSE, FALSE, TRUE))
      complain("space followed by space", grepl("  ", x, FALSE, FALSE, TRUE))
      if (three.dots)
        complain("':::' operator used", grepl(":::", x, FALSE, FALSE, TRUE))
      if (comma) {
        complain("comma not followed by space",
          grepl(",[^\\s]", x, FALSE, TRUE))
        complain("comma preceded by space", grepl("[^,]\\s+,", x, FALSE, TRUE))
      }
      if (ops) {
        x <- gsub("(?<=[(\\[])\\s*~", "DUMMY ~", x, FALSE, TRUE)
        x <- gsub("(?<=%)\\s*~", " DUMMY ~", x, FALSE, TRUE)
        complain("operator not preceded by space",
          grepl(OPS_LEFT, x, FALSE, TRUE))
        complain("operator not followed by space",
          grepl(OPS_RIGHT, x, FALSE, TRUE))
      }
      if (assign)
        complain("line ends in single equals sign",
          grepl("(^|[^=<>!])=\\s*$", x, FALSE, TRUE))
      if (parens) {
        complain("'if', 'for' or 'while' directly followed by parenthesis",
          grepl("\\b(if|for|while)\\(", x, FALSE, TRUE))
        complain("opening parenthesis or bracket followed by space",
          grepl("[([{]\\s", x, FALSE, TRUE))
        complain("closing parenthesis or bracket preceded by space",
          grepl("[^,]\\s+[)}\\]]", x, FALSE, TRUE))
        complain("closing parenthesis or bracket followed by wrong character",
          grepl("[)\\]}][^\\s()[\\]}$@:,;]", x, FALSE, TRUE))
      }
    }
    lines_to_filter_out <- function(x, how) {
      case(how,
        none = FALSE,
        roxygen = {
          pos <- grepl("^#'\\s*@\\w+\\b", x, FALSE, TRUE)
          pos <- pos | !grepl("^#'", x, FALSE, TRUE)
          pos <- as.integer(sections(pos, TRUE))
          pos[is.na(pos)] <- -1L
          starts <- grepl("^#'\\s*@examples\\b", x, FALSE, TRUE)
          !(pos %in% pos[starts]) | starts
        },
        sweave = {
          starts <- grepl("^<<.*>>=", x, FALSE, TRUE)
          stops <- grepl("^@", x, FALSE, TRUE)
          stopifnot(must(which(starts) < which(stops)))
          as.integer(sections(starts | stops)) %% 2L == 1L | starts |
            grepl("^\\s*<<.*>>\\s*$", x, FALSE, TRUE) # Sweave references
        }
      )
    }
    if (any(filtered <- lines_to_filter_out(x, filter))) {
      orig <- x
      x[filtered] <- ""
      if (filter == "roxygen") {
        x[!filtered] <- sub(roxygen.space, "", x[!filtered], FALSE, TRUE)
        filtered[!filtered] <- TRUE
      }
    } else {
      orig <- NULL
    }
    if (modify) {
      x <- sub("\\s+$", "", x, FALSE, TRUE)
      x <- gsub("\t", spaces(indention), x, FALSE, FALSE, TRUE)
    } else if (!accept.tabs)
      complain("tab contained", grepl("\t", x, FALSE, FALSE, TRUE))
    complain(sprintf("line longer than %i", lwd), nchar(x) > lwd)
    complain(sprintf("indention not multiple of %i", indention),
      bad_ind(sub(roxygen.space, "", x, FALSE, TRUE), indention))
    complain("roxygen comment not placed at beginning of line",
      grepl("^\\s+#'", x, FALSE, TRUE))
    code_check(sub("\\s*#.*", "", unquote(x), FALSE, TRUE))
    if (modify)
      if (is.null(orig))
        x
      else
        ifelse(filtered, orig, x)
    else
      NULL
  }
  invisible(map_files(pkg_files(x = x, what = what, installed = FALSE,
    ignore = ignore, ...), check_fun, .encoding = encoding))
}

#' @rdname check_R_code
#' @export
#'
check_Sweave_start <- function(x, ...) UseMethod("check_Sweave_start")

#' @rdname check_R_code
#' @method check_Sweave_start character
#' @export
#'
check_Sweave_start.character <- function(x, ignore = TRUE,
    what = c("vignettes", file.path("inst", "doc")), encoding = "", ...) {
  get_code_chunk_starts <- function(x) {
    parse_chunk_start <- function(x) {
      to_named_vector <- function(x) {
        x <- structure(vapply(x, `[[`, "", 2L), names = vapply(x, `[[`, "", 1L))
        x <- map_values(x, c(true = "TRUE", false = "FALSE"))
        lapply(as.list(x), type.convert, "", TRUE)
      }
      x <- sub("\\s+$", "", sub("^\\s+", "", x, FALSE, TRUE), FALSE, TRUE)
      implicit.label <- grepl("^[^=,]+(,|$)", x, FALSE, TRUE)
      x[implicit.label] <- paste0("label=", x[implicit.label])
      x <- strsplit(x, "\\s*,\\s*", FALSE, TRUE)
      lapply(lapply(x, strsplit, "\\s*=\\s*", FALSE, TRUE), to_named_vector)
    }
    m <- regexpr("(?<=^<<).*(?=>>=)", x, FALSE, TRUE)
    structure(parse_chunk_start(regmatches(x, m)), names = seq_along(x)[m > 0L])
  }
  check_fun <- function(x) {
    infile <- attr(x, ".filename")
    x <- get_code_chunk_starts(x)
    lines <- as.integer(names(x))
    complain <- function(text, is.bad) if (any(is.bad))
      problem(text, infile = infile, line = lines[is.bad])
    check_label <- function(x) {
      check_1 <- function(x, bad) {
        x <- gsub("[^\\w_]", "", x, FALSE, TRUE)
        bad <- duplicated.default(x, NA_character_) & !bad
        complain("duplicated label ignoring non-alphanumeric characters", bad)
        x <- gsub("\\d+", "", x, FALSE, TRUE)
        bad <- duplicated.default(x, NA_character_) & !bad
        complain("duplicated label ignoring non-letter characters", bad)
      }
      x <- lapply(x, `[[`, "label")
      complain("missing label", bad <- vapply(x, is.null, NA))
      x[bad] <- NA_character_
      x <- unlist(x, FALSE, FALSE)
      complain("duplicated label", bad <- duplicated.default(x, NA_character_))
      bad <- duplicated.default(x <- tolower(x), NA_character_) & !bad
      complain("duplicated label ignoring case", bad)
      check_1(x, bad)
      x <- gsub("\\W", "", gsub("\\b\\w\\b", "", x, FALSE, TRUE), FALSE, TRUE)
      bad <- duplicated.default(x, NA_character_)
      complain("duplicated label ignoring single-character words", bad)
    }
    check_figure <- function(x) {
      is.fig <- vapply(x, function(this) isTRUE(this$fig), NA)
      bad <- is.fig & vapply(x, function(this) identical(this$eval, FALSE), NA)
      complain("figure but not evaluated", bad)
      bad <- !is.fig & !vapply(x, function(this) is.null(this$width), NA)
      complain("useless 'width' entry", bad)
      bad <- !is.fig & !vapply(x, function(this) is.null(this$height), NA)
      complain("useless 'height' entry", bad)
    }
    check_label(x)
    check_figure(x)
    NULL
  }
  if (is.logical(ignore))
    if (L(ignore))
      ignore <- I(list(pattern = "\\.[RS]?nw$", ignore.case = TRUE))
    else
      ignore <- NULL
  invisible(map_files(pkg_files(x = x, what = what, installed = FALSE,
    ignore = ignore, ...), check_fun, .encoding = encoding))
}

#' @rdname check_R_code
#' @export
#'
logfile <- function(x) UseMethod("logfile")

#' @rdname check_R_code
#' @method logfile NULL
#' @export
#'
logfile.NULL <- function(x) {
  PKGUTILS_OPTIONS$logfile
}

#' @rdname check_R_code
#' @method logfile character
#' @export
#'
logfile.character <- function(x) {
  old <- PKGUTILS_OPTIONS$logfile
  PKGUTILS_OPTIONS$logfile <- L(x[!is.na(x)])
  if (nzchar(x))
    tryCatch(expr = cat(sprintf("\nLOGFILE RESET AT %s\n", date()), file = x,
      append = TRUE), error = function(e) {
        PKGUTILS_OPTIONS$logfile <- old
        stop(e)
      })
  invisible(x)
}


################################################################################


