

################################################################################


#' Create sections
#'
#' The \sQuote{logical} method treats a logical vector by regarding \code{TRUE}
#' as indicating separation. It creates a factor that could be used with
#' \code{split} to split the logical vector, or any equal-length object from
#' which it was created, into according groups. The \sQuote{character} method
#' splits a character vector according to a pattern or to a given output partial
#' string length.
#'
#' @inheritParams pack_desc
#' @param x Logical vector. It is an error if \code{NA} values are contained.
#' @param include Logical scalar indicating whether or not the separator
#'   positions should also be included in the factor levels instead of being
#'   coded as \code{NA}. If \code{include} is \code{NA}, the behaviour is
#'   special; pairs of runs of distinct values are expected (\code{length(x)}
#'   must be an even number), each pair yields a distinct factor level, and the
#'   output does not contain any \code{NA} values.
#' @param pattern Scalar. If of mode \sQuote{character}, passed to
#'   \code{grepl} from the \pkg{base} package. If numeric, used to indicate the
#'   lengths of the partial strings to extract.
#' @param invert Negate the results of \code{grepl}?
#' @param perl Logical scalar passed to \code{grepl}.
#' @return The \sQuote{logical} method returns an ordered factor, its length
#'   being the one of \code{x}. The levels correspond to a groups whose indexes
#'   correspond to the index of a \code{TRUE} value in \code{x} plus the indexes
#'   of the \code{FALSE} values immediately following it. \sQuote{logical}
#'   method returns a list of character vectors.
#' @details When applying \code{split}, positions corresponding to \code{NA}
#'   factor levels will usually be removed. Thus note the action of the
#'   \code{include} argument, and note that the positions of \code{TRUE} values
#'   that are followed by other \code{TRUE} values are always set to \code{NA},
#'   irrespective of \code{include}. The \sQuote{character} method using a
#'   pattern works by passing this pattern to \code{grepl}, the result to the
#'   \sQuote{logical} method and this in turn to \code{split}.
#' @seealso base::split base::grepl
#' @export
#' @family character-functions
#' @keywords utilities character
#' @examples
#'
#' ## 'logical' method
#'
#' # clean input
#' x <- c(TRUE, FALSE, FALSE, TRUE, FALSE, TRUE, FALSE)
#' (y <- sections(x))
#' stopifnot(identical(as.ordered(c(1, 1, 1, 2, 2, 3, 3)), y))
#'
#' # now exclude the separators
#' y <- sections(x, include = FALSE)
#' stopifnot(identical(as.ordered(c(NA, 1, 1, NA, 2, NA, 3)), y))
#'
#' # and now put pairs of runs together
#' y <- sections(x, include = NA)
#' stopifnot(identical(as.ordered(c(1, 1, 1, 2, 2, 3, 3)), y))
#'
#' # leading FALSE
#' x <- c(FALSE, x)
#' (y <- sections(x))
#' stopifnot(identical(as.ordered(c(1, 2, 2, 2, 3, 3, 4, 4)), y))
#' (y <- try(sections(x, include = NA), silent = TRUE))
#' stopifnot(inherits(y, "try-error")) # no pairs throughout => error
#'
#' # adjacent TRUEs and trailing TRUE
#' x <- c(FALSE, TRUE, TRUE, FALSE, TRUE, FALSE, TRUE, TRUE, FALSE, TRUE)
#' (y <- sections(x))
#' stopifnot(identical(as.ordered(c(1, NA, 2, 2, 3, 3, NA, 4, 4, 5)), y))
#' (y <- sections(x, include = NA))
#' stopifnot(identical(as.ordered(c(1, 1, 1, 2, 2, 3, 3, 3, 4, 4)), y))
#'
#' # several adjacent TRUEs
#' x <- c(FALSE, TRUE, TRUE, TRUE, TRUE, FALSE, TRUE, FALSE)
#' (y <- sections(x))
#' stopifnot(identical(as.ordered(c(1, NA, NA, NA, 2, 2, 3, 3)), y))
#' (y <- try(sections(x, include = NA), silent = TRUE))
#' stopifnot(inherits(y, "try-error")) # no pairs throughout => error
#'
#' ## 'character' method
#'
#' # using a specified length
#' x <- c("abcdef", "ghijk")
#' (y <- sections(x, 2))
#' stopifnot(is.list(y), length(y) == 2)
#' stopifnot(y[[1]] == c("ab", "cd", "ef"), y[[2]] == c("gh", "ij", "k"))
#'
#' # using a regexp pattern
#' x <- c(">abc", ">def", "acgtagg", ">hij", "gatattag", "aggtagga") # FASTA
#' (y <- sections(x, "^>", include = TRUE))
#' stopifnot(identical(y, list(`1` = x[2:3], `2` = x[4:6])))
#' (y <- sections(x, "^>", include = FALSE))
#' stopifnot(identical(y, list(`1` = x[3], `2` = x[5:6])))
#'
sections <- function(x, ...) UseMethod("sections")

#' @rdname sections
#' @method sections logical
#' @export
#'
sections.logical <- function(x, include = TRUE, ...) {
  runs <- function(x) {
    unlist(mapply(FUN = rep.int, x = seq_along(x), times = x, SIMPLIFY = FALSE,
      USE.NAMES = FALSE), FALSE, FALSE)
  }
  prepare_sections <- function(x) {
    if (prepend <- !x[1L])
      x <- c(TRUE, x)
    if (append <- x[length(x)])
      x <- c(x, FALSE)
    x <- matrix(cumsum(rle(x)$lengths), ncol = 2L, byrow = TRUE)
    x <- runs(x[, 2L] - x[, 1L] + 1L)
    if (prepend)
      x <- x[-1L]
    if (append)
      x <- x[-length(x)]
    x
  }
  if (!length(x))
    return(structure(.Data = factor(ordered = TRUE), names = names(x)))
  if (anyNA(x))
    stop("'x' must not contain NA values")
  if (is.na(L(include))) {
    result <- rle(x)$lengths
    if (length(result) %% 2L != 0L)
      stop("data do not comprise pairs of runs of distinct values")
    result <- runs(colSums(matrix(result, 2L)))
  } else {
    result <- integer(length(x))
    true.runs <- x & c(x[-1L] == x[-length(x)], FALSE)
    result[!true.runs] <- prepare_sections(x[!true.runs])
    if (include)
      result[true.runs] <- NA_integer_
    else
      result[x] <- NA_integer_
  }
  structure(.Data = as.ordered(result), names = names(x))
}

#' @rdname sections
#' @method sections character
#' @export
#'
sections.character <- function(x, pattern, invert = FALSE, include = TRUE,
    perl = TRUE, ...) {
  if (is.character(pattern)) {
    found <- grepl(pattern = pattern, x = x, perl = perl, ...)
    if (L(invert))
      found <- !found
    split.default(x, sections(found, include))
  } else if (is.numeric(pattern)) {
    if (identical(pattern <- as.integer(pattern), 1L))
      return(strsplit(x, "", TRUE))
    pattern <- sprintf("(.{%i,%i})", pattern, pattern)
    strsplit(gsub(pattern, "\\1\a", x, FALSE, TRUE), "\a", TRUE)
  } else
    stop("'pattern' must be a character or numeric scalar")
}


################################################################################


#' Map files or file names
#'
#' Read lines from a file, modify the lines using a given function, and write
#' the lines back to the input file unless the result of applying the function
#' is identical to the lines read. Alternatively, map (sets of) input file names
#' to (sets of) output file names and check for duplicates.
#'
#' @param x Character vector of input (and potentially output) file names.
#'   Names of directories are not supported.
#' @param mapfun Mapping function, receives character vector with the lines per
#'   file as first argument, with the name of the file added as attribute with
#'   the name given using \code{.attr}.
#' @param ... Optional additional arguments passed to \code{fun} (in the case
#'   of \code{map_files}) or between other methods.
#' @param .attr Character scalar. See description to \code{mapfun}.
#' @param .encoding Passed to \code{readLines} as \sQuote{encoding} argument.
#' @param .sep \code{NULL} or character scalar. If empty, ignored. Otherwise
#'   used as output line separator, causing output files to always be written
#'   unless \code{mapfun} returns \code{NULL}. Can be used to just change line
#'   breaks if \code{mapfun} is \code{identity}.
#' @param .warn Logical scalar passed as \code{warn} argument to
#'   \code{readLines}.
#' @param out.ext Character vector with one to several output file extensions.
#'   Recycled if necessary.
#' @param append Character vector appended after the base name of the input file
#'   name (separated from it with an underscore) but before the output file
#'   extension. Recycled if necessary but ignored where equal the empty string.
#' @param out.dir Character vector with one to several names of output
#'   directories. Recycled if necessary.
#' @param groups Integer scalar indicating the number of input file names to be
#'   assumed in one group. Used in conjunction with the next argument unless
#'   \code{groups} is negative. If so, \code{assort} is ignored and all pairs
#'   of names found in \code{x} are generated.
#' @param assort Character scalar indicating how to assort input file names.
#' \describe{
#'   \item{lst}{All files of the first kind first, then all of the second kind,
#'   etc., sorted increasingly.}
#'   \item{rlst}{All files of the first kind first, then all of the second kind,
#'   etc., sorted decreasingly.}
#'   \item{ext}{Assort according to the file extensions, assume increasing
#'   order.}
#'   \item{rext}{Assort according to the file extensions, assume decreasing
#'   order.}
#'   \item{grp}{Assume one set of file after each other, each sorted
#'   increasingly.}
#'   \item{rgrp}{Assume one set of file after each other, each sorted
#'   decreasingly.}
#' }
#' @param normalize Logical scalar indicating whether \code{normalizePath} from
#'   the \pkg{base} package shall be applied. Eases the recognition of duplicate
#'   file names.
#' @param overwrite Logical scalar. Overwrite already existing files, and do not
#'   care for duplicate names created by cleaning the file names?
#' @param empty.tmpl Character scalar. The template to use for file names that
#'   become empty after cleaning. Should include an integer placeholder to
#'   enable incrementing an index for creating unique file names. (Empty
#'   file names should occur rarely anyway.)
#' @param demo Logical scalar. For \code{clean_filenames}, \code{TRUE} means to
#'   not rename files but just return the usual result indicating the renaming
#'   actions that would be attempted? (Note that this does not indicate whether
#'   the renaming would also by successful.)
#' @return \code{map_files} returns a logical vector using \code{x} as names,
#'   with \code{TRUE} indicating a successfully modified file, \code{FALSE} a
#'   file that yielded no errors but needed not to be modified, and \code{NA} a
#'   file name that caused an error. An attribute \sQuote{errors} is provided,
#'   containing a character vector with error messages (empty strings if no
#'   error occurred).
#'
#'   \code{map_filenames} returns a matrix of mode \code{character}. Each row
#'   contains a set of one to several input file names and its associated set of
#'   one to several output file names constructed from these input file names
#'   and the arguments \code{out.ext}, \code{append} and \code{out.dir}.
#'
#'   \code{clean_filenames} yields a character vector, its names corresponding
#'   to the renamed old files, values corresponding to the novel names, returned
#'   invisibly.
#' @details These function are mainly of use in non-interactive scripts.
#'
#' If \code{mapfun} returns \code{NULL}, it is ignored by \code{map_files}.
#' Otherwise is it an error if \code{mapfun} does not return a character vector.
#' If this vector is identical to the lines read from the file, it is not
#' printed to this file unless \code{.sep} is non-empty. Otherwise the file is
#' attempted to be overwritten with the result of \code{mapfun}.
#'
#' The purpose of \code{map_filenames} is to ease the generation of output file
#' names from input file names and to assort these input file names. This in
#' turn helps converting sets of input file names to sets of output file names.
#' @seealso base::readLines base::writeLines base::identity
#'
#' \code{clean_filenames} modifies file names by removing anything else then
#' word characters, dashes, and dots. Also remove trailing and leading dashes
#' and underscores (per part of a file name, with dots separating these parts)
#' and reduce adjacent dashes and underscores to a single one. Note that
#' directory parts within the file names, if any, are not affected.
#' @family character-functions
#' @export
#' @keywords IO
#' @examples
#'
#' ## map_files
#'
#' tmpfile <- tempfile()
#' write(letters, file = tmpfile)
#' (x <- map_files(tmpfile, identity))
#' stopifnot(!x)
#' # now enforce other output line separator
#' (x <- map_files(tmpfile, identity, .sep = "\n"))
#' stopifnot(x)
#' (x <- map_files(tmpfile, toupper))
#' stopifnot(x)
#' x <- readLines(tmpfile)
#' stopifnot(x == LETTERS)
#' (x <- map_files(tmpfile, as.null))
#' stopifnot(!x)
#'
#' ## clean_filenames
#'
#' # Example with temporary files
#' (x <- tempfile(pattern = "cb& ahi+ si--")) # bad file name
#' write("test", x)
#' stopifnot(file.exists(x))
#' (y <- clean_filenames(x)) # file renamed
#' stopifnot(!file.exists(x), file.exists(y))
#' unlink(y) # tidy up
#'
#' ## map_filenames
#' (x <- map_filenames(letters, out.ext = c("txt", "csv"),
#'   normalize = FALSE))
#' stopifnot(is.matrix(x), dim(x) == c(26, 3))
#' (x <- map_filenames(letters, out.ext = c("txt", "csv"),
#'   out.dir = LETTERS, normalize = FALSE))
#' stopifnot(is.matrix(x), dim(x) == c(26, 3))
#'
#' # Several sets of input files
#' infiles <- paste0(letters, c(".txt", ".csv"))
#' (x <- map_filenames(infiles, "tmp", normalize = FALSE,
#'   groups = 2, assort = "ext"))
#' stopifnot(is.matrix(x), dim(x) == c(13, 3), grepl("csv", x[, 1]),
#'   grepl("txt", x[, 2]))
#'
map_files <- function(x, ...) UseMethod("map_files")

#' @method map_files character
#' @rdname map_files
#' @export
#'
map_files.character <- function(x, mapfun, ..., .attr = ".filename",
    .encoding = "", .sep = NULL, .warn = FALSE) {
  doit <- function(filename) tryCatch({
    add_attr <- function(x) {
      attr(x, .attr) <- filename
      x
    }
    connection <- file(description = filename, encoding = .encoding)
    x <- readLines(con = connection, warn = .warn)
    close(connection)
    if (is.null(y <- mapfun(add_attr(x), ...))) # shortcut
      return(list(FALSE, ""))
    if (optional.output) {
      attributes(y) <- NULL
      if (identical(x, y))
        return(list(FALSE, ""))
    }
    if (!is.character(y))
      stop("applying 'matchfun' did not yield a character vector")
    writeLines(text = y, con = filename, sep = sep)
    list(TRUE, "")
  }, error = function(e) list(NA, conditionMessage(e)))
  case(length(.sep),
    {
      optional.output <- TRUE
      if (grepl("windows", Sys.info()[["sysname"]], TRUE, TRUE))
        sep <- "\r\n"
      else
        sep <- "\n"
    },
    {
      optional.output <- FALSE
      sep <- .sep
    },
    stop("'.sep' must be of length 0 or 1")
  )
  mapfun <- match.fun(mapfun)
  if (!length(x))
    return(structure(.Data = logical(), names = character(),
      errors = character()))
  result <- do.call(rbind, lapply(x, doit))
  structure(.Data = unlist(result[, 1L]), names = x,
    errors = unlist(result[, 2L]))
}

#' @rdname map_files
#' @export
#'
map_filenames <- function(x, ...) UseMethod("map_filenames")

#' @method map_filenames character
#' @rdname map_files
#' @export
#'
map_filenames.character <- function(x, out.ext, append = "", out.dir = ".",
    groups = 1L, assort = c("lst", "rlst", "ext", "rext", "grp", "rgrp"),
    normalize = TRUE, ...) {

  file_ext <- function(x) sub(".*\\.", "", x, FALSE, TRUE)

  assort_files <- function(files, ngrp, how) {
    all_pairs <- function(x, n = length(x)) {
      if (n < 2L)
        stop("cannot create pairs from less than two items")
      n <- seq_len(n)
      n <- do.call(rbind, mapply(FUN = cbind, USE.NAMES = FALSE,
        SIMPLIFY = FALSE, X = n[-1L], Y = lapply(n[-1L] - 1L, seq_len)))
      cbind(x[n[, 2L]], x[n[, 1L]])
    }
    do_split <- function(files, ngrp) {
      cnt <- sort.int(table(ext <- file_ext(files)), NULL, NA, TRUE)
      if (!all(cnt[seq_len(ngrp)] == sum(cnt) * (ngrp - 1L) / ngrp))
        stop("except for one group all file names must have the same extension")
      if (length(cnt) > ngrp) {
        grps <- names(cnt)[seq_len(ngrp)]
        repl <- "_"
        while (repl %in% grps)
          repl <- paste0(repl, repl)
        ext[!ext %in% grps] <- repl
        grps <- c(grps, repl)
      } else {
        grps <- names(cnt)
      }
      do.call(cbind, split.default(files, factor(ext, grps)))
    }
    if (ngrp < 0L)
      return(all_pairs(files))
    if (ngrp == 1L)
      return(cbind(files))
    if (length(files) %% ngrp)
      stop("need number of file names divisible by ", ngrp)
    case(how,
      ext = do_split(files, ngrp),
      rext = do_split(files, ngrp)[, seq.int(ngrp, 1L), drop = FALSE],
      lst = matrix(files, length(files) / ngrp, ngrp, FALSE),
      rlst = matrix(files, length(files) / ngrp, ngrp, FALSE)[,
        seq.int(ngrp, 1L), drop = FALSE],
      grp = matrix(files, length(files) / ngrp, ngrp, TRUE),
      rgrp = matrix(files, length(files) / ngrp, ngrp, TRUE)[,
        seq.int(ngrp, 1L), drop = FALSE]
    )
  }

  prepare_basename <- function(infiles) {
    join <- function(x) apply(X = x, MARGIN = 1L, FUN = paste0, collapse = "_")
    infiles[] <- basename(infiles)
    x <- sub("\\.[^.]*(\\.(gz|xz|bz2|lzma))?$", "", infiles, TRUE, TRUE)
    dim(x) <- dim(infiles)
    x <- x[, !apply(matrix(apply(x, 1L, duplicated.default), ncol(x)), 1L, all),
      drop = FALSE]
    if (!anyDuplicated.default(result <- join(x)))
      return(result)
    for (i in rev.default(seq_len(ncol(x))))
      for (fun in list(file_ext, identity)) {
        x[, i] <- fun(infiles[, i])
        if (!anyDuplicated.default(result <- join(x)))
          return(result)
      }
    stop("file names yield duplicate base names")
  }

  prepare_filename <- function(base, out.ext, append, out.dir) {
    file.path(out.dir, paste0(base, append, ".", out.ext))
  }

  if (!length(x))
    stop("empty 'x' argument")
  if (!length(out.ext))
    stop("empty 'out.ext' argument")
  LL(groups, normalize)
  if (!length(append))
    append <- ""
  if (!length(out.dir))
    out.dir <- ""

  files <- assort_files(x, groups, match.arg(assort))
  colnames(files) <- sprintf("Infile%i", seq.int(ncol(files)))
  ok <- nzchar(out.dir <- rep_len(out.dir, nrow(files)))
  if (normalize) {
    files[] <- normalizePath(files)
    if (any(ok))
      out.dir[ok] <- normalizePath(out.dir[ok])
  }
  if (!all(ok))
    out.dir[!ok] <- dirname(files[!ok, 1L])

  append <- rep_len(append, length(out.ext))
  ok <- nzchar(append)
  append[ok] <- paste0("_", append[ok])
  if (is.null(names(out.ext)))
    names(out.ext) <- out.ext
  if (any(names(out.ext) %in% colnames(files)))
    stop("duplicate column names -- use (other) names of 'out.ext'")

  files <- cbind(files, do.call(cbind, mapply(FUN = prepare_filename,
    MoreArgs = list(base = prepare_basename(files), out.dir = out.dir),
    out.ext = out.ext, append = append, SIMPLIFY = FALSE)))
  ok <- seq.int(1L, ncol(files) - length(out.ext))
  if (any(bad <- files[, ok] %in% files[, -ok]))
    stop("file '", files[, ok][bad][1L], "' and its output file are identical")
  if (anyDuplicated.default(files[, !ok]))
    stop("duplicated output file names")
  files
}

#' @rdname map_files
#' @export
#'
clean_filenames <- function(x, ...) UseMethod("clean_filenames")

#' @method clean_filenames character
#' @rdname map_files
#' @export
#'
clean_filenames.character <- function(x, overwrite = FALSE, demo = FALSE,
    empty.tmpl = "__EMPTY__%05i__", ...) {
  empty.idx <- 0L
  clean_parts <- function(x) {
    x <- gsub("[^\\w-]+", "_", x, FALSE, TRUE)
    x <- gsub("_*-_*", "-", x, FALSE, TRUE)
    x <- gsub("-+", "-", gsub("_+", "_", x, FALSE, TRUE), FALSE, TRUE)
    x <- sub("[_-]+$", "", sub("^[_-]+", "", x, FALSE, TRUE), FALSE, TRUE)
    x <- x[nzchar(x)]
    if (!length(x))
      x <- sprintf(empty.tmpl, empty.idx <<- empty.idx + 1L)
    x
  }
  clean_basenames <- function(x) {
    x <- lapply(strsplit(x, ".", TRUE), clean_parts)
    unlist(lapply(X = x, FUN = paste0, collapse = "."), FALSE, FALSE)
  }
  LL(overwrite, demo, empty.tmpl)
  x <- unique.default(as.character(x))
  if (any(bad <- !nzchar(x))) {
    warning("removing invalid empty file name")
    x <- x[!bad]
  }
  result <- clean_basenames(basename(x))
  result <- ifelse(dirname(x) == ".", result, file.path(dirname(x), result))
  different <- result != x
  result <- structure(.Data = result[different], names = x[different])
  if (!overwrite) {
    result <- result[!duplicated(result)]
    result <- result[!file.exists(result)]
  }
  if (demo)
    message(listing(result, header = "Attempted renamings:"))
  else
    result <- result[file.rename(names(result), result)]
  invisible(result)
}


################################################################################

