sections <- function(x, ...) UseMethod("sections")

sections.logical <- function(x, include = TRUE, ...) {
  prepare_sections <- function(x) {
    if (prepend <- !x[1L])
      x <- c(TRUE, x)
    if (append <- x[length(x)])
      x <- c(x, FALSE)
    x <- matrix(cumsum(rle(x)$lengths), ncol = 2L, byrow = TRUE)
    x <- x[, 2L] - x[, 1L] + 1L
    x <- mapply(rep.int, seq_along(x), x, SIMPLIFY = FALSE, USE.NAMES = FALSE)
    x <- unlist(x)
    if (prepend)
      x <- x[-1L]
    if (append)
      x <- x[-length(x)]
    x
  }
  if (!length(x))
    return(structure(factor(ordered = TRUE), names = names(x)))
  if (anyNA(x))
    stop("'x' must not contain NA values")
  result <- integer(length(x))
  true.runs <- x & c(x[-1L] == x[-length(x)], FALSE)
  result[!true.runs] <- prepare_sections(x[!true.runs])
  if (L(include))
    result[true.runs] <- NA_integer_
  else
    result[x] <- NA_integer_
  structure(as.ordered(result), names = names(x))
}

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

map_files <- function(x, ...) UseMethod("map_files")

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
    return(structure(logical(), names = character(), errors = character()))
  result <- do.call(rbind, lapply(x, doit))
  structure(unlist(result[, 1L]), names = x, errors = unlist(result[, 2L]))
}

map_filenames <- function(x, ...) UseMethod("map_filenames")

map_filenames.character <- function(x, out.ext, append = "", out.dir = ".",
    groups = 1L, assort = c("lst", "rlst", "ext", "rext", "grp", "rgrp"),
    normalize = TRUE, ...) {

  file_ext <- function(x) sub(".*\\.", "", x, FALSE, TRUE)

  assort_files <- function(files, ngrp, how) {
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
    infiles <- basename(infiles)
    x <- sub("\\.[^.]*(\\.(gz|xz|bz2|lzma))?$", "", infiles, TRUE, TRUE)
    if (any(duplicated.default(x)[-1L]))
      if (any(duplicated.default(x <- file_ext(infiles))[-1L]))
        if (any(duplicated.default(x <- infiles)[-1L]))
          stop("duplicate file names")
    x
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
    out.dir[!ok] <- dirname(x[!ok, 1L])

  append <- rep_len(append, length(out.ext))
  ok <- nzchar(append)
  append[ok] <- paste0("_", append[ok])
  if (is.null(names(out.ext)))
    names(out.ext) <- out.ext
  names(out.ext) <- toupper(names(out.ext))
  if (any(names(out.ext) %in% colnames(files)))
    stop("duplicate column names -- use (other) names of 'out.ext'")

  files <- cbind(files, do.call(cbind, mapply(FUN = prepare_filename,
    MoreArgs = list(base = prepare_basename(files[, 1L]), out.dir = out.dir),
    out.ext = out.ext, append = append, SIMPLIFY = FALSE)))
  ok <- seq.int(1L, ncol(files) - length(out.ext))
  if (any(bad <- files[, ok] %in% files[, -ok]))
    stop("file '", files[, ok][bad][1L], "' and its output file are identical")
  if (anyDuplicated.default(files[, !ok]))
    stop("duplicated output file names")
  files
}

