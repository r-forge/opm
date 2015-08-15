#!/usr/local/bin/Rscript --vanilla


################################################################################
#
# merge.R -- R script for merging CSV files. Part of the pkgutils package.
#
# (C) 2013 by Markus Goeker (markus [DOT] goeker [AT] dsmz [DOT] de)
#
# This program is distributed under the terms of the Gnu Public License V2.
# For further information, see http://www.gnu.org/licenses/gpl.html
#
################################################################################


COLUMN_DEFAULT_NAME <- "Object"


################################################################################
#
# helper functions
#


do_write <- function(x, opt) {
  write.table(x, sep = opt$separator, row.names = FALSE, na = opt$prune,
    quote = !opt$unquoted, col.names = !opt$bald || opt$`make-header`)
}


do_read <- function(infile, opt) {
  if (infile == "-")
    infile <- file("stdin")
  read.delim(infile, sep = opt$separator, check.names = opt$names,
    strip.white = !opt$keep, header = !opt$bald, na.strings = opt$prune,
    stringsAsFactors = FALSE, fileEncoding = opt$encoding)
}


truncate <- function(files) {
  sub("\\.[^.]+(\\.(gz|xz|bz2|lzma))?$", "", basename(files), TRUE, TRUE)
}


trunc_zeros <- function(x) {
  gsub("(?<!\\d)0+(?=\\d)", "", x, FALSE, TRUE)
}


read_and_create_unique_column_names <- function(files, options) {
  data <- lapply(X = files, FUN = do_read, opt = options)
  change.first <- !options$first
  suffixes <- if (options$indices)
      seq_along(files)
    else
      truncate(files)
  cn <- lapply(data, colnames)
  ok <- rep(list(options$ycolumn), length(data))
  ok[[1L]] <- options$xcolumn
  for (i in seq_along(cn)) {
    if (any(bad <- !nzchar(cn[[i]]))) # merge() crashes with empty column names
      colnames(data[[i]])[bad] <- sprintf("%s.%i", suffixes[i],
        seq_along(cn[[i]])[bad])
    twice <- cn[[i]] %in% setdiff(unlist(cn[-i]), ok[[i]])
    if (change.first || i != 1L)
      colnames(data[[i]])[twice] <- sprintf("%s.%s", cn[[i]][twice],
        suffixes[i])
  }
  data
}


do_split <- function(x) {
  x <- unlist(strsplit(x, ",", TRUE), FALSE, FALSE)
  x[nzchar(x)]
}


make_unique <- function(x) {
  make.unique(as.character(x), " #")
}


join_unique <- function(x, join) {
  x <- unlist(strsplit(as.character(x), join, TRUE), FALSE, FALSE)
  paste0(unique.default(x[nzchar(x) & !is.na(x)]), collapse = join)
}


join_most_frequent <- function(x, join) {
  best <- table(unlist(strsplit(as.character(x), join, TRUE), FALSE, FALSE))
  paste0(names(best[best == max(best)]), collapse = join)
}


to_numbered_header <- function(x) {
  x[x == COLUMN_DEFAULT_NAME] <- "1"
  x <- sub("^\\s*V", "", x, TRUE, TRUE)
  sprintf("V%i", tryCatch(as.integer(x), warning = stop))
}


fill_randomly <- function(x, empty = TRUE) {
  if (is.list(x)) { # works also for data frames
    x[] <- rapply(x, fill_randomly, "ANY", NULL, "replace", empty)
    return(x)
  }
  isna <- is.na(x)
  if (empty && is.character(x))
    isna <- isna | !nzchar(x)
  if (all(isna)) {
    warning("cannot fill in values because all are missing")
    return(x)
  }
  x[isna] <- sample(x[!isna], sum(isna), TRUE)
  x
}


fill_downwards <- function(x, ...) {
  fill_column <- function(x) {
    do_fill <- function(x) rep.int(x[[1L]], length(x))
    f <- !is.na(x)
    if (is.character(x))
      f <- f & nzchar(x)
    f <- pkgutils::sections(f, TRUE)
    x[!is.na(f)] <- unlist(lapply(split.default(x, f), do_fill), FALSE, FALSE)
    x
  }
  x[] <- rapply(x, fill_column, "ANY", NULL, "replace")
  x
}


unnest <- function(x, col, sep = "; ", fixed = TRUE) {
  result <- lapply(x[, col, drop = FALSE], strsplit, sep, fixed, !fixed)
  result <- as.data.frame(do.call(cbind, result))
  result <- cbind(x[, setdiff(colnames(x), col), drop = FALSE], result)
  col <- colnames(result)
  args <- list(check.names = FALSE, stringsAsFactors = FALSE)
  result <- lapply(seq.int(nrow(result)),
    function(i) do.call(data.frame, c(result[i, , drop = FALSE], args)))
  for (i in seq_along(result))
    colnames(result[[i]]) <- col
  do.call(rbind, result)
}


process_specially <- function(files, opt) {
  merge_horizontally <- function(x, opt) {
    x <- apply(x, 1L, function(x) pkgutils::listing(x[nzchar(x)],
      style = "%s: %s", collapse = opt$join))
    matrix(x, length(x), 1L, FALSE, list(NULL, COLUMN_DEFAULT_NAME))
  }
  merge_vertically <- function(x, opt) {
    aggregate(x = x[, -match(opt$xcolumn, colnames(x), 0L), drop = FALSE],
      by = x[, opt$xcolumn, drop = FALSE], FUN = if (opt$good)
        join_most_frequent
      else
        join_unique, join = opt$join, simplify = TRUE)
  }
  if (opt$rows)
    do_convert <- merge_horizontally
  else if (opt$vertical)
    do_convert <- merge_vertically
  else if (opt$load)
    do_convert <- function(x, opt) fill_randomly(x, opt$all)
  else if (opt$zack)
    do_convert <- fill_downwards
  else if (opt$widen)
    do_convert <- function(x, opt) unnest(x, opt$xcolumn, opt$join)
  else
    stop("invalid combination of options")
  for (file in files)
    do_write(do_convert(do_read(file, opt), opt), opt)
}


# This is slow if 'x' contains strings that have no close match in 'y' (and no
# 'cutoff' is set) but otherwise converges quickly.
#
assort_strings <- function(x, y, cutoff = Inf, ...) {
  result <- adist(x, y, ...)
  result <- data.frame(X = rep.int(seq_along(x), length(y)),
    Y = rep(seq_along(y), each = length(x)), D = c(result), W = FALSE)
  if (is.finite(cutoff))
    result <- result[result[, 3L] < cutoff, , drop = FALSE]
  result <- result[sort.list(result[, 3L]), , drop = FALSE]
  have.x <- logical(length(x))
  have.y <- logical(length(y))
  for (i in seq_len(nrow(result))) {
    if (have.x[[result[i, 1L]]] || have.y[[result[i, 2L]]])
      next
    result[i, 4L] <- have.x[[result[i, 1L]]] <- have.y[[result[i, 2L]]] <- TRUE
    if (all(have.x) || all(have.y))
      break
  }
  result <- result[result[, 4L], -4L, drop = FALSE]
  result[, 1L] <- x[result[, 1L]]
  result[, 2L] <- y[result[, 2L]]
  if (length(x <- setdiff(x, result[, 1L])))
    result <- rbind(result, data.frame(X = x, Y = NA_character_, D = NA_real_,
      stringsAsFactors = FALSE))
  if (length(y <- setdiff(y, result[, 2L])))
    result <- rbind(result, data.frame(X = NA_character_, Y = y, D = NA_real_,
      stringsAsFactors = FALSE))
  result
}


# Replace the first of the merge columns by one that contains the approximate
# matches to the first merge column in the other data frame. Retain the original
# merge columns as well as the edit distance between the strings.
#
include_approximate_matches <- function(x, y, options, idx) {
  ycol <- options$ycolumn[1L]
  m <- assort_strings(x[, options$xcolumn[1L]], y[, ycol], options$threshold)
  m <- m[match(y[, ycol], m$Y), , drop = FALSE]
  ncol <- paste(ycol, c("ORIG", "DIST"), options$threshold, idx, sep = "_")
  colnames(y)[colnames(y) == ycol] <- ncol[1L]
  y[, c(ycol, ncol[2L])] <- m[, c("X", "D")]
  if (options$all && any(isna <- is.na(y[, ycol])))
    y[isna, ycol] <- y[isna, ncol[1L]]
  y
}


################################################################################
#
# option processing
#


option.parser <- optparse::OptionParser(option_list = list(

  optparse::make_option(c("-a", "--all"), action = "store_true",
    help = "Keep non-matching lines of file 2, too [default: %default]",
    default = FALSE),

  optparse::make_option(c("-b", "--bald"), action = "store_true",
    help = "Assume files have no headers [default: %default]",
    default = FALSE),

  optparse::make_option(c("-c", "--conserve"), action = "store_true",
    help = "Conserve input column order, do not sort [default: %default]",
    default = FALSE),

  optparse::make_option(c("-d", "--delete"), action = "store_true",
    help = "Delete non-matching lines of file 1 [default: %default]",
    default = FALSE),

  optparse::make_option(c("-e", "--encoding"), type = "character",
    help = "Encoding to be assumed in input files [default: '%default']",
    metavar = "NAME", default = ""),

  optparse::make_option(c("-f", "--first"), action = "store_true",
    help = "Do not adapt column names of file 1 [default: %default]",
    default = FALSE),

  optparse::make_option(c("-G", "--good"), action = "store_true",
    help = paste0("Use case-insensitive matching; with -v, keep only most ",
      "frequent entries [default: %default]"),
    default = FALSE),

  optparse::make_option(c("-h", "--help"), action = "store_true",
    help = "Print help message and exit [default: %default]",
    default = FALSE),

  optparse::make_option(c("-i", "--indices"), action = "store_true",
    help = "Use indices for adapting column names [default: %default]",
    default = FALSE),

  optparse::make_option(c("-j", "--join-by"), type = "character",
    help = "Join character(s) for -r/-v [default: '%default']",
    metavar = "SEP", default = "; "),

  optparse::make_option(c("-k", "--keep"), action = "store_true",
    help = "Keep whitespace surrounding the separators [default: %default]",
    default = FALSE),

  optparse::make_option(c("-l", "--load"), action = "store_true",
    help = "Randomly replace missing by present values [default: %default]",
    default = FALSE),

  optparse::make_option(c("-m", "--make-header"), action = "store_true",
    help = "Output headers even for input without headers [default: %default]",
    default = FALSE),

  optparse::make_option(c("-n", "--names"), action = "store_true",
    help = "Convert column names to syntactical names [default: %default]",
    default = FALSE),

  optparse::make_option(c("-o", "--onename"), action = "store_true",
    help = paste("Do not split arguments of '-x' and '-y' at ','",
      "[default: %default]"), default = FALSE),

  optparse::make_option(c("-p", "--prune"), type = "character",
    help = "Value to prune by treating as NA [default: %default]",
    default = "NA", metavar = "STR"),

  optparse::make_option(c("-q", "--unique"), action = "store_true",
    help = "Make entries in join column unique [default: %default]",
    default = FALSE),

  optparse::make_option(c("-r", "--rows"), action = "store_true",
    help = "Merge each row horizontally, file by file [default: %default]",
    default = FALSE),

  optparse::make_option(c("-s", "--separator"), type = "character",
    help = "Field separator in CSV files [default: '%default']",
    metavar = "SEP", default = "\t"),

  optparse::make_option(c("-t", "--threshold"), type = "numeric",
    help = "Threshold for error-tolerant matching [default: %default]",
    metavar = "NUM", default = -1),

  optparse::make_option(c("-u", "--unquoted"), action = "store_true",
    help = "Do not quote fields in output [default: %default]",
    default = FALSE),

  optparse::make_option(c("-v", "--vertical"), action = "store_true",
    help = "Merge vertically, file by file [default: %default]",
    default = FALSE),

  optparse::make_option(c("-w", "--widen"), action = "store_true",
    help = "Widen (unnest) selected column(s) [default: %default]",
    default = FALSE),

  optparse::make_option(c("-x", "--xcolumn"), type = "character",
    help = "Name of the merge column(s) in file 1 [default: '%default']",
    default = COLUMN_DEFAULT_NAME, metavar = "COLUMNS"),

  optparse::make_option(c("-y", "--ycolumn"), type = "character",
    help = "Name of the merge column(s) in file 2 [default: like file 1]",
    default = "", metavar = "COLUMNS"),

  optparse::make_option(c("-z", "--zack"), action = "store_true",
    help = "Fill (sack) column(s) downwards [default: %default]",
    default = FALSE)

), usage = "%prog [options] csv_file_1 csv_file_2 ...", prog = "merge.R",
  add_help_option = FALSE, description = paste("\nMerge CSV files",
    "rows or columns in such files", "or fill rows randomly.", sep = ", "),
  epilogue = paste0(
    "In default mode, providing less than two files makes not much sense.\n")
)


opt <- optparse::parse_args(option.parser, positional_arguments = TRUE)
files <- opt$args
opt <- opt$options


################################################################################
#
# special treatment of column-name options
#


if (opt$bald) {
  opt$onename <- FALSE
}
if (!opt$onename) {
  opt$xcolumn <- do_split(opt$xcolumn)
}
if (!nzchar(opt$ycolumn)) {
  opt$ycolumn <- opt$xcolumn
} else if (!opt$onename) {
  opt$ycolumn <- do_split(opt$ycolumn)
}
if (opt$bald) {
  opt$xcolumn <- to_numbered_header(opt$xcolumn)
  opt$ycolumn <- to_numbered_header(opt$ycolumn)
}


################################################################################
#
# help message if not enough file names are provided
#


if (opt$help || !length(files)) {
  optparse::print_help(option.parser)
  quit(status = 1L)
}


################################################################################
#
# horizontal merging of rows, vertical merging, or random filling
#

if (opt$vertical || opt$rows || opt$load || opt$widen || opt$zack) {
  process_specially(files, opt)
  quit(status = 0L)
}


################################################################################
#
# horizontal merging of files (= default)
#


data <- read_and_create_unique_column_names(files, opt)

if (opt$good) {
  data[[1L]][, opt$xcolumn] <- toupper(trunc_zeros(data[[1L]][, opt$xcolumn]))
  for (i in seq_along(data)[-1L])
    data[[i]][, opt$ycolumn] <- toupper(trunc_zeros(data[[i]][, opt$ycolumn]))
}

if (opt$unique) {
  data[[1L]][, opt$xcolumn] <- make_unique(data[[1L]][, opt$xcolumn])
  for (i in seq_along(data)[-1L])
    data[[i]][, opt$ycolumn] <- make_unique(data[[i]][, opt$ycolumn])
}

x <- data[[1L]]
for (i in seq_along(data)[-1L]) {
  if (opt$threshold >= 0)
    data[[i]] <- include_approximate_matches(x, data[[i]], opt, i)
  x <- merge(x, data[[i]], by.x = opt$xcolumn, by.y = opt$ycolumn,
    all.x = !opt$delete, all.y = opt$all, sort = !opt$conserve)
}

if (opt$conserve) {
  found <- match(previous <- data[[1L]][, opt$xcolumn], x[, opt$xcolumn], 0L)
  if (all(found > 0L) && all(x[, opt$xcolumn] %in% previous)) {
    # appending the row index avoids duplicate row names
    rownames(x) <- paste(x[, opt$xcolumn], seq.int(nrow(x)))
    x <- x[paste(previous, found), , drop = FALSE]
    rownames(x) <- NULL
  }
}

do_write(x, opt)


################################################################################


