#!/usr/local/bin/Rscript --vanilla


################################################################################
#
# merge.R -- R script for merging CSV files. Part of the pkgutils package.
#
# (C) 2012 by Markus Goeker (markus [DOT] goeker [AT] dsmz [DOT] de)
#
# This program is distributed under the terms of the Gnu Public License V2.
# For further information, see http://www.gnu.org/licenses/gpl.html
#
################################################################################


invisible(lapply(c("optparse", "tools", "pkgutils"), library, quietly = TRUE,
  warn.conflicts = FALSE, character.only = TRUE))


COLUMN_DEFAULT_NAME <- "Object"


################################################################################
#
# helper functions
#


do_write <- function(x, options) {
  write.table(x, sep = options$separator, row.names = FALSE,
    quote = !options$unquoted)
}


do_read <- function(file, options) {
  read.delim(file, sep = options$separator, check.names = options$names,
    strip.white = !options$keep, header = !options$bald,
    stringsAsFactors = FALSE, fileEncoding = options$encoding)
}


read_and_create_unique_names <- function(files, options) {
  data <- lapply(X = files, FUN = do_read, options = options)
  suffixes <- file_path_sans_ext(basename(files), compression = TRUE)
  cn <- lapply(data, colnames)
  ok <- rep(list(options$ycolumn), length(data))
  ok[[1L]] <- options$xcolumn
  for (i in seq_along(cn)) {
    if (any(bad <- !nzchar(cn[[i]]))) # merge() crashes with empty column names
      colnames(data[[i]])[bad] <- sprintf("%s.%i", suffixes[i], 
        seq_along(cn[[i]])[bad])
    twice <- cn[[i]] %in% setdiff(unlist(cn[-i]), ok[[i]])
    colnames(data[[i]])[twice] <- sprintf("%s.%s", cn[[i]][twice], suffixes[i])
  }
  data
}


do_split <- function(x) {
  x <- unlist(strsplit(x, ",", fixed = TRUE))
  x[nzchar(x)]
}


join_unique <- function(x, join) {
  paste(unique.default(x[nzchar(x)]), collapse = join)
}


to_numbered_header <- function(x) {
  x[x == COLUMN_DEFAULT_NAME] <- "1"
  x <- sub("^\\s*V", "", x, perl = TRUE, ignore.case = TRUE)
  sprintf("V%i", must(as.integer(x)))
}


################################################################################
#
# option processing
#


option.parser <- OptionParser(option_list = list(

  make_option(c("-a", "--all"), action = "store_true",
    help = "Keep non-matching lines of file 2, too [default: %default]", 
    default = FALSE),

  make_option(c("-b", "--bald"), action = "store_true",
    help = "Assume files have no headers [default: %default]",
    default = FALSE),

  make_option(c("-c", "--conserve"), action = "store_true",
    help = "Conserve input column order, do not sort [default: %default]",
    default = FALSE),

  make_option(c("-d", "--delete"), action = "store_true",
    help = "Delete non-matching lines of file 1 [default: %default]", 
    default = FALSE),

  make_option(c("-e", "--encoding"), type = "character",
    help = "Encoding to be assumed in input files [default: '%default']",
    metavar = "NAME", default = ""),

  make_option(c("-j", "--join-by"), type = "character",
    help = "Join character(s) for vertical merging mode [default: '%default']",
    metavar = "SEP", default = "; "),

  make_option(c("-k", "--keep"), action = "store_true",
    help = "Keep whitespace surrounding the separators [default: %default]", 
    default = FALSE),

  make_option(c("-n", "--names"), action = "store_true",
    help = "Convert column names to syntactical names [default: %default]",
    default = FALSE),

  make_option(c("-o", "--onename"), action = "store_true",
    help = paste("Do not split arguments of '-x' and '-y' at ','",
      "[default: %default]"), default = FALSE),

  make_option(c("-s", "--separator"), type = "character",
    help = "Field separator in CSV files [default: '%default']",
    metavar = "SEP", default = "\t"),

  make_option(c("-u", "--unquoted"), action = "store_true",
    help = "Do not quote fields in output [default: %default]",
    default = FALSE),

  make_option(c("-v", "--vertical"), action = "store_true",
    help = "Merge vertically, file by file [default: %default]",
    default = FALSE),

  make_option(c("-x", "--xcolumn"), type = "character",
    help = "Name of the merge column in file 1 [default: '%default']",
    default = COLUMN_DEFAULT_NAME, metavar = "COLUMN"),

  make_option(c("-y", "--ycolumn"), type = "character", 
    help = "Name of the merge column in file 2 [default: like file 1]",
    default = "", metavar = "COLUMN")

), usage = "%prog [options] csv_file_1 csv_file_2 ...", prog = "merge.R")


opt <- parse_args(option.parser, positional_arguments = TRUE)
files <- opt$args
opt <- opt$options


################################################################################
#
# special treatment of column-name options
#


if (opt$bald)
  opt$onename <- FALSE
if (!opt$onename)
  opt$xcolumn <- do_split(opt$xcolumn)
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


if (opt$help || (length(files) + opt$vertical) < 2L) {
  print_help(option.parser)
  quit(status = 1L)
}


################################################################################
#
# vertical merge mode
#


if (opt$vertical) {
  for (file in files) {
    x <- aggregate(do_read(file, opt), by = list(x[[opt$xcolumn]]),
      FUN = join_unique, join = opt$join, simplify = TRUE)
    do_write(x[, -1L, drop = FALSE], opt)
  }
  quit(status = 0L)
}


################################################################################
#
# horizontal merge mode (= default)
#


data <- read_and_create_unique_names(files, opt)

x <- data[[1L]]
for (i in seq_along(data)[-1L])
  x <- merge(x, data[[i]], by.x = opt$xcolumn, by.y = opt$ycolumn, 
    all.x = !opt$delete, all.y = opt$all, sort = !opt$conserve)

if (opt$conserve) {
  previous <- data[[1L]][, opt$xcolumn]
  if (setequal(previous, x[, opt$xcolumn])) {
    rownames(x) <- x[, opt$xcolumn]
    x <- x[previous, , drop = FALSE]
    rownames(x) <- NULL
  }
}

do_write(x, opt)


################################################################################

