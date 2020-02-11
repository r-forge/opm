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

INSERTED_COLUMNS <- character()

options(warn = 1L)


################################################################################
#
# helper functions
#


do_write <- function(x, opt, file = "") {
  if (is.data.frame(x) || is.matrix(x))
    write.table(x = x, file = file, sep = opt$`output-separator`,
      row.names = FALSE, na = opt$prune, quote = !opt$unquoted,
      col.names = !opt$bald || opt$`make-header`)
  else if (is.list(x) && !is.null(names(x)) &&
      all(vapply(x, is.data.frame, NA)))
    mapply(FUN = do_write, x = x, file = names(x), MoreArgs = list(opt = opt),
      SIMPLIFY = FALSE, USE.NAMES = FALSE)
  else if (is.character(x))
    if (opt$unquoted)
      write(x = x, file = file)
    else
      write(x = yaml::as.yaml(x), file = file)
  else
    write(x = yaml::as.yaml(x), file = file)
}


do_read <- function(infile, opt) {
  if (infile == "-")
    infile <- file("stdin")
  x <- read.table(file = infile, sep = opt$separator, check.names = opt$names,
    strip.white = !opt$keep, header = !opt$bald, na.strings = opt$prune,
    stringsAsFactors = FALSE, fileEncoding = opt$encoding, fill = TRUE,
    comment.char = "", quote = opt$quote)
  if (any(dup <- duplicated.data.frame(x))) {
    warning("removing ", sum(dup), " duplicate row(s)")
    x <- x[!dup, , drop = FALSE]
  }
  x
}


truncate <- function(files) {
  sub("\\.[^.]+(\\.(gz|xz|bz2|lzma))?$", "", basename(files), TRUE, TRUE)
}


trunc_zeros <- function(x) {
  gsub("(?<!\\d)0+(?=\\d)", "", x, FALSE, TRUE)
}


output_extension <- function(options) {
  switch(options$separator, `;` = "ssv", `\t` = "tsv", "csv")
}


read_sheets <- function(file, header, na.strings) {
  read_ods <- function(file, header, na.strings) {
    sheets <- readODS::ods_sheets(file)
    sapply(X = sheets, FUN = readODS::read_ods, path = file, na = na.strings,
      col_names = header, formula_as_formula = FALSE, simplify = FALSE)
  }
  read_xls <- function(file, header) {
    wb <- XLConnect::loadWorkbook(file)
    sheets <- XLConnect::getSheets(wb)
    sapply(X = sheets, FUN = XLConnect::readWorksheet, object = wb,
      header = header, check.names = FALSE, readStrategy = "fast",
      autofitRow = TRUE, autofitCol = TRUE, simplify = FALSE)
  }
  switch(tolower(ext <- tools::file_ext(file)),
    ods = read_ods(file, header, na.strings),
    xls =,
    xlsx = read_xls(file, header),
    stop(sprintf("file extension \"%s\" not recognized", ext))
  )
}


process_spreadsheets <- function(files, options) {
  old.java.opt <- options(java.parameters = "-Xmx4g")
  on.exit(options(old.java.opt))
  files <- pkgutils::map_filenames(files, output_extension(options), "SHEET_%s")
  for (i in seq_len(nrow(files))) {
    x <- read_sheets(files[i, 1L], !options$bald, options$prune)
    names(x) <- chartr(".", "_", make.names(names(x), TRUE))
    mapply(FUN = do_write, x = x, file = sprintf(files[i, 2L], names(x)),
      MoreArgs = list(opt = options), SIMPLIFY = FALSE, USE.NAMES = FALSE)
  }
  files
}


read_and_create_unique_column_names <- function(files, options) {

  join <- function(x, y) sprintf("%s.%s", x, y)

  data <- lapply(X = files, FUN = do_read, opt = options)

  suffix <- if (options$indices)
      seq_along(files)
    else
      truncate(files)
  keys <- lapply(data, names) # needed for a global check, see below
  selection <- rep(list(options$ycolumn), length(data))
  selection[[1L]] <- options$xcolumn

  if (nzchar(options$insert)) {
    insert <- vapply(data, ncol, 0L) <= lengths(selection, FALSE)
    insert <- ifelse(insert, join(options$insert, suffix), "")
  } else {
    insert <- character(length(data))
  }

  for (i in seq_along(data)) {
    # insert presence column if only selection columns are there
    if (nzchar(insert[[i]])) {
      insert[[i]] <- tail(make.unique(c(keys[[i]], insert[[i]])), 1L)
      data[[i]][, insert[[i]]] <- rep.int(TRUE, nrow(data[[i]]))
      keys[[i]] <- colnames(data[[i]])
    }
    # merge() would crash with empty column names, hence we repair them here
    if (any(bad <- !nzchar(keys[[i]])))
      keys[[i]][bad] <- join(suffix[[i]], seq_along(keys[[i]])[bad])
    # if a column name is duplicated but not among the selection columns
    other <- setdiff(unlist(keys[-i], FALSE, FALSE), selection[[i]])
    if (any(twice <- keys[[i]] %in% other) && (!options$first || i != 1L)) {
      keys[[i]][twice] <- join(keys[[i]][twice], suffix[[i]])
      if (nzchar(insert[[i]]) && insert[[i]] %in% other)
        insert[[i]] <- join(insert[[i]], suffix[[i]])
    }
    names(data[[i]]) <- keys[[i]]
  }

  INSERTED_COLUMNS <<- c(INSERTED_COLUMNS, insert[nzchar(insert)])

  data
}


do_split <- function(x) {
  x <- unlist(strsplit(x, ",", TRUE), FALSE, FALSE)
  x[nzchar(x)]
}


make_combination_unique <- function(x, wanted, remove, sep = "\a") {
  y <- do.call(paste, c(x[, wanted, drop = FALSE], list(sep = sep)))
  if (remove) {
    x <- x[!duplicated.default(y), , drop = FALSE]
  } else {
    y <- do.call(rbind, strsplit(make.unique(y, " #"), sep, TRUE))
    if (ncol(y) != length(wanted))
      stop("unexpected number of columns -- other separator needed")
    x[, wanted[[length(wanted)]]] <- y[, ncol(y)]
  }
  x
}


make_unique <- function(x) {
  make.unique(as.character(x), " #")
}


join_unique <- function(x, collapse) {
  x <- unlist(strsplit(as.character(x), collapse, TRUE), FALSE, FALSE)
  paste0(unique.default(x[nzchar(x) & !is.na(x)]), collapse = collapse)
}


join_most_frequent <- function(x, collapse) {
  best <- table(unlist(strsplit(as.character(x), collapse, TRUE), FALSE, FALSE))
  paste0(names(best[best == max(best)]), collapse = collapse)
}


to_numbered_header <- function(x) {
  x[x == COLUMN_DEFAULT_NAME] <- "1"
  x <- sub("^\\s*V", "", x, TRUE, TRUE)
  sprintf("V%i", tryCatch(expr = as.integer(x), warning = stop))
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


narrow <- function(x, opt) {
  get_cols <- function(col1, col2, x, insert) {
    x <- x[, c(col2, col1), drop = FALSE]
    colnames(x)[ncol(x)] <- COLUMN_DEFAULT_NAME
    x[, insert] <- col1
    x
  }
  xcol <- opt$xcolumn
  inam <- opt$insert
  do.call(rbind, lapply(setdiff(colnames(x), xcol), get_cols, xcol, x, inam))
}


compact_table <- function(x, opt) {
  empty <- function(x) if (is.character(x)) is.na(x) | !nzchar(x) else is.na(x)
  space <- function(x) {
    x <- sub("^\\s+", "", x, FALSE, TRUE)
    x <- sub("\\s+$", "", x, FALSE, TRUE)
    gsub("\\s{2,}", " ", x, FALSE, TRUE)
  }
  problem <- do.call(cbind, lapply(x, empty))
  N <- ncol(x)
  groups <- !apply(problem, 1L, any)
  groups <- split(seq_len(nrow(x)), pkgutils::sections(groups, TRUE))
  for (group in groups) {
    good <- group[[1L]]
    bad <- group[-1L]
    for (column in seq_len(N)) {
      these <- x[bad, column][!problem[bad, column]]
      if (length(these))
        x[good, column] <- space(paste(c(x[good, column], these),
          collapse = " "))
    }
  }
  groups <- do.call(c, lapply(groups, `[`, -1L))
  x <- x[-groups, , drop = FALSE]
  if (opt$vertical) {
    tmp <- x[, 1L]
    if (ncol(x) > 1L)
      tmp <- sprintf("%s: %s", tmp,
        do.call(paste, args = x[, -1L, drop = FALSE]))
    x <- paste0(tmp, collapse = "; ")
    if (!grepl("\\.$", x, FALSE, TRUE))
      x <- paste0(x, ".")
  }
  x
}


to_yaml <- function(files, opt) {
  dataframe_to_map <- function(x, from) {
    to_map <- function(x, from) {
      if (any(dup <- duplicated.default(from))) {
        warning("duplicates in column selected as map keys, data will be lost")
        x <- x[!dup, , drop = FALSE]
        from <- from[!dup]
      }
      lapply(split.data.frame(x, from), as.list)
    }
    pkgutils::case(length(from),
      to_map(x, rownames(x)),
      to_map(x[, setdiff(names(x), from), drop = FALSE], x[, from]),
      stop("length of 'from' argument must be 0 or 1")
    )
  }
  for (i in seq_along(files))
    write(yaml::as.yaml(dataframe_to_map(do_read(files[[i]], opt),
      opt$xcolumn)), "")
}


process_specially <- function(files, opt) {
  merge_horizontally <- function(x, opt) {
    if (opt$good) {
      for (i in seq_along(opt$xcolumn)[-1L]) {
        bad <- is.na(x[, opt$xcolumn[[1L]]])
        x[bad, opt$xcolumn[[1L]]] <- x[bad, opt$xcolumn[[i]]]
        x[, opt$xcolumn[[i]]] <- NULL
      }
      x
    } else {
      x <- apply(x, 1L, function(x) pkgutils::listing(x = x[nzchar(x)],
        style = "%s: %s", collapse = opt$join))
      matrix(x, length(x), 1L, FALSE, list(NULL, COLUMN_DEFAULT_NAME))
    }
  }
  merge_vertically <- function(x, opt) {
    aggregate(x = x[, -match(opt$xcolumn, colnames(x), 0L), drop = FALSE],
      by = x[, opt$xcolumn, drop = FALSE], FUN = if (opt$good)
        join_most_frequent
      else if (opt$all)
        paste0
      else
        join_unique, collapse = opt$join, simplify = TRUE)
  }
  if (opt$duplicates) {
    do_convert <- function(x, opt) make_combination_unique(x, opt$xcolumn,
      opt$good)
  } else if (opt$compact) {
    do_convert <- compact_table
  } else if (opt$rows) {
    do_convert <- merge_horizontally
  } else if (opt$vertical) {
    do_convert <- merge_vertically
  } else if (opt$load) {
    do_convert <- function(x, opt) fill_randomly(x, opt$all)
  } else if (opt$zack) {
    do_convert <- fill_downwards
  } else if (opt$widen) {
    do_convert <- if (opt$all)
        narrow
      else
        function(x, opt) pkgutils::unnest(object = x, sep = opt$join,
          col = opt$xcolumn)
  } else if (nzchar(opt$`map-table`)) {
    opt$`map-table` <- if (opt$`map-table` == "_")
        list()
      else
        yaml::yaml.load_file(opt$`map-table`)
    do_convert <- function(x, opt) pkgutils::map_values(x, opt$`map-table`)
  } else if (opt$fission) {
    do_convert <- function(x, opt) {
      x <- split.data.frame(x, x[, opt$xcolumn])
      names(x) <- chartr(".", "_", make.names(names(x), TRUE))
      names(x) <- sprintf("%s.%s", names(x), output_extension(opt))
      x
    }
  } else {
    stop("invalid combination of options")
  }
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
  ycol <- options$ycolumn[[1L]]
  m <- assort_strings(x[, options$xcolumn[[1L]]], y[, ycol], options$threshold)
  m <- m[match(y[, ycol], m$Y), , drop = FALSE]
  ncol <- paste(ycol, c("ORIG", "DIST"), options$threshold, idx, sep = "_")
  colnames(y)[colnames(y) == ycol] <- ncol[[1L]]
  y[, c(ycol, ncol[[2L]])] <- m[, c("X", "D")]
  if (options$all && any(isna <- is.na(y[, ycol])))
    y[isna, ycol] <- y[isna, ncol[[1L]]]
  y
}


################################################################################
#
# option processing
#


option.parser <- optparse::OptionParser(option_list = list(

  optparse::make_option(opt_str = c("-a", "--all"), action = "store_true",
    help = paste0("Keep non-matching lines of file 2, too; with -v, keep all ",
      "(even duplicated) entries; with -w, create 'long' format",
      " [default: %default]"),
    default = FALSE),

  # A

  optparse::make_option(opt_str = c("-b", "--bald"), action = "store_true",
    help = "Assume files have no headers [default: %default]",
    default = FALSE),

  # B

  optparse::make_option(opt_str = c("-c", "--conserve"), action = "store_true",
    help = "Conserve input column order, do not sort [default: %default]",
    default = FALSE),

  optparse::make_option(opt_str = c("-C", "--compact"), action = "store_true",
    help = "Compact table with gaps by upwards moves [default: %default]",
    default = FALSE),

  optparse::make_option(opt_str = c("-d", "--delete"), action = "store_true",
    help = "Delete non-matching lines of file 1 [default: %default]",
    default = FALSE),

  optparse::make_option(opt_str = c("-D", "--duplicates"),
    action = "store_true", default = FALSE,
    help = paste0("Remove duplicates of the specified combination of ",
      "columns, skip normal run [default: %default]")),

  optparse::make_option(opt_str = c("-e", "--encoding"), type = "character",
    help = "Encoding to be assumed in input files [default: '%default']",
    metavar = "NAME", default = ""),

  # E

  optparse::make_option(opt_str = c("-f", "--first"), action = "store_true",
    help = "Do not adapt column names of file 1 [default: %default]",
    default = FALSE),

  optparse::make_option(opt_str = c("-F", "--fission"), action = "store_true",
    help = "Fission each file by the chosen column(s) [default: %default]",
    default = FALSE),

  # g

  optparse::make_option(opt_str = c("-G", "--good"), action = "store_true",
    help = paste0("Use case-insensitive matching; with -v, keep only most ",
      "frequent entries; with -D, delete duplicate lines; with -r, ",
      "carefully merge only the selected columns [default: %default]"),
    default = FALSE),

  optparse::make_option(opt_str = c("-h", "--help"), action = "store_true",
    help = "Print help message and exit [default: %default]",
    default = FALSE),

  # H

  optparse::make_option(opt_str = c("-i", "--indices"), action = "store_true",
    help = "Use indices for adapting column names [default: %default]",
    default = FALSE),

  optparse::make_option(opt_str = c("-I", "--insert"), type = "character",
    help = paste("Prefix for name of column to insert if only -x/-y columns",
      "are present [default: '%default']"),
    metavar = "NAME", default = "Present"),

  optparse::make_option(opt_str = c("-j", "--join-by"), type = "character",
    help = "Join character(s) for -r/-v [default: '%default']",
    metavar = "SEP", default = "; "),

  # J

  optparse::make_option(opt_str = c("-k", "--keep"), action = "store_true",
    help = "Keep whitespace surrounding the separators [default: %default]",
    default = FALSE),

  # K

  optparse::make_option(opt_str = c("-l", "--load"), action = "store_true",
    help = "Randomly replace missing by present values [default: %default]",
    default = FALSE),

  # L

  optparse::make_option(opt_str = c("-m", "--make-header"),
    help = "Output headers even for input without headers [default: %default]",
    default = FALSE, action = "store_true"),

  optparse::make_option(opt_str = c("-M", "--map-table"), type = "character",
    help = "YAML file for table-mapping running mode [default: %default]",
    default = "", metavar = "FILE"),

  optparse::make_option(opt_str = c("-n", "--names"), action = "store_true",
    help = "Convert column names to syntactical names [default: %default]",
    default = FALSE),

  # N

  optparse::make_option(opt_str = c("-o", "--onename"), action = "store_true",
    help = paste("Do not split arguments of '-x' and '-y' at ','",
      "[default: %default]"), default = FALSE),

  optparse::make_option(opt_str = c("-O", "--output-separator"),
    help = "Field separator for output CSV files [default: '%default']",
    type = "character", metavar = "SEP", default = "\t"),

  optparse::make_option(opt_str = c("-p", "--prune"), type = "character",
    help = "Value to prune by treating as NA [default: '%default']",
    default = "NA", metavar = "STR"),

  # P

  optparse::make_option(opt_str = c("-q", "--unique"), action = "store_true",
    help = "Make entries in join column unique [default: %default]",
    default = FALSE),

  optparse::make_option(opt_str = c("-Q", "--quote"), type = "character",
    help = "Input quoting character [default: '%default']",
    default = "\"", metavar = "CHAR"),

  optparse::make_option(opt_str = c("-r", "--rows"), action = "store_true",
    help = "Merge each row horizontally, file by file [default: %default]",
    default = FALSE),

  # R

  optparse::make_option(opt_str = c("-s", "--separator"), type = "character",
    help = "Field separator in input CSV files [default: '%default']",
    metavar = "SEP", default = "\t"),

  optparse::make_option(opt_str = c("-S", "--spreadsheets"),
    help = paste0("Split LibreOffice/OpenOffice/Excel spreadsheet files",
      ", skip normal run [default: %default]"),
    action = "store_true", default = FALSE),

  optparse::make_option(opt_str = c("-t", "--threshold"), type = "numeric",
    help = "Threshold for error-tolerant matching [default: %default]",
    metavar = "NUM", default = -1),

  # T

  optparse::make_option(opt_str = c("-u", "--unquoted"), action = "store_true",
    help = "Do not quote fields in output [default: %default]",
    default = FALSE),

  # U

  optparse::make_option(opt_str = c("-v", "--vertical"), action = "store_true",
    help = "Merge vertically, file by file [default: %default]",
    default = FALSE),

  # V

  optparse::make_option(opt_str = c("-w", "--widen"), action = "store_true",
    help = paste0("Widen (unnest) selected column(s); with -a, create 'long'",
    " format [default: %default]"),
    default = FALSE),

  # W

  optparse::make_option(opt_str = c("-x", "--xcolumn"), type = "character",
    help = "Name of the merge column(s) in file 1 [default: '%default']",
    default = COLUMN_DEFAULT_NAME, metavar = "COLUMNS"),

  # X

  optparse::make_option(opt_str = c("-y", "--ycolumn"), type = "character",
    help = "Name of the merge column(s) in file 2 [default: like file 1]",
    default = "", metavar = "COLUMNS"),

  optparse::make_option(opt_str = c("-Y", "--yaml"), action = "store_true",
    help = "Produce YAML mappings, skip normal run [default: %default]",
    default = FALSE),

  optparse::make_option(opt_str = c("-z", "--zack"), action = "store_true",
    help = "Fill (sack) column(s) downwards [default: %default]",
    default = FALSE)

  # Z

), usage = "%prog [options] csv_file_1 csv_file_2 ...", prog = "merge.R",
  add_help_option = FALSE, description = paste("\nMerge CSV files",
    "rows or columns in such files", "or fill rows randomly.", sep = ", "),
  epilogue = paste(
    "In default mode, providing less than two files makes not much sense. It",
    "is possible to provide unequal numbers of X and Y columns, but then the",
    "behaviour is special, as the multiple columns are used subsequently until",
    "matching succeeds, whereas some other merging options are not available.",
    "", sep = "\n")
)


opt <- optparse::parse_args(object = option.parser, positional_arguments = TRUE)
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
  quit("no", 1L)
}


################################################################################
#
# horizontal merging of rows, vertical merging, random filling, or YAML
# generation; everything file-by-file, no merging between files
#

if (opt$spreadsheets) {
  process_spreadsheets(files, opt)
  quit("no", 0L)
} else if (opt$vertical || opt$rows || opt$load || opt$widen || opt$zack ||
    opt$duplicates || opt$fission || opt$compact || nzchar(opt$`map-table`)) {
  process_specially(files, opt)
  quit("no", 0L)
} else if (opt$yaml) {
  to_yaml(files, opt)
  quit("no", 0L)
}


################################################################################
#
# horizontal merging of files (= default)
#


data <- read_and_create_unique_column_names(files, opt)

if (opt$good) {
  for (col in opt$xcolumn)
    data[[1L]][, col] <- toupper(trunc_zeros(data[[1L]][, col]))
  for (i in seq_along(data)[-1L])
    for (col in opt$ycolumn)
      data[[i]][, col] <- toupper(trunc_zeros(data[[i]][, col]))
}

if (opt$unique) {
  for (col in opt$xcolumn)
    data[[1L]][, col] <- make_unique(data[[1L]][, col])
  for (i in seq_along(data)[-1L])
    for (col in opt$ycolumn)
      data[[i]][, col] <- make_unique(data[[i]][, col])
}

match_successively <- function(x, y, xcol, ycol, opt) {
  result <- cbind(x, y[match(x[, xcol], y[, ycol[[1L]]]), , drop = FALSE])
  bad <- is.na(result[, ycol[[1L]]])
  for (yc2 in ycol[-1L]) {
    if (!any(bad))
      break
    result[bad, colnames(y)] <- y[match(x[bad, xcol], y[, yc2]), , drop = FALSE]
    bad <- bad & is.na(result[, yc2])
  }
  if (!opt$conserve)
    result <- result[order(bad, result[, xcol]), , drop = FALSE]
  rownames(result) <- NULL
  result
}

if (length(opt$ycolumn) > length(opt$xcolumn)) {
  stopifnot(length(opt$xcolumn) == 1L)
  x <- data[[1L]]
  for (item in data[-1L])
    x <- match_successively(x, item, opt$xcolumn, opt$ycolumn, opt)
} else if (length(opt$xcolumn) > length(opt$ycolumn)) {
  stopifnot(length(opt$ycolumn) == 1L)
  x <- data[[1L]]
  for (item in data[-1L])
    x <- match_successively(item, x, opt$ycolumn, opt$xcolumn, opt)
} else {
  x <- data[[1L]]
  for (i in seq_along(data)[-1L]) {
    if (opt$threshold >= 0)
      data[[i]] <- include_approximate_matches(x, data[[i]], opt, i)
    x <- merge(x = x, y = data[[i]], by.x = opt$xcolumn, by.y = opt$ycolumn,
      all.x = !opt$delete, all.y = opt$all, sort = !opt$conserve)
  }
}

# In presence-indicating columns, NA means FALSE.
for (name in INSERTED_COLUMNS) {
  x[is.na(x[, name]), name] <- FALSE
}

do_write(x, opt)


################################################################################


