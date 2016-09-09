#!/usr/local/bin/Rscript --vanilla


################################################################################
#
# profile.R -- Rscript script for non-interactively profiling R code.
#
# (C) 2013 by Markus Goeker (markus [DOT] goeker [AT] dsmz [DOT] de)
#
# This program is distributed under the terms of the Gnu Public License V2.
# For further information, see http://www.gnu.org/licenses/gpl.html
#
################################################################################


make_outfiles <- function(x, opt) {
  if (opt$stdout)
    return(rep.int("/dev/stdout", length(x)))
  x <- sprintf("%s.%s", tools::file_path_sans_ext(x, TRUE), opt$extension)
  if (nzchar(opt$directory))
    file.path(opt$directory, basename(x))
  else
    x
}


parse_arg_listing <- function(x) {
  unique.default(unlist(strsplit(x, ",", TRUE), FALSE, FALSE))
}


################################################################################


option.parser <- optparse::OptionParser(option_list = list(

  optparse::make_option(opt_str = c("-d", "--directory"), type = "character",
    help = "Output directory (empty => input directory) [default: %default]",
    metavar = "STR", default = "."),

  optparse::make_option(opt_str = c("-e", "--extension"), type = "character",
    help = "Output file extension [default: %default]", default = "out",
    metavar = "STR"),

  optparse::make_option(opt_str = c("-f", "--files"), type = "character",
    help = "Comma-separated list of *.rda files to load [default: %default]",
    metavar = "STR", default = ""),

  optparse::make_option(opt_str = c("-p", "--packages"), type = "character",
    help = "Comma-separated list of R packages to load [default: %default]",
    metavar = "STR", default = ""),

  optparse::make_option(opt_str = c("-r", "--replicates"), type = "integer",
    help = "Number of replicates when profiling [default: %default]",
    metavar = "NUM", default = 100L),

  optparse::make_option(opt_str = c("-s", "--stdout"), action = "store_true",
    help = "Send output to STDOUT [default: %default]", default = FALSE)

))


################################################################################


opt <- optparse::parse_args(object = option.parser, positional_arguments = TRUE)
infiles <- opt$args
opt <- opt$options

if (opt$help || !length(infiles)) {
  optparse::print_help(option.parser)
  quit("no", 1L)
}

opt$files <- parse_arg_listing(opt$files)
opt$packages <- parse_arg_listing(opt$packages)
opt$replicates <- seq_len(opt$replicates)

invisible(lapply(X = opt$packages, FUN = require, quietly = TRUE,
  warn.conflicts = FALSE, character.only = TRUE))
for (file in opt$files)
  load(file)


################################################################################


# Note that other output modes are possible. For instance, sourceing all input
# files, then calling a specified function.

outfiles <- make_outfiles(infiles, opt)

for (i in seq_along(infiles)) {
  expr <- parse(infiles[[i]])
  Rprof(outfiles[[i]])
  for (j in opt$replicates)
    eval(expr)
  Rprof(NULL)
}


################################################################################


