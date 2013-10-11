#!/usr/local/bin/Rscript --vanilla


################################################################################
#
# profile.R -- Rscript script for non-interactively profiling R code.
#
# (C) 2012/2013 by Markus Goeker (markus [DOT] goeker [AT] dsmz [DOT] de)
#
# This program is distributed under the terms of the Gnu Public License V2.
# For further information, see http://www.gnu.org/licenses/gpl.html
#
################################################################################


for (lib in c("tools", "optparse"))
  library(lib, quietly = TRUE, warn.conflicts = FALSE, character.only = TRUE)


#do_profile <- function(FNAME, N, ..., FILE = paste(FNAME, "out", sep = ".")) {
#  f <- match.fun(FNAME)
#  args <- list(...)
#  Rprof(FILE)
#  for (i in seq_len(N))
#    do.call(f, args)
#  Rprof(NULL)
#}


################################################################################


option.parser <- OptionParser(option_list = list(

  make_option(c("-r", "--replicates"), type = "integer", default = 100L,
    help = "Number of replicates when profiling [default: %default]",
    metavar = "NUM")

))


opt <- parse_args(option.parser, positional_arguments = TRUE)
infiles <- opt$args
opt <- opt$options


if (opt$help || !length(infiles)) {
  print_help(option.parser)
  quit(status = 1L)
}

# TODO: add other output mode: source all input files, then call a specified
# function

# TODO: add file extension option
ext <- "out"

# TODO: add output directory option
outfiles <- sprintf("%s.%s", file_path_sans_ext(basename(infiles)), ext)

for (i in seq_along(infiles)) {
  expr <- parse(infiles[i])
  Rprof(outfiles[i])
  for (i in seq_len(opt$replicates))
    eval(expr)
  Rprof(NULL)
}



