#!/usr/local/bin/Rscript --vanilla


for (pkg in c("methods", "optparse"))
  library(pkg, quietly = TRUE, character.only = TRUE)


options(warn = 1L)


################################################################################
#
# Functions
#


rtf2yaml <- function(infile, outfile) {
  write(to_yaml(read_rtf(infile, include = NULL)), outfile)
}


rtf2csv <- function(infile, outfile) {
  x <- extract(read_rtf(infile, include = NULL), "Sample ID", dataframe = TRUE)
  write.table(x, file = outfile, sep = "\t", row.names = FALSE)
}



################################################################################
#
# Option processing
#


option.parser <- OptionParser(option_list = list(

  make_option(c("-d", "--dir"), type = "character", default = ".",
    help = "Output directory (empty => input directory) [default: %default]"),

#   make_option(c("-k", "--keep"), action = "store_true",
#     default = FALSE,
#     help = "Also keep uninformative fatty-acid datasets [default: %default]"),

  make_option(c("-n", "--newer"), action = "store_true",
    default = FALSE,
    help = "Overwrite only if input file is newer [default: %default]"),

  make_option(c("-o", "--output"), type = "character",
    default = "yaml", metavar = "MODE",
    help = "Main output mode [default: %default]"),

  make_option(c("-p", "--processes"), type = "numeric",
    default = 1L, metavar = "NUMBER",
    help = "Number of processes to spawn [default: %default]"),

  make_option(c("-q", "--quiet"), action = "store_true",
    default = FALSE, help = "Run quietly [default: %default]")

))


################################################################################


opt <- parse_args(option.parser, positional_arguments = TRUE)
infiles <- opt$args
opt <- opt$options


if (!length(infiles)) {
  message("\nrun_opmlipids.R -- Analyse RTF files from the MIDI system.\n")
  print_help(option.parser)
  quit(status = 1L)
}

for (pkg in c("opmlipids", "pkgutils", "yaml"))
  suppressMessages(library(pkg, quietly = TRUE, character.only = TRUE))


################################################################################

case(opt$output,
  csv = {
    opt$out.ext <- "csv"
    opt$io.fun <- rtf2csv
  },
  yml =,
  yaml = {
    opt$out.ext <- "yml"
    opt$io.fun <- rtf2yaml
  }
)

opm::batch_process(names = infiles, out.ext = opt$out.ext, in.ext = "any",
  io.fun = opt$io.fun, wildcard = TRUE, outdir = opt$dir,
    compressed = TRUE, literally = FALSE, overwrite = if (opt$newer)
    "older"
  else
    "yes",
  verbose = !opt$quiet, include = "*.rtf", demo = FALSE)


################################################################################

