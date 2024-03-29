#!/usr/local/bin/Rscript --vanilla


################################################################################
#
# run_opm.R -- R script for non-interactive use of the opm package
#
# (C) 2013 by Markus Goeker (markus [DOT] goeker [AT] dsmz [DOT] de)
#
# This script is distributed under the terms of the GPL. For further details
# see the opm package.
#
################################################################################


invisible(lapply(X = c("methods", "pkgutils"), FUN = library, quietly = TRUE,
  warn.conflicts = FALSE, character.only = TRUE))


MD.OUTFILE <- "metadata.csv"
RESULT <- c(
  "Clean filenames by removing non-word characters except dots and dashes.",
  "Draw level plots into graphics files, one per input file.",
  "Split OmniLog(R) CSV files into one file per plate.",
  "Collect a template for adding metadata.",
  "Draw xy plots into graphics files, one per input file.",
  "Convert input OmniLog(R) CSV (or opm YAML or JSON) files to opm YAML.",
  "Convert input OmniLog(R) CSV (or opm YAML or JSON) files to opm JSON.",
  "Convert input OmniLog(R) CSV (or opm YAML or JSON) files to opm CSV."
)
names(RESULT) <- c("clean", "levelplot", "split", "template", "xyplot",
  "yaml", "json", "csv")
AGGREGATION <- c(
  "No estimation of curve parameters.",
  "Fast estimation (only two parameters).",
  "Approach from grofit package.",
  "Using p-splines.",
  "Using smoothing splines.",
  "Using thin-plate splines"
)
names(AGGREGATION) <- c("no", "fast", "grofit", "p", "smooth", "thin")


################################################################################
#
# Helper functions
#


parse_key_list <- function(x) {
  if (!length(x))
    return(NULL)
  x <- unlist(strsplit(x, ",", TRUE))
  x <- do.call(cbind, strsplit(x, ":", TRUE))
  structure(x[nrow(x), ], names = x[1L, ])
}


################################################################################


run_template_mode <- function(input, opt) {
  mdfile <- opt$mdfile
  if (is.null(mdfile)) {
    message(sprintf("NOTE: Using default metadata template outfile '%s'.",
      MD.OUTFILE))
    mdfile <- MD.OUTFILE
    previous <- if (file.exists(mdfile))
      mdfile
    else
      NULL
  } else
    previous <- mdfile
  collect_template(object = input, outfile = mdfile, previous = previous,
    include = opt$include, exclude = opt$exclude, sep = opt$sep,
    instrument = opt$ynstrument, normalize = opt$`value-normal`)
}


################################################################################


run_batch_opm <- function(input, opt) {
  make_md_args <- function(opt) {
    if (is.null(opt$mdfile))
      NULL
    else
      list(md = opt$mdfile, sep = opt$sep, replace = opt$exchange)
  }
  make_disc_args <- function(opt) {
    if (opt$discretize)
      list(cutoff = opt$weak, plain = FALSE)
    else
      NULL
  }
  make_table_args <- function(opt) {
    list(row.names = FALSE, sep = opt$`use-sep`)
  }
  make_aggr_args <- function(opt) {
    aggr_args <- function(opt, method, spline) {
      x <- list(boot = opt$bootstrap, verbose = !opt$quiet,
        cores = opt$processes)
      x$method <- method
      if (length(spline))
        x$options <- set_spline_options(type = spline)
      x
    }
    case(match.arg(opt$aggregate, names(AGGREGATION)),
      no = NULL,
      fast = aggr_args(opt, "opm-fast", NULL),
      grofit = aggr_args(opt, "grofit", NULL),
      p = aggr_args(opt, "splines", "p.spline"),
      smooth = aggr_args(opt, "splines", "smooth.spline"),
      thin = aggr_args(opt, "splines", "tp.spline")
    )
  }
  if (opt$coarse || opt$aggregate == "fast") {
    proc <- opt$processes # this must be run before make_aggr_args()
    opt$processes <- 1L
  } else
    proc <- 1L
  batch_opm(names = input, proc = proc, disc.args = make_disc_args(opt),
    outdir = opt$dir, aggr.args = make_aggr_args(opt),
    md.args = make_md_args(opt), verbose = !opt$quiet, force.aggr = opt$force,
    table.args = make_table_args(opt), force.disc = opt$force,
    overwrite = opt$overwrite, include = opt$include, device = opt$format,
    exclude = opt$exclude, gen.iii = opt$type, output = opt$result,
    combine.into = opt$join, csv.args = opt$keys)
}


################################################################################
#
# Option processing
#


option.parser <- optparse::OptionParser(option_list = list(

  optparse::make_option(c("-a", "--aggregate"), type = "character",
    help = "Aggregate by estimating curve parameters [default: %default]",
    default = "no", metavar = "METHOD"),

  # A

  optparse::make_option(c("-b", "--bootstrap"), type = "integer",
    help = "# bootstrap replicates when aggregating [default: %default]",
    default = 100L, metavar = "NUMBER"),

  # B

  optparse::make_option(c("-c", "--coarse"), action = "store_true",
    help = "Use coarse-grained parallelization, if any [default: %default]",
    default = FALSE),

  # C

  optparse::make_option(c("-d", "--dir"), type = "character", default = ".",
    help = "Output directory (empty => input directory) [default: %default]"),

  # D

  optparse::make_option(c("-e", "--exclude"), type = "character", default = "",
    help = "File exclusion globbing pattern [default: <none>]",
    metavar = "PATTERN"),

  # E

  optparse::make_option(c("-f", "--format"), type = "character",
    help = "Graphics output format [default: %default]", metavar = "NAME",
    default = "postscript"),

  optparse::make_option(c("-F", "--force"), action = "store_true",
    help = paste("Overwrite previous aggregated/discretized values",
      "[default: %default]"), default = FALSE),

  ## A bug in Rscript causes '-g' to generate strange warning messages.
  ## See https://stat.ethz.ch/pipermail/r-devel/2008-January/047944.html
  # g

  # G

  optparse::make_option(c("-h", "--help"), action = "store_true",
    help = "Show this help message and exit [default: %default]",
    default = FALSE),

  # H

  optparse::make_option(c("-i", "--include"), type = "character",
    help = "File inclusion globbing pattern [default: <see package>]",
    metavar = "PATTERN", default = NULL),

  # I

  optparse::make_option(c("-j", "--join"), type = "character", default = NULL,
    help = "Template for joining data into that outfile [default: <none>]",
    metavar = "TEMPLATE"),

  # J

  optparse::make_option(c("-k", "--keys"), type = "character", default = NULL,
    help = "Keys for CSV => metadata, comma-separated list [default: <none>]",
    metavar = "KEYS"),

  # K

  optparse::make_option(c("-l", "--list"), type = "character",
    help = "List of numbers giving the 'input.try.order' [default: %default]",
    metavar = "INDEXES", default = "1,2,3"),

  # L

  optparse::make_option(c("-m", "--mdfile"), type = "character",
    default = NULL, metavar = "NAME",
    help = "Metadata infile (also used as outfile if given) [default: <none>]"),

  # M

  optparse::make_option(c("-n", "--encoding"), type = "character",
    default = "", metavar = "NAME",
    help = "Assumed character encoding in CSV input [default: %default]"),

  # N

  optparse::make_option(c("-o", "--overwrite"), type = "character",
    help = "Overwrite pre-existing output files [default: %default]",
    metavar = "MODE", default = "older"),

  # O

  optparse::make_option(c("-p", "--processes"), type = "integer", default = 1L,
    help = paste("Number of processes to spawn (>1 impossible under Windows)",
      "[default: %default]"),
    metavar = "NUMBER"),

  # P

  optparse::make_option(c("-q", "--quiet"), action = "store_true",
    help = "Do not run verbosely [default: %default]", default = FALSE),

  # Q

  optparse::make_option(c("-r", "--result"), type = "character",
    metavar = "MODE", default = "yaml", help = sprintf(
      "Main result mode; possible values: %s [default: %%default]",
      paste(names(RESULT), collapse = ", "))),

  # R

  optparse::make_option(c("-s", "--sep"), type = "character", default = "\t",
    help = "Field separator for metadata files [default: <tab>]",
    metavar = "CHAR"),

  # S

  optparse::make_option(c("-t", "--type"), type = "character", default = "",
    help = "Change plate type to this one [default: %default]",
    metavar = "CHAR"),

  # T

  optparse::make_option(c("-u", "--use-sep"), type = "character",
    help = "Field separator for metadata files [default: <tab>]",
    metavar = "CHAR", default = "\t"),

  # U

  optparse::make_option(c("-v", "--value-normal"), action = "store_true",
    help = "Normalize setup time and position [default: %default]",
    default = FALSE),

  # V

  optparse::make_option(c("-w", "--weak"), action = "store_true",
    help = paste("When discretizing, estimate intermediary (weak) reaction",
      "[default: %default]"), default = FALSE),

  # W

  optparse::make_option(c("-x", "--exchange"), action = "store_true",
    help = paste("Exchange old by new metadata instead of appending",
      "[default: %default]"), default = FALSE),

  # X

  optparse::make_option(c("-y", "--ynstrument"), type = "integer", default = -1,
    help = "Use this as instrument ID (ignored if < 0) [default: %default]",
    metavar = "NUM"),

  # Z

  optparse::make_option(c("-z", "--discretize"), action = "store_true",
    help = "Discretize after estimating curve parameters [default: %default]",
    default = FALSE)

), usage = "%prog [options] [directories/files]", add_help_option = FALSE,
  epilogue = c(
    listing(RESULT, header = "The output modes are:",
      footer = paste0(
        "\nNote that an unsuitable combination of input and output settings",
        " can cause\nthe deletion of input files.\n"
      ),
      prepend = 5L, indent = 10L),
    listing(AGGREGATION, header = "The aggregation modes are:", footer = "",
      prepend = 5L, indent = 10L)
  )
)

opt <- optparse::parse_args(object = option.parser, positional_arguments = TRUE)
input <- opt$args
opt <- opt$options
if (is.null(opt$include))
  opt$include <- list()
if (!nzchar(opt$dir))
  opt$dir <- NULL
opt$keys <- parse_key_list(opt$keys)
if (opt$ynstrument < 0L)
  opt$ynstrument <- NULL
opt$list <- must(as.integer(parse_key_list(opt$list)))


################################################################################
#
# Running the proper script
#


if (!length(input)) {
  optparse::print_help(option.parser)
  quit("default", 1L)
}


library(opm, quietly = TRUE, warn.conflicts = FALSE)


invisible(opm_opt(file.encoding = opt$encoding, input.try.order = opt$list))


case(match.arg(opt$result, names(RESULT)),
  clean =,
  split =,
  xyplot =,
  levelplot = {
    opt$aggregate <- "no"
    opt$coarse <- TRUE # fine-grained approach only of use within aggregation
    opt$discretize <- FALSE
    run_batch_opm(input, opt)
  },
  template = run_template_mode(input, opt),
  csv =,
  json =,
  yaml = run_batch_opm(input, opt)
)


################################################################################



