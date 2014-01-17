

#' # Analyse a Bacillus fatty-acid data set provided together with the package
#'
#' Author: *Markus Goeker*

library(opmlipids)


#' # Select one of the example input files that come with the package

files <- pkgutils::pkg_files("opmlipids", "testdata")
if (!length(files)) {
  stop("'opmlipids' example input files not found")
}

x <- read_rtf(files[grepl("Sikorski", basename(files))], include = NULL)
print(summary(x))


#' # Plot with transformation

heat_map(x, "Sample ID", asqr = NA)

#' # Remove the calibration plates, if any, and plot again

if (any(is.calib <- x %q% c(Type = "Calib")))
  heat_map(x[!is.calib], "Sample ID", asqr = NA)


