

#' # Analyse a Bacillus fatty-acid example data set
#'
#' Author: *Markus Goeker*

library(opmlipids)


#' # Select one of the example input files that come with the package

files <- pkgutils::pkg_files("opmlipids", "testdata")
if (!length(files)) {
  stop("'opmlipids' example input files not found")
}

#' # Input and summarise data

x <- read_rtf(files[grepl("Sikorski", basename(files))], include = NULL)
print(summary(x))


#' # Plot without transformation

heat_map(x, "Sample ID", asqr = NA)


#' # Plot with or without transformation

heat_map(x, "Sample ID")

#' The arcsine-square root transformation is recommended for proportion data
#' under certain conditions. The code below takes care of the fact that here
#' proportions are given as percent.

heat_map(x, "Sample ID", asqr = NA, main = "With transformation")


#' # Remove dubious measurements, if any, and plot again

#' This code removes all calibration plates and all plates with less than 99%
#' named peaks.
#'
x <- x[~ Type != "Calib" && as.numeric(`Percent Named`) >= 99]

heat_map(x, "Sample ID", asqr = NA, main = "Cleaned data")



#' # Manipulate metadata

#' In many cases strain names are hidden in (= are part of) the sample ID.
#'
metadata(x) <- Strain ~ sub("^([^-]+-)+", "", `Sample ID`)


#' # Create and plot principal components

xpr <- prcomp(extract(x, "Strain"))
biplot(xpr)


