
################################################################################
################################################################################
#
# Descriptions of all data sets included in the package
#


#' Example data sets from Vaas et al. (2012)
#'
#' Two literature example data sets are provided with the \pkg{opm} package,
#' containing one or four plates, respectively.
#'
#' @details The \code{\link{OPMS}} object \code{vaas_4} contains measurements
#' from four selected plates from the study by Vaas \emph{et al.} (2012).
#' Metadata have been added to fully describe the conducted \acronym{PM}
#' experiments: these plates are the sixth technical replicate from the first
#' biological replicate for the four bacterial strains considered in the study.
#'
#' This particular subset served as input for Figure 2 in Vaas \emph{et al.}
#' (2012), which can be reproduced by running \code{\link{xy_plot}} with
#' \code{vaas_4}. Accordingly, Figure 3 in Vaas \emph{et al.} (2012) represents
#' the wells G11 and H11 selected from \code{vaas_4}. Figure 4 focuses then
#' further on that subset, namely showing the data from strain \acronym{DSM}
#' 30083T (left curve) and strain \emph{Pseudomonas aeruginosa} \acronym{DSM}
#' 1707 (right curve).
#'
#' The \code{\link{OPMD}} object \code{vaas_1} contains measurements from a
#' single selected plate from the study by Vaas \emph{et al.} (2012). Metadata
#' have been added to fully describe the conducted \acronym{PM} experiments:
#' this plate is the sixth technical replicate from the first biological
#' replicate for the strain \emph{Escherichia coli} \acronym{DSM} 30083T (yes,
#' the type strain of \emph{E. coli}). This is a subset of \code{vaas_4}.
#'
#' The complete data set is available as \code{vaas_et_al} in the \pkg{opmdata}
#' package.
#'
#' @docType data
#' @keywords datasets
#' @name vaas_4
#' @aliases vaas_1
#' @format \code{vaas_4} is an \code{\link{OPMS}} object with the dimensions 4 x
#'   384 x 96, i.e. 4 plates with 384 time points and 96 wells per plate.
#'   \code{vaas_1} is an \code{\link{OPMD}} object with the dimensions 384 x 96,
#'   i.e. a single plate with 384 time points and 96 wells.
#' @references Bochner, B.R., Savageau, M.A. 1977. Generalized indicator plate
#'   for genetic, metabolic, and taxonomic studies with microorganisms. \emph{
#'   Applied and Environmental Microbiology} \strong{33}, 434--444.
#' @references Selezska, K., Kazmierczak, M., Muesken, M., Garbe, J., Schobert,
#'   M., Haeussler, S., Wiehlmann, L., Rohde, C., Sikorski, J. 2012
#'   \emph{Pseudomonas aeruginosa} population structure revisited under
#'   environmental focus: impact of water quality and phage pressure.
#'   \emph{Environmental Microbiology} \strong{14}, 1952--1967.
#' @references Vaas, L. A. I., Sikorski, J., Michael, V., Goeker, M., Klenk
#'   H.-P. 2012 Visualization and curve parameter estimation strategies for
#'   efficient exploration of Phenotype Microarray kinetics. \emph{PLoS ONE}
#'   \strong{7}, e34846.
#' @references \url{http://www.dsmz.de/catalogues/details/culture/DSM-1707.html}
#' @references
#' \url{http://www.dsmz.de/catalogues/details/culture/DSM-18039.html}
#' @references
#' \url{http://www.dsmz.de/catalogues/details/culture/DSM-30083.html}
#' @examples \dontrun{
#'
#' # Calling this yielded a variable 'vaas_4' containing the data. The opm
#' # package must be loaded beforehand using library().
#' data(vaas_4)
#'
#' # Calling this yielded a variable 'vaas_1' containing the data. The opm
#' # package must be loaded beforehand using library().
#' data(vaas_1)
#' }
#'
NULL


################################################################################


#' Potato cell-line growth data set
#'
#' Example data set for analysing growth curves with \pkg{opm} containing
#' manually entered fresh-mass and dry-mass measurements over time from three
#' distinct potato (\emph{Solanum tuberosum}) cell lines under several stress
#' treatments.
#'
#' @docType data
#' @keywords datasets
#' @name potato
#' @format Data frame with 540 rows and six columns. The columns are:
#' \describe{
#'   \item{Genotype}{Factor containing the names of the cell lines.}
#'   \item{Treatment}{Factor describing the applied stress conditions. There is
#'     also one level for the control.}
#'   \item{Replicate}{Integer vector with the number of the replicate. There
#'     are five replicates per combination of \sQuote{Genotype} and
#'     \sQuote{Treatment}.}
#'   \item{Time}{Integer vector containing the measurement times in hours.}
#'   \item{FM}{Integer vector containing the fresh-mass measurements in grams.}
#'   \item{DM}{Integer vector containing the dry-mass measurements in grams.}
#' }
#' @details How to convert and analyse this data set is explained in the
#'   vignette on working with growth curves in \pkg{opm}.
#' @references Sandford, S. A. 1995. Apples and Oranges -- A Comparison.
#'   \emph{Annals of Improbable Research} \strong{1} (3).
#' @references  Vaas, L. A. I., Marheine, M., Sikorski, J., Goeker, M.,
#'   Schumacher, H.-M. 2013 Impacts of pr-10a overexpression at the molecular
#'   and the phenotypic level. \emph{International Journal of Molecular
#'   Sciences} \strong{14}: 15141--15166.
#' @examples \dontrun{
#'
#' # Calling this yielded a variable 'potato' containing the data. The opm
#' # package must be loaded beforehand using library().
#' data(potato)
#' }
#'
NULL


################################################################################
