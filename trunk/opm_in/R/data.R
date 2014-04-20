
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
#' 30083\eqn{\textsuperscript{T}}{T} (left curve) and strain \emph{Pseudomonas
#' aeruginosa} \acronym{DSM} 1707 (right curve).
#'
#' The \code{\link{OPMD}} object \code{vaas_1} contains measurements from a
#' single selected plate from the study by Vaas \emph{et al.} (2012). Metadata
#' have been added to fully describe the conducted \acronym{PM} experiments:
#' this plate is the sixth technical replicate from the first biological
#' replicate for the strain \emph{Escherichia coli} \acronym{DSM}
#' 30083\eqn{\textsuperscript{T}}{T} (yes, the type strain of \emph{E. coli}).
#' This is a subset of \code{vaas_4}.
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
#'   \item{Genotype}{Factor containing the names of the cell lines. See Vaas
#'   \emph{et al.} (2013) for further details.}
#'   \item{Treatment}{Factor describing the three applied stress conditions.
#'   There is also one level for the control.}
#'   \item{Replicate}{Integer vector with the number of the replicate. There
#'     are five replicates per combination of \sQuote{Genotype} and
#'     \sQuote{Treatment}.}
#'   \item{Time}{Integer vector containing the measurement times in days.}
#'   \item{FM}{Integer vector containing the fresh-mass measurements in
#'   milligramme.}
#'   \item{DM}{Integer vector containing the dry-mass measurements in
#'   milligramme.}
#' }
#' @details How to convert and analyse this data set is explained in the
#'   vignette on working with growth curves in \pkg{opm}.
#'
#' @references Vaas, L. A. I., Marheine, M., Sikorski, J., Goeker, M.,
#'   Schumacher, H.-M. 2013 Impacts of pr-10a overexpression at the molecular
#'   and the phenotypic level. \emph{International Journal of Molecular
#'   Sciences} \strong{14}: 15141--15166.
#' @references El-Banna, A., Hajirezaei, M. R., Wissing, J., Ali, Z., Vaas, L.,
#'   Heine-Dobbernack, E., Jacobsen, H.-J., Schumacher, H.-M., Kiesecker, H.
#'   2010 Over-expression of PR-10a leads to increased salt and osmotic
#'   tolerance in potato cell cultures. \emph{Journal of Biotechnology}
#'   \strong{150}: 277--287.
#' @references Sandford, S. A. 1995. Apples and Oranges -- A Comparison.
#'   \emph{Annals of Improbable Research} \strong{1} (3).
#'
#' @examples \dontrun{
#'
#' # Calling this yielded a variable 'potato' containing the data. The opm
#' # package must be loaded beforehand using library().
#' data(potato)
#' }
#'
NULL


################################################################################


#' Autism cell-line example data set
#'
#' Example data set for analysing phenotype microarray data that were not
#' recorded with an OmniLog\eqn{\textsuperscript{\textregistered}}{(R)}
#' instrument.
#'
#' @docType data
#' @keywords datasets
#' @name boccuto_et_al
#' @format \code{boccuto_et_al} is a \code{\link{MOPMX}} object with four
#'   \code{\link{OPMX}} objects of the dimension 35 x 1 x 96 as elements. The
#'   plate types are \sQuote{CUSTOM:PM-M01} to \sQuote{CUSTOM:PM-M04}. The well
#'   assignment of these is fully identical to their non-custom counterparts,
#'   but separate plate types are nevertheless useful here to avoid comparing
#'   apples and oranges, as the scale of the measurements is totally different
#'   from OmniLog units.
#' @details The data set is identical to the supplement of Boccuto \emph{et al.}
#'   (2013) except for the following differences:\itemize{
#'   \item The measurements are not logarithmised.
#'   \item The negative controls are contained, hence the plates are complete.
#'   \item The individuals N1 to N15 are missing, hence the data set is reduced
#'   to autism-spectrum disorder patients and control group.
#'   }
#'   The data are point measurements, so a call to \code{\link{do_aggr}} would
#'   just copy the data. Two of the metadata entries are important,
#'   \sQuote{individual} for identifying the cell culture and \sQuote{group}
#'   for assigning the individuals to patients and control group, respectively.
#' @note Information provided by C.E. Schwartz and colleagues additional to the
#'   supplement of Boccuto \emph{et al.} (2013) is gratefully acknowledged.
#' @references Boccuto, L., Chen, C.-F., Pittman, A.R., Skinner, C.D.,
#'   McCartney, H.J., Jones, K., Bochner, B.R., Stevenson, R.E., Schwartz, C.E.
#'   2013. Decreased tryptophan metabolim in patients with autism spectrum
#'   disorder. \emph{Molecular Autism} \strong{4}: 16.
#' @references Schwartz, C.E., pers. comm.
#'
#' @examples \dontrun{
#'
#' # Calling this yielded a variable 'boccuto_et_al' containing the data. The
#' # opm package must be loaded beforehand using library().
#' data(boccuto_et_al)
#'
#' # Pseudo-aggregate the data (use a copy of each point measurement as
#' # maximum-height value).
#' boccuto_et_al <- do_aggr(boccuto_et_al)
#' }
#' # Copy the well maps of the pre-defined counterparts.
#' register_plate(`CUSTOM:PM-M01` = wells(plate = "PM-M01"))
#' register_plate(`CUSTOM:PM-M02` = wells(plate = "PM-M02"))
#' register_plate(`CUSTOM:PM-M03` = wells(plate = "PM-M03"))
#' register_plate(`CUSTOM:PM-M04` = wells(plate = "PM-M04"))
#' # Now the data would be ready for analysis.
#'
NULL


################################################################################
