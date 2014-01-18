

#' @import methods opm
#'
NULL


################################################################################
################################################################################
#
# Descriptions of all data sets included in the package
#


#' Example data set from Vaas et al. (2012)
#'
#' This \code{OPMS} object contains all measurements from the study by Vaas
#' \emph{et al.} (2012). Metadata have been added to fully describe the
#' conducted OmniLog\eqn{\textsuperscript{\textregistered}}{(R)} phenotype
#' microarray (\acronym{PM}) experiments. The plate type is \sQuote{Generation
#' III}, but the running mode was as for the usual \acronym{PM} plates. Four
#' bacterial strains from two species were considered in the study. For the
#' three publicly accessible ones, the web links to their \acronym{DSMZ}
#' catalogue entries are given below.
#'
#' @docType data
#' @keywords datasets
#' @name vaas_et_al
#' @format \code{OPMS} object with the dimensions 114 x 384 x 96, i.e. 114
#'   plates with 384 time points and 96 wells per plate. (10 plates have 364,
#'   365, 368 or 371 time points, respectively; the remaining 74 plates have 384
#'   time points).
#' @details All plates also contain aggregated and discretised values generated
#'   \emph{via} \code{do_aggr} and \code{do_disc} from the \pkg{opm} package
#'   under default values.
#'
#'   The data set \code{vaas_et_al} is a superset of the data sets \code{vaas_4}
#'   and \code{vaas_1} that come with the \pkg{opm} package. A particular subset
#'   of \code{vaas_et_al} comprising the third technical repetition in the first
#'   experimental run was used for creating Figure 1 in Vaas \emph{et al.}
#'   (2012). Code for extracting these plates (and specific wells) is given
#'   below. Please see also the description for the data set \code{vaas_4} in
#'   the \pkg{opm} package for further details regarding the Figures 2, 3 and 4
#'   in Vaas \emph{et al.} (2012). Figure 5 in Vaas \emph{et al.} (2012)
#'   represents the data for the \emph{E. coli} strain \acronym{DSM} 30083T from
#'   well D12 only but comprising all ten technical replicates from the first
#'   experimental run. Finally, the upper part of Figure 6 shows the data
#'   derived only from well C08 in the time-series experiment.
#'
#' @references Bochner, B.R., Savageau, M.A. 1977. Generalized indicator plate
#'   for genetic, metabolic, and taxonomic studies with microorganisms. \emph{
#'   Applied and Environmental Microbiology} \strong{33}, 434--444.
#' @references Selezska, K., Kazmierczak, M., Muesken, M., Garbe, J., Schobert,
#'   M., Haeussler, S., Wiehlmann, L., Rohde, C., Sikorski, J. 2012
#'   \emph{Pseudomonas aeruginosa} population structure revisited under
#'   environmental focus: impact of water quality and phage pressure.
#'   \emph{Environmental Microbiology} \strong{14}, 1952--1967
#'   (\url{http://dx.doi.org/10.1111/j.1462-2920.2012.02719.x}).
#' @references Vaas, L. A. I., Sikorski, J., Michael, V., Goeker, M., Klenk
#'   H.-P. 2012 Visualization and curve parameter estimation strategies for
#'   efficient exploration of Phenotype Microarray kinetics. \emph{PLoS ONE}
#'   \strong{7}, e34846 (\url{http://dx.doi.org/10.1371/journal.pone.0034846}).
#' @references
#'   \url{http://www.dsmz.de/catalogues/details/culture/DSM-1707.html}
#' @references
#'   \url{http://www.dsmz.de/catalogues/details/culture/DSM-18039.html}
#' @references
#'   \url{http://www.dsmz.de/catalogues/details/culture/DSM-30083.html}
#'
#' @examples
#' data(vaas_et_al)
#' plate_type(vaas_et_al) # should indicate generation-III plates
#' (d <- dim(vaas_et_al))
#' (ha <- has_aggr(vaas_et_al))
#' (hd <- has_disc(vaas_et_al))
#' stopifnot(d == c(114, 384, 96), ha, hd)
#'
#' \dontrun{
#' # get the data subset used in Figure 1 in Vaas et al. (2012)
#' vaas.1.3 <- subset(vaas_et_al, query = list(Experiment = "First replicate",
#'   'Plate number' = 3))
#'
#' # plot the data approximately as in Vaas et al. (2012)
#' xy_plot(vaas.1.3[, , c("D01", "D02", "C10", "C11")],
#'   main = "E. coli vs. P. aeruginosa", include = list("Species", "Strain"))
#' level_plot(vaas.1.3[, , c("D01", "D02", "C10", "C11")],
#'   main = "E. coli vs. P. aeruginosa", include = list("Species", "Strain"))
#' }
NULL


################################################################################


#' Example data set from Wittmann et al. (2014)
#'
#' This \code{OPMS} object contains the measurements used in the study by
#' Wittmann \emph{et al.} (2014). Metadata have been added to fully describe the
#' conducted OmniLog\eqn{\textsuperscript{\textregistered}}{(R)} phenotype
#' microarray (\acronym{PM}) experiments. The plate type is \sQuote{Generation
#' III}, but the running mode was as for the usual \acronym{PM} plates.
#'
#' @docType data
#' @keywords datasets
#' @name wittmann_et_al
#' @format \code{OPMS} object with the dimensions 41 x 382 x 96, i.e. 41 plates
#'   with about 382 time points and 96 wells per plate. (38 plates have 384 time
#'   points; three plates have 382 time points).
#' @details All plates also contain aggregated values generated \emph{via}
#'   \code{do_aggr} using the \code{"opm-fast"} method without bootstrapping.
#'
#'   For 62 strains of the opportunistic pathogenic bacterium
#'   \emph{Achromobacter xylosoxidans} \acronym{PM} experiments were conducted
#'   with Generation-III plates using inoculation fluid IF-A. All plates were
#'   incubated in the left plate sliding carriage of the
#'   OmniLog\eqn{\textsuperscript{\textregistered}}{(R)} reader.
#'
#'   In addition to \sQuote{genus}, \sQuote{species} and \sQuote{strain}, the
#'   following metadata entries are contained in the Wittmann \emph{et al.}
#'   \code{OPMS} object:
#'   \describe{
#'   \item{country}{Lists the geographical origin.}
#'   \item{city}{Lists the city where the strain was isolated.}
#'   \item{year}{Lists the year in which the strain was isolated.}
#'   \item{isolated_from}{Lists the scientist who isolated the strain.}
#'   \item{source}{Distinguishes between environmental or clinical origin, if
#'   known.}
#'   \item{habitat}{Lists details on the habitat, if known.}
#'   \item{replicate}{Lists the number of experimental plate replicates per
#'   strain. 2-3 replicates per strain were performed.}
#'   \item{MLSTcluster}{Name of phylogenetic cluster to which a strain is
#'   affiliated. See reference Wittmann \emph{et al}.}
#' }
#'
#' @references Bochner, B.R., Savageau, M.A. 1977. Generalized indicator plate
#'   for genetic, metabolic, and taxonomic studies with microorganisms. \emph{
#'   Applied and Environmental Microbiology} \strong{33}, 434--444.
#' @references Wittmann, J., Dreiseikelmann, B., Rohde, C., Rohde, M.,
#'   Sikorski, J. 2014 Isolation and characterization of numerous novel phages
#'   targeting diverse strains of the ubiquitious and opportunistic pathogen
#'   \emph{Achromobacter xylosoxidans}. \emph{PLoS ONE}, in press.
#'
#' @examples
#' data(wittmann_et_al)
#' plate_type(wittmann_et_al) # should indicate generation-III plates
#' (d <- dim(wittmann_et_al))
#' (ha <- has_aggr(wittmann_et_al))
#' stopifnot(d == c(41, 382, 96), ha)
#'
#' ## Brief overview of the metadata
#' head(to_metadata(wittmann_et_al))
#'
NULL


################################################################################

