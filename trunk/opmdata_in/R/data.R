
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
#'   The data sets \code{vaas_1} and \code{vaas_4} are subsets of
#'   \code{vaas_et_al} and come along with package \pkg{opm}. The data set contains,
#'   e.g., the measurements used for Figure 1 in Vaas \emph{et al.} (2012). This
#'   particular subset comprises the third technical repetition of the first
#'   experimental run. Code for extracting these plates and wells is given
#'   below.
#'   See description for \code{vaas_4} in \pkg{opm} for further details.
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
#' xy_plot(vaas.1.3[, , c("D01", "D02" ,"C10", "C11")],
#'   main = "E. coli vs. P. aeruginosa", include = list("Species", "Strain"))
#' level_plot(vaas.1.3[, , c("D01", "D02" ,"C10", "C11")],
#'   main = "E. coli vs. P. aeruginosa", include = list("Species", "Strain"))
#' }
NULL


################################################################################


#' Example data set from Shrestha et al. (2013)
#'
#' This \code{OPMS} object contains all measurements from the study by Shrestha
#' \emph{et al.} (2013). Metadata have been added to fully describe the
#' conducted OmniLog\eqn{\textsuperscript{\textregistered}}{(R)} phenotype
#' microarray (\sQuote{PM}) experiments. The plate type is \sQuote{Generation
#' III}, but the running mode was as for the usual \sQuote{PM} plates.
#' \strong{[Further details to be added by Johannes Sikorski]}.
#'
#' @docType data
#' @keywords datasets
#' @name shrestha_et_al
#' @format \code{OPMS} object with the dimensions 42 x 378 x 96, i.e. 42 plates
#'   with 378 time points and 96 wells per plate. (17 plates have 376, 378 or
#'   383 time points, respectively; the remaining 25 plates have 384 time
#'   points).
#' @details All plates also contain aggregated and discretised values generated
#'   \emph{via} \code{do_aggr} and \code{do_disc} from the \pkg{opm} package
#'   under default values.
#' @references Bochner, B.R., Savageau, M.A. 1977. Generalized indicator plate
#'   for genetic, metabolic, and taxonomic studies with microorganisms. \emph{
#'   Applied and Environmental Microbiology} \strong{33}, 434--444.
#' @references Shrestha, R.K., Rosenberg, T., Makarovsky, D., Eckshtain-Levi,
#'   N., Zelinger, E., Kopelowitz, J., Sikorski, J., Burdman, S. 2013 Phenotypic
#'   variation in the plant pathogenic bacterium \emph{Acidovorax citrulli}.
#'   \emph{PLoS ONE} \strong{8}, e73189
#'   (\url{http://dx.doi.org/10.1371/journal.pone.0073189}).
#'
#' @examples
#' data(shrestha_et_al)
#' plate_type(shrestha_et_al) # should indicate generation-III plates
#' (d <- dim(shrestha_et_al))
#' (ha <- has_aggr(shrestha_et_al))
#' (hd <- has_disc(shrestha_et_al))
#' stopifnot(d == c(42, 378, 96), ha, hd)
#'
NULL

################################################################################

