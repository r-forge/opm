\docType{data}
\name{wittmann_et_al}
\alias{wittmann_et_al}
\title{Example data set from Wittmann et al. (2014)}
\format{\code{OPMS} object with the dimensions 41 x 382 x 96, i.e. 41 plates
  with about 382 time points and 96 wells per plate. (38 plates have 384 time
  points; three plates have 382 time points).}
\description{
  This \code{OPMS} object contains the measurements used in
  the study by Wittmann \emph{et al.} (2014). Metadata have
  been added to fully describe the conducted
  OmniLog\eqn{\textsuperscript{\textregistered}}{(R)}
  phenotype microarray (\acronym{PM}) experiments. The
  plate type is \sQuote{Generation III}, but the running
  mode was as for the usual \acronym{PM} plates.
}
\details{
  All plates also contain aggregated values generated
  \emph{via} \code{do_aggr} using the \code{"opm-fast"}
  method without bootstrapping.

  For 62 strains of the opportunistic pathogenic bacterium
  \emph{Achromobacter xylosoxidans} \acronym{PM}
  experiments were conducted with Generation-III plates
  using inoculation fluid IF-A. All plates were incubated
  in the left plate sliding carriage of the
  OmniLog\eqn{\textsuperscript{\textregistered}}{(R)}
  reader.

  In addition to \sQuote{genus}, \sQuote{species} and
  \sQuote{strain}, the following metadata entries are
  contained in the \code{wittmann_et_al} \code{OPMS}
  object: \describe{ \item{country}{Lists the geographical
  origin.} \item{city}{Lists the city where the strain was
  isolated.} \item{year}{Lists the year in which the strain
  was isolated.} \item{isolated_from}{Lists the scientist
  who isolated the strain.} \item{source}{Distinguishes
  between environmental or clinical origin, if known.}
  \item{habitat}{Lists details on the habitat, if known.}
  \item{replicate}{Lists the number of experimental plate
  replicates per strain. 2-3 replicates per strain were
  performed.} \item{MLSTcluster}{Name of phylogenetic
  cluster to which a strain is affiliated. See reference
  Wittmann \emph{et al} (2014).} }
}
\examples{
data(wittmann_et_al)
plate_type(wittmann_et_al) # should indicate generation-III plates
(d <- dim(wittmann_et_al))
(ha <- has_aggr(wittmann_et_al))
stopifnot(d == c(41, 382, 96), ha)

## Brief overview of the metadata
head(to_metadata(wittmann_et_al))
}
\references{
  Bochner, B.R., Savageau, M.A. 1977. Generalized indicator
  plate for genetic, metabolic, and taxonomic studies with
  microorganisms. \emph{ Applied and Environmental
  Microbiology} \strong{33}, 434--444.

  Wittmann, J., Dreiseikelmann, B., Rohde, C., Rohde, M.,
  Sikorski, J. 2014 Isolation and characterization of
  numerous novel phages targeting diverse strains of the
  ubiquitious and opportunistic pathogen
  \emph{Achromobacter xylosoxidans}. \emph{PLoS ONE}
  \strong{9}, e86935.
}
\keyword{datasets}
\keyword{datasets}

