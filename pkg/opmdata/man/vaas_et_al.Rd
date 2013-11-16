\docType{data}
\name{vaas_et_al}
\alias{vaas_et_al}
\title{Example data set from Vaas et al. (2012)}
\format{\code{OPMS} object with the dimensions 114 x 384 x 96, i.e. 114
  plates with 384 time points and 96 wells per plate. (10 plates have 364,
  365, 368 or 371 time points, respectively; the remaining 74 plates have 384
  time points).}
\description{
  This \code{OPMS} object contains all measurements from
  the study by Vaas \emph{et al.} (2012). Metadata have
  been added to fully describe the conducted
  OmniLog\eqn{\textsuperscript{\textregistered}}{(R)}
  phenotype microarray (\acronym{PM}) experiments. The
  plate type is \sQuote{Generation III}, but the running
  mode was as for the usual \acronym{PM} plates. Four
  bacterial strains from two species were considered in the
  study. For the three publicly accessible ones, the web
  links to their \acronym{DSMZ} catalogue entries are given
  below.
}
\details{
  All plates also contain aggregated and discretised values
  generated \emph{via} \code{do_aggr} and \code{do_disc}
  from the \pkg{opm} package under default values.
}
\examples{
data(vaas_et_al)
plate_type(vaas_et_al) # should indicate generation-III plates
(d <- dim(vaas_et_al))
(ha <- has_aggr(vaas_et_al))
(hd <- has_disc(vaas_et_al))
stopifnot(d == c(114, 384, 96), ha, hd)
}
\references{
  Bochner, B.R., Savageau, M.A. 1977. Generalized indicator
  plate for genetic, metabolic, and taxonomic studies with
  microorganisms. \emph{ Applied and Environmental
  Microbiology} \strong{33}, 434--444.

  Selezska, K., Kazmierczak, M., Muesken, M., Garbe, J.,
  Schobert, M., Haeussler, S., Wiehlmann, L., Rohde, C.,
  Sikorski, J. 2012 \emph{Pseudomonas aeruginosa}
  population structure revisited under environmental focus:
  impact of water quality and phage pressure.
  \emph{Environmental Microbiology} \strong{14}, 1952--1967
  (\url{http://dx.doi.org/10.1111/j.1462-2920.2012.02719.x}).

  Vaas, L. A. I., Sikorski, J., Michael, V., Goeker, M.,
  Klenk H.-P. 2012 Visualization and curve parameter
  estimation strategies for efficient exploration of
  Phenotype Microarray kinetics. \emph{PLoS ONE}
  \strong{7}, e34846
  (\url{http://dx.doi.org/10.1371/journal.pone.0034846}).

  \url{http://www.dsmz.de/catalogues/details/culture/DSM-1707.html}

  \url{http://www.dsmz.de/catalogues/details/culture/DSM-18039.html}

  \url{http://www.dsmz.de/catalogues/details/culture/DSM-30083.html}
}
\keyword{datasets}

