\docType{data}
\name{montero_et_al}
\alias{montero_et_al}
\title{Example data set from Montero-Calasanz et al. (2013)}
\format{\code{OPMS} object with the dimensions 12 x 382 x 96, i.e. twelve
  plates with about 382 time points and 96 wells per plate. (Three quartets
  of plates with 292, 380 or 382 time points, respectively).}
\description{
  This \code{OPMS} object contains a selection of the
  measurements used in two studies by Montero-Calasanz
  \emph{et al.} (2013). A minimal set of metadata has been
  added to describe the conducted
  OmniLog\eqn{\textsuperscript{\textregistered}}{(R)}
  phenotype microarray (\acronym{PM}) experiments as far as
  necessary (see below). The plate type is
  \sQuote{Generation III}, but the running mode was as for
  the usual \acronym{PM} plates.
}
\details{
  The plates do not contain aggregated values generated
  \emph{via} \code{do_aggr}. Rather, they serve as an
  exemplar for merging subsequent runs of the same physical
  plate prior to estimating curve parameters. See the
  example section below.

  The metadata comprise the entries \sQuote{Strain} and
  \sQuote{Replicate}, which describe all aspects necessary
  for serving as an exemplar. The data comprise three
  subsequent runs for each of the two replicates for each
  of the two strains, hence twelve plates. The two strains
  are the type strains of \emph{Geodermatophilus telluris}
  (\acronym{DSM} 45421) and \emph{Geodermatophilus
  tzadiensis} (\acronym{DSM} 45416), respectively.
}
\examples{
data(montero_et_al)
plate_type(montero_et_al) # should indicate generation-III plates
(d <- dim(montero_et_al))
(ha <- has_aggr(montero_et_al))
stopifnot(!ha, d == c(12, 382, 96))

## Overview of the metadata
to_metadata(montero_et_al)

## Splitting, merging, joining
x <- opms(lapply(split(montero_et_al, ~ Strain + Replicate), merge))
# That's it. Other datasets might need an adaption of the parsing of the
# setup time, and of course might contain other combinations of metadata
# that identify the physical plate.
x
stopifnot(is(x, "OPMS"), length(x) == 4)
\dontrun{
  xy_plot(x, include = ~ Strain + Replicate)
}
}
\references{
  Bochner, B.R., Savageau, M.A. 1977. Generalized indicator
  plate for genetic, metabolic, and taxonomic studies with
  microorganisms. \emph{ Applied and Environmental
  Microbiology} \strong{33}, 434--444.

  Montero-Calasanz, M.C., Goeker, M., Poetter, G., Rohde,
  M., Sproeer, C., Schumann, P., Gorbushina, A.A., Klenk,
  H.-P. 2013 \emph{Geodermatophilus telluris} sp. nov., a
  novel actinomycete isolated from Saharan desert sand in
  Chad. \emph{International Journal of Systematic and
  Evolutionary Microbiology} \strong{13}, 2254--2259.

  Montero-Calasanz, M.C., Goeker, M., Broughton, W.J.,
  Cattaneo, A., Favet, J., Poetter, G., Rohde, M., Sproeer,
  C., Schumann, P., Gorbushina, A.A., Klenk, H.-P. 2013
  \emph{Geodermatophilus tzadiensis} sp.  nov., isolated
  Sahara desert sand in Chad. \emph{Systematic and Applied
  Microbiology} \strong{36}, 177--182.

  \url{http://www.dsmz.de/catalogues/details/culture/DSM-45416.html}

  \url{http://www.dsmz.de/catalogues/details/culture/DSM-45421.html}
}
\keyword{datasets}
\keyword{datasets}

