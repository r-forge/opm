\docType{data}
\name{shrestha_et_al}
\alias{shrestha_et_al}
\title{Example data set from Shrestha et al. (2013)}
\format{\code{OPMS} object with the dimensions 42 x 378 x 96, i.e. 42 plates
  with 378 time points and 96 wells per plate. (17 plates have 376, 378 or
  383 time points, respectively; the remaining 25 plates have 384 time
  points).}
\description{
  This \code{OPMS} object contains all measurements from
  the study by Shrestha \emph{et al.} (2013). Metadata have
  been added to fully describe the conducted
  OmniLog\eqn{\textsuperscript{\textregistered}}{(R)}
  phenotype microarray (\sQuote{PM}) experiments. The plate
  type is \sQuote{Generation III}, but the running mode was
  as for the usual \sQuote{PM} plates. \strong{[Further
  details to be added by Johannes Sikorski]}.
}
\details{
  All plates also contain aggregated and discretised values
  generated \emph{via} \code{do_aggr} and \code{do_disc}
  from the \pkg{opm} package under default values.
}
\examples{
data(shrestha_et_al)
plate_type(shrestha_et_al) # should indicate generation-III plates
(d <- dim(shrestha_et_al))
(ha <- has_aggr(shrestha_et_al))
(hd <- has_disc(shrestha_et_al))
stopifnot(d == c(42, 378, 96), ha, hd)
}
\references{
  Bochner, B.R., Savageau, M.A. 1977. Generalized indicator
  plate for genetic, metabolic, and taxonomic studies with
  microorganisms. \emph{ Applied and Environmental
  Microbiology} \strong{33}, 434--444.

  Shrestha, R.K., Rosenberg, T., Makarovsky, D.,
  Eckshtain-Levi, N., Zelinger, E., Kopelowitz, J.,
  Sikorski, J., Burdman, S. 2013 Phenotypic variation in
  the plant pathogenic bacterium \emph{Acidovorax
  citrulli}.  \emph{PLoS ONE} \strong{8}, e73189
  (\url{http://dx.doi.org/10.1371/journal.pone.0073189}).
}
\keyword{datasets}
\keyword{datasets}

