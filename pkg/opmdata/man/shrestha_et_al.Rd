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
  phenotype microarray (\acronym{PM}) experiments. The
  plate type is \sQuote{Generation III}, but the running
  mode was as for the usual \acronym{PM} plates.
}
\details{
  All plates also contain aggregated and discretised values
  generated \emph{via} \code{do_aggr} and \code{do_disc}
  from the \pkg{opm} package under default values.

  For each of two strains of the plant pathogenic bacterium
  \emph{Acidovorax citrulli}, M6 and 7a1, two phenotypic
  variants V1 and V2 were generated.  \acronym{PM}
  experiments were conducted in order to identify
  differences between the parental wild type and any of its
  two variant descendants.

  In addition to \sQuote{strain}, \sQuote{variant} and
  \sQuote{strain_variant}, the following metadata entries
  are contained in the Shrestha \emph{et al.} \code{OPMS}
  object: \describe{ \item{experiment}{Lists the
  experimental replicates \strong{zero}, \strong{first},
  \strong{second}, and \strong{third}.}
  \item{replicate}{Lists the technical replicates
  \strong{a} or \strong{b}. Technical replicates have
  precisely the same inoculum.} \item{slot}{Lists the left
  (\strong{A}) or right (\strong{B}) plate sliding carriage
  in the
  OmniLog\eqn{\textsuperscript{\textregistered}}{(R)}
  reader.} \item{growth_medium}{Lists the agar plate medium
  in which the bacteria were grown for inoculation,
  \strong{NB} or \strong{LB} medium, respectively.} }
}
\examples{
data(shrestha_et_al)
plate_type(shrestha_et_al) # should indicate generation-III plates
(d <- dim(shrestha_et_al))
(ha <- has_aggr(shrestha_et_al))
(hd <- has_disc(shrestha_et_al))
stopifnot(d == c(42, 378, 96), ha, hd)

## Brief overview
flatten(shrestha_et_al[,1,"A01"], include = list("experiment", "strain",
"variant", "replicate", "growth_medium","slot", "strain_variant"))
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

