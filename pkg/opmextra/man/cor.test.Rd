\name{cor.test}
\alias{cor.test}
\title{Correlate wells}
\arguments{
  \item{x}{\code{\link{OPMS}} or \code{\link{MOPMX}}
  object. It is an error if the \code{x} contains
  non-aggregated plates and if \code{\link{MOPMX}} object
  contains \code{\link{OPM}} objects.}

  \item{y}{Optional vector with names or indexes of columns
  to correlate with the full extracted matrix. Note that
  the names must refer to this generated matrix, not to the
  well names in \code{x}.}

  \item{method}{Character scalar passed to \code{cor} from
  the \pkg{stats} package.}

  \item{use}{Character scalar also passed to \code{cor}.}

  \item{subset}{Character scalar passed to
  \code{\link{extract}}.}

  \item{...}{Optional arguments passed to
  \code{\link{extract}}.}
}
\value{
  For the \code{\link{OPMS}} method, a squared matrix of
  correlations if \code{y} is empty, another matrix with
  correlations otherwise. The \code{\link{MOPMX}} method
  returns a list of such matrices.
}
\description{
  This calculates all-against-all correlations between
  selected parameters inferred from the contained wells or
  correlations between all wells and selected wells of
  interest.
}
\details{
  The resulting matrix can be plotted, e.g., using the
  \code{corrplot} function from the \pkg{corrplot} package.
}
\examples{
(x <- cor.test(vaas_4[, , 1:20]))
stopifnot(is.matrix(x), dim(x) == 20)
# adapt default text size to a full 96-well plate
my_corrplot <- function(corr, tl.col = "grey30", tl.cex = 0.4,
    mar = c(2, 2, 2, 2), ...) {
  corrplot::corrplot(corr = corr, tl.col = tl.col,
    tl.cex = tl.cex, mar = mar, ...)
}
my_corrplot(x)
}
\seealso{
  stats::cor stats::cor.test corrplot::corrplot

  Other res: \code{\link{opm_dbclear}},
  \code{\link{opm_dbfind}}, \code{\link{opm_dbget}},
  \code{\link{opm_dbnext}}, \code{\link{opm_dbput}}
}
\keyword{regression}

%% BEGIN INSERTION BY repair_S4_docu.rb
\docType{methods}
\alias{cor.test-methods}
\alias{cor.test,MOPMX-method}
\alias{cor.test,OPMS-method}
\usage{
  \S4method{cor.test}{MOPMX}(x, ...) 
  \S4method{cor.test}{OPMS}(x, y = NULL, method = "pearson",
    use = "everything", subset = opm_opt("curve.param"), ...) 
}
%% END INSERTION BY repair_S4_docu.rb
