################################################################################


#' Correlate wells
#'
#' This calculates all-against-all correlations between selected parameters
#' inferred from the contained wells or correlations between all wells and
#' selected wells of interest.
#'
#' @param x \code{\link{OPMS}} or \code{\link{MOPMX}} object. It is an error if
#'   the \code{x} contains non-aggregated plates and if \code{\link{MOPMX}}
#'   object contains \code{\link{OPM}} objects.
#' @param y Optional vector with names or indexes of columns to correlate with
#'   the full extracted matrix. Note that the names must refer to this generated
#'   matrix, not to the well names in \code{x}.
#' @param method Character scalar passed to \code{cor} from the \pkg{stats}
#'   package.
#' @param use Character scalar also passed to \code{cor}.
#' @param subset Character scalar passed to \code{\link{extract}}.
#' @param ... Optional arguments passed to \code{\link{extract}}.
#' @export
#' @return For the \code{\link{OPMS}} method, a squared matrix of correlations
#'   if \code{y} is empty, another matrix with correlations otherwise. The
#'   \code{\link{MOPMX}} method returns a list of such matrices.
#' @details The resulting matrix can be plotted, e.g., using the \code{corrplot}
#'   function from the \pkg{corrplot} package.
#' @family plotting-functions
#' @keywords regression
#' @seealso stats::cor stats::cor.test corrplot::corrplot
#' @examples
#' (x <- cor.test(vaas_4[, , 1:20]))
#' stopifnot(is.matrix(x), dim(x) == 20)
#' # adapt default text size to a full 96-well plate
#' my_corrplot <- function(corr, tl.col = "grey30", tl.cex = 0.4,
#'     mar = c(2, 2, 2, 2), ...) {
#'   corrplot::corrplot(corr = corr, tl.col = tl.col,
#'     tl.cex = tl.cex, mar = mar, ...)
#' }
#' my_corrplot(x)
#'
setGeneric("cor.test")

setMethod("cor.test", "OPMS", function(x, y = NULL, method = "pearson",
    use = "everything", subset = opm_opt("curve.param"), ...) {
  x <- extract(object = x, subset = subset, dataframe = FALSE, ci = FALSE,
    as.groups = NULL, as.labels = NULL, ...)
  if (length(y))
    y <- x[, y, drop = FALSE]
  cor(x, y, use, method)
}, sealed = SEALED)

setMethod("cor.test", "MOPMX", function(x, ...) {
  oapply(object = x, fun = cor.test, simplify = FALSE, ...)
}, sealed = SEALED)


################################################################################

