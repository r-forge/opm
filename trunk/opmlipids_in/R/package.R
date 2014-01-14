


#' The \pkg{opmlipids} package
#'
#' This packages uses the \pkg{opm} infrastructure (particularly the facilities
#' for storing and querying arbitrary metadata) for analysing fatty-acid
#' measurements.
#'
#' @name opmlipids
#' @docType package
#'
#' @details Here is a brief guideline for using this manual. In addition to this
#'   manual, code examples accessible via \code{demo} are available together
#'   with the package.
#' \describe{
#'
#'   \item{families}{All functions and methods belong to a family of functions
#'   and methods with similar purposes. The respective other family members are
#'   found in each \sQuote{See Also} entry.}
#'
#'   \item{classes}{Users normally will create at least one object of the class
#'   \code{\link{FAMES}}. An example object is available via
#'   \code{\link{DSM_44549}}.}
#'
#'   \item{input}{Most \pkg{opmlipids} users will start by inputting data using
#'   \code{\link{read_rtf}}, which create the appropriate objects.}
#' }
#'
#' @references \url{http://opm.dsmz.de/}
#' @keywords package
#' @examples
#' # demos that come with the package
#' if (interactive())
#'   demo(package = "opmlipids")
#'
#' # example input files that come with the package
#' pkgutils::pkg_files("opmlipids", "testdata")
#'
#' # list all classes, methods and functions exported by the package
#' ls("package:opmlipids")
#'
NULL

