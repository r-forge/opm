

################################################################################
################################################################################
#
# Package description and imports
#


#' The opmDB package
#'
#' Package for database I/O of OmniLog Phenotype MicroArray data.
#'
#' @name opmDB-package
#' @docType package
#'
#' @details This package extends the \pkg{opm} package with functions for
#' writing phenotype microarray data to databases and reading from databases.
#' For the underlying type of data, see the documentation of \pkg{opm}.
#'
#' For the principle of conversion to database tables, see the virtual class
#' \code{\link{DBTABLES}} and its methods, \code{\link{DBTABLES-methods}}.
#'
#' For the application of these conversions to \pkg{opm} data objects, see the
#' \code{\link{OPM_DB}} class.
#' @keywords package
#'
NULL

#' @importFrom stats update
#'
NULL

#' @import methods opm
#'
NULL

#' @importFrom rjson toJSON fromJSON
#'
NULL
