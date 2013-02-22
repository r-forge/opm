################################################################################
#' Computation of Normalisation and CI for group-means
#'
#' Executes normalisation and/or computes normalized point-estimates and 
#' respective confidence intervals for user-defined experimental groups.
#'
#' @param object object Data frame, numeric matrix or \sQuote{OPMS} object (with
#'   aggregated values)
#'
#' @param grouping logical. If data should be grouped according metadata-columns
#'   given in \code{as.labels}
#'
#' @param as.labels \code{NULL} or character vector specifying the 
#'   factor-variables which should be used for grouping. If \code{NULL} and 
#'   \code{grouping = TRUE}, all available factor-varialbes are used. 
#'
#' @param norm.method character scalar to state the method for normalization. 
#'   See Details.
#'
#' @param x numerical vector for determining, which well(s)-means should be used
#'   as denominator for normalisation with \code{norm.method = plate.rat}.
#' 
#' @return 
#'   An object of class \code{data.frame} with columns for metadata specified in
#'   \code{as.labels}, one column \code{Parameter} and the numerical part of 96
#'   columns. If point estimators and the limits of corresponding confidence-
#'   intervals were computed, the \code{Parameter}-column harbours repetitively 
#'   the specifier "A", "A CI95 low" and "A CI95 high". Accordingly, in each 
#'   column of the numerical part each point-estimator for a group mean is 
#'   followed by its conficence-interval limits.
#' 
#' @keywords htest
#' 
#' @seealso boot::norm
#'
#' @details The function gives option to correct aggregated parameters   
#'   \itemze{
#'   \item \code{raw} applies no normalisations.
#'   \item \code{plate.sub} substracts the mean of the complete plate(s) from 
#'     each single value
#'   \item \code{plate.rat} divides each value by the value from one or more 
#'     wells, given by \code{x}.
#'   \item \code{well.sub} substracts the mean of wells and \code{well.rat} each 
#'     value by well means.
#'   }
#'   The returned object is of class \code{data.frame} and if grouping was 
#'   executed, it can be visualized using \code{ci_plot()} (see examples).
#'
#'
#' @examples
#' 
#' data preparation
#'
#' require(opmdata)
#' data(vaas_et_al)
#'
#' # select only the first replicate of the E.coli strains
#' vaas.test <- subset(vaas_et_al, 
#'   query = list(Experiment = "First replicate", Species = "Escherichia coli"))
#' # extract parameter A with strain, experiment, slot and species as metadata
#' vaas.test.A <- extract(vaas.test, 
#'   as.labels = list("Strain", "Experiment", "Slot", "Species"),
#'   subset = "A", dataframe = TRUE)
#' stopifnot(dim(vaas.test.A) == c(20L, 101L))
#' 
#' # simplest version: 
#' # no 'as.labels' specified, no grouping, no normalisation
#' x <- group_CI(vaas.test.A, grouping = FALSE, norm.method = NULL)
#' stopifnot(is.data.frame(x), identical(dim(x), c(20L, 101L)))
#' 
#' # grouping according all available metadata-columns, no normalisation
#' x <- group_CI(vaas.test.A, grouping = TRUE, as.labels = NULL, 
#'   norm.method = NULL)
#' stopifnot(is.data.frame(x), identical(dim(x), c(12L, 101L)))
#' stopifnot(is_ci_plottable(x))
#' 
#' # visualisation using ci_plot
#' message("plot #1")
#' ci_plot(x[, 1L:10L], legend.field = c(3L, 2L))
#' # note: the first five columns are factors, thus only four wells plotted
#' 
#' # with specified columns ('as.labels' given as character-string of the 
#' # column-names) for grouping (TRUE), normalisation by division ("plate.rat") 
#' # using well A10-values (positive control)
#' x <- group_CI(object = vaas.test.A, grouping = TRUE,
#'    as.labels = colnames(vaas.test.A[, 1L:3L]), norm.method = "plate.rat", 
#'    x = 10 )
#' stopifnot(is.data.frame(x), identical(dim(x), c(12L, 100L)))
#' stopifnot(is_ci_plottable(x))
#' 
#' # visualisation using ci_plot
#' message("plot #2")
#' ci_plot(x[, 4L:14L], vline = 1) # good
#' # note: the first four columns are factors, thus only six plots
#' # A10 all point-estimator have value 1, since the values of this well are 
#' # divided by themselves
#'

group_CI <- function(object, grouping = TRUE, as.labels = NULL,
      norm.method = c("plate.sub", "plate.rat", "well.sub", "well.rat", "raw"),
      x) {
  
  as.labels = as.labels
  # preparation:
  # state the position of the 'Parameter'-column
  dim.start <- dim(object)
  param.pos <- which(colnames(object) == "Parameter")
  
  # Give error-message, if data frame does not have the required structure
  # If more than exactly one 'Parameter'-column exists
  if (length(param.pos) != 1L)
    stop("need data frame with exactly one column called 'Parameter'")
  
  # Give warning, if no numerical variables exist or the 'Parameter'-column
  # is not at the required position
  if (param.pos == ncol(object))
    warning("no numerical data or 'Parameter'-column at wrong position")
  
  # Check, if only complete plates are taken into account
  if (ncol(object) - param.pos != 96L)
    warning("normalisation is not computed with plate-means")
  
  # function for normalisation
  norm_opm <- function(object, norm.method, x) {
    switch(norm.method,
           # by substracting the plate-means
           plate.sub = sweep(object, 1L, rowMeans(object)),
           # normalisation by ratio
           plate.rat = sweep(object, 1L, FUN = "/", object[, x]),
           # by substracting the well-means
           well.sub = sweep(object, 2L, colMeans(object)),
           # by ratio over well-means
           well.rat = sweep(object, 2L, FUN = "/", colMeans(object)),
           # without normalisation
           raw = object,
           stop("no action for arguments in norm_opm. DUSSEL!")
    )
  }
  
  # compute the normalisation
  pn <- norm_opm(object[, (param.pos + 1L):ncol(object), drop = FALSE],
                 match.arg(norm.method), x)
  
  # check, if result is of right dimension
  ## TODO: remove this once certain
  stopifnot(dim(object[, (param.pos + 1L):ncol(object)]) == dim(pn))
  
  # add the factorial part
  object <- cbind(object[, 1L:param.pos], pn)
  
  # check if right dimensions occur
  ## TODO: remove this once certain
  stopifnot(dim(object) == dim.start)
  
  ##################### end normalisation
  
  if (!grouping)
    return(object)
  
  # check, if 'as.labels' is specified; if not give warning and use all factorial
  # variables for grouping
  if (!is.null(as.labels)) {
    # give warning, if columns for grouping were stated double
    if (anyDuplicated(as.labels))
      warning("grouping variables are not unique")
    # check, if the entries for 'what' exist in the data frame
    bad <- which(!as.labels %in% colnames(object)[1L:param.pos])
    if (length(bad))
      stop("cannot find column name: ", as.labels[bad[1L]])
  } else
    as.labels <- colnames(object)[1L:param.pos]
  
  # make list from the factorial column-names
  # + length of the list
  gl <- length(group.facs <- lapply(as.labels, function(i) object[[i]]))
  
  # compute the means and variances concerning the stated grouping
  aggr.mean <- aggregate(
    object[, (param.pos + 1L):ncol(object), drop = FALSE],
    by = group.facs, FUN = mean)
  
  aggr.var <- aggregate(
    object[, (param.pos + 1L):ncol(object), drop = FALSE],
    by = group.facs, FUN = stats::var)
  
  # compute the CI for the group means
  aggr.CI <- boot::norm.ci(t0 = aggr.mean[, (gl + 1L):ncol(aggr.mean)],
                           var.t0 = aggr.var[, (gl + 1L):ncol(aggr.var)])
  
  # the output has to be organized in a certain structure:
  # three rows per group: first the mean, second the lower CI-limit third
  # the upper CI-limit
  
  # triple each row of the factors
  faccols <- as.data.frame(sapply(aggr.mean[, 1L:gl, drop = FALSE],
                                  rep, each = 3L))
  
  # add the column-names
  colnames(faccols) <- colnames(object[, as.labels, drop = FALSE])
  
  # add the new 'Parameter'-column required for the ci_plot()-function
  faccols$Parameter <- paste(object[1L, param.pos],
                             c("", " CI95 low", " CI95 high"), sep = "")
  
  # sort the numerical part of aggr_CI
  # preparation: numerical part as a matrix without the 'conf' column
  aggr.CInum <- data.matrix(aggr.CI[, 2L:ncol(aggr.CI), drop = FALSE])
  
  # prepare the means as a matrix without the factorial part
  aggr.mean.num <- data.matrix(aggr.mean[, (gl + 1L): ncol(aggr.mean),
                                         drop = FALSE])
  
  # prepare the matrix for numerical results
  outnum <- matrix(ncol = 3L * nrow(aggr.mean.num),
                   nrow = ncol(aggr.mean.num))
  
  # fill the matrix
  for (i in 1L:nrow(aggr.mean))
    outnum[, (i * 3L - 2L):(3L * i)] <- c(
      aggr.mean.num[i, ],
      aggr.CInum[i, 1L:(ncol(aggr.CInum) / 2L)],
      aggr.CInum[i, (ncol(aggr.CInum) / 2L + 1L):ncol(aggr.CInum)]
    )
  
  # transpose matrix into required dimensions
  outnum <- t(outnum)
  
  # checking the result
  ## TODO: remove this once certain
  stopifnot(dim(outnum) == c(3L * nrow(aggr.mean.num), ncol(aggr.mean.num)))
  
  # add the colnames
  colnames(outnum) <- colnames(aggr.mean.num)
  # add the factors
  as.data.frame(cbind(faccols, outnum))
  
}


################################################################################
