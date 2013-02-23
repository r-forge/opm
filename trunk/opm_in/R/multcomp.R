

################################################################################


#' Multiple comparison of group means
#'
#' This function invokes functionality from \code{glht} (from package
#' \pkg{multcomp}) and thus provides linear hypothesis-testing and multiple
#' comparisons for group means of curve parameters.
#'
#' @param object \code{\link{OPMS}} object or data frame derived via
#'   \code{\link{extract}} with  specified labels (factor-variables) determining
#'   experimental groups for multiple comparison of means.
#'
#' @param as.labels \code{NULL}, list or character vector specifying the factor
#'   variables which determine the experimental groups to be compared. If
#'   \code{object} is of class \code{\link{OPMS}}, \code{as.labels} must not be
#'   \code{NULL}, but given as a list. If \code{object} is of class
#'   \code{data.frame}, \code{as.labels} can be given as a character vector, and
#'   by default all factor variables included in \code{object} are used.
#'
#' @param per.mcp Logical scalar that determines whether or not a multiple
#'   comparison of groups means should be performed. If \code{FALSE}, a reshaped
#'   data frame is returned which contains the variables given in
#'   \code{as.labels} as factors and can be used for more complex model building
#'   by the user user.
#'
#' @param model A symbolic description of the linear model to be fitted using
#'   \code{lm}. See \code{formula} for details (both in the \pkg{stats}
#'   package).
#'
#' @param m.type Character scalar indicating which of the following model types
#'   to use in model fitting: \sQuote{glm}, \sQuote{aov} or \sQuote{lm}.
#~ TODO LEA: describe the effect of this setting either or under details (then
#~ link this here) using an itemized list. Otherwise clearly state where these
#~ values are explained.
#'
#' @param mcp.def a specification of the linear hypotheses to be tested
#'   analogously to \code{linfct} in \code{glht}: Linear functions can be
#'   specified by either the matrix of coefficients or by symbolic descriptions
#'   of one or more linear hypotheses. See also \code{contrMat} from the
#'   \pkg{multcomp} package.
#'
#' @param sub.list Numerical vector determining whether instead of complete
#'   plates only a subset of substrates should be used for the comparisons.
#~ TODO LEA: explain what to do if no subselection should happen; is 'ignored
#~ if empty' correct?
#'
#' @return
#'   An object of class \sQuote{glht} with \code{print}, \code{summary},
#'   \code{confint}, \code{coef} and \code{vcov} methods being available. See
#'   \code{glht} for details.
#~ TODO LEA: explain what is returned if per.mcp is FALSE
#'
#' @keywords htest
#' @export
#'
#' @family multcomp-functions
#' @seealso multcomp::glht stats::lm stats::formula
#'
#' @details This function provides suitably formatted data-matrix
#'   (flat-file-format) to enable linear model building and user defined
#'   simultaneous multiple testing procedures.
#~ TODO LEA: doesn't his depend on per.mcp?
#'
#~ hier beschreiben worauf man achten sollte
#~ wie man die kontrastmatrix definiert
#~ aufpassen bei der modellwahl -> bisher nur lineare modelle moeglich
#~ nicht zu viele gruppen zulassen -> warnung einbauen
#'
#'
#' @examples
#'
#' ## OPMS method
#' data("vaas_4")
#'
#' # Without computation of multiple comparisons of means
#' (xx <- opm_mcp(vaas_4, as.labels = list("Species", "Strain"),
#'   per.mcp = FALSE))
#' (stopifnot(dim(xx) == c(384L, 6L)))
#'
#' # comparison with specified model
#' (xx <- opm_mcp(vaas_4, as.labels = list("Species"), m.type = "lm"))
#' (stopifnot(class(xx) == "glht"))
#'
#'
#' # comparisons of Species pooled over complete plates
#' (xx <- opm_mcp(vaas_4, as.labels = list("Species"), m.type ="lm",
#'   mcp.def = mcp(Species = "Dunnett")))
#' (stopifnot(class(xx) == "glht", length(coef(x)) == 1)) 
#'
#'  # plot-method is available
#' op <- par(no.readonly = TRUE) # default plotting settings
#' par(mar = c(3, 15, 3, 2))
#' plot(xx)
#' par(op) # reset plotting settings
#'
#'
#' # comparison of only A01 - A04 against the intercept
#' (xx <- opm_mcp(vaas_4, as.labels = list("Species", "Strain"),
#'   sub.list = c(1:4), model = "Value ~ Well + Species", m.type = "lm"))
#' (stopifnot(class(xx) == "glht", length(coef(x)) == 5))
#' 
#' # plot-method is available
#' op <- par(no.readonly = TRUE) # default plotting settings
#' par(mar = c(3, 15, 3, 2))
#' plot(xx)
#' par(op) # reset plotting settings
#'
#' 
#' # user defined a contrastmatrix
#' a <- mcp(Well = "Dunnett")
#'   (xx <- opm_mcp(vaas_4, as.labels = list("Species", "Strain"),
#'   sub.list = c(1:4), m.type = "lm", mcp.def = a, model = "Value ~ Well"))
#' (stopifnot(class(xx) == "glht", length(coef(x)) == 3))
#' 
#' # plot method
#' op <- par(no.readonly = TRUE) # default plotting settings
#' par(mar = c(3, 20, 3, 2))
#' plot(xx)
#' par(op) # reset plotting settings
#' 
#'
#' ## matrix method
#' x <- extract(vaas_4, as.labels = list("Species", "Strain"), subset = "A",
#'   dataframe = TRUE)
#'   
#' (xx <- opm_mcp(x, as.labels = c("Species"), m.type = "lm"))
#' (stopifnot(class(xx) == "glht", length(coef(xx)) == 2))
#' 
#' # plot method is available
#' op <- par(no.readonly = TRUE) # default plotting settings
#' par(mar = c(3, 15, 3, 2))
#' plot(xx)
#' par(op) # reset plotting settings
#' 
#' 
#' # without performing the MCP
#' xx <- opm_mcp(x, per.mcp = FALSE)
#'
#' # testing for subsets of object
#' (xx <- opm_mcp(subset(x, Species == "Escherichia coli"),
#'   mcp.def = mcp(Strain = "Dunnett"), as.labels = c("Strain"), m.type = "lm"))
#' (stopifnot(class(xx) == "glht", length(coef(xx)) == 1))
#' 
#' # plot method available
#' op <- par(no.readonly = TRUE) # default plotting settings
#' par(mar = c(3, 15, 3, 2))
#' plot(xx)
#' par(op) # reset plotting settings
#' 

opm_mcp <- function(object, model, mcp.def, as.labels = NULL, per.mcp = TRUE, 
  m.type = c("glm", "lm", "aov"), sub.list = NULL) {
  ## TODO LEA: currently args without default AFTER args with default -- please
  ## fix (and see below)

  if (!suppressWarnings(require(
      multcomp, quietly = TRUE, warn.conflicts = FALSE)))
    stop("package 'multcomp' must be available to run this function")

  # matrix or OPMS input
  ## TODO:  must be later on changed to S4 method
  if (inherits(object, "OPMS")) {
    if (length(as.labels) < 1L) {
      stop("'as.labels'is missing")
    }
    object <- extract(object, as.labels = as.labels, subset = "A",
              dataframe = TRUE)
  }
 
  param.pos <- which(colnames(object) == "Parameter")

  # Give error-message, if dataframe does not have the required structure
  # If more than exactly one 'Parameter'-column exists
  if (length(param.pos) != 1L)
    stop("need data frame with exactly one column called 'Parameter'")

  # Give warning, if no numerical variables exist or the 'Parameter'-column
  # is not at the required position
  ## TODO LEA: Would this crash later on? Then warning was unnecessary.
  if (param.pos == ncol(object))
    warning("no numerical data or 'Parameter'-column at wrong position")

  # Check, if only complete plates are taken into account
  ## TODO LEA: is this necessary?
  if (ncol(object) - param.pos != 96L)
    message("comparisons for only a subset of the plates")

  # whole plate or subset of wells for comparisons?
  if (length(sub.list))
    object <- object[, c(1L:param.pos, sub.list + param.pos)]

  # include like the what-argument
  cnames <- colnames(object[, 1L:param.pos])

  # check, if 'as.labels' is specified; if not give warning and use all
  # factorial variables for model-building

  ## TODO LEA: where is this check?

  # check if 'as.labels' has more than one level
  # give warning, if columns for grouping were stated double
  if (anyDuplicated(as.labels))
    warning("x-variable(s) are not unique")

  # check, if the entries for include exist in the dataframe
  includecol <- colnames(object)[1L:param.pos]   #- 1L
  bad <- which(!as.labels %in% includecol)

  if (length(bad))
    stop("cannot find column name: ", as.labels[bad[1L]])

  # reshape dataframe

  # helper-variable to avoid non-unique values when setting 'row.names'
  idhelper <- c(1L:length(object[, 1L]))
  dataframe1 <- cbind(idhelper, object)

  # reshaping
  result <- reshape(dataframe1,
    direction = "long",
    idvar = colnames(dataframe1[, 1L:param.pos]),
    varying = colnames(dataframe1[, c(param.pos + 2L):length(dataframe1)]),
    v.names = "Value",
    timevar = "Well",
    times = colnames(dataframe1[, (param.pos + 2L):length(dataframe1)]))
  rownames(result) <- NULL

  # factorial columns of 'result'
  result$Well <- as.factor(result$Well)
  
  if (!per.mcp)
    return(result)
  
  well.pos <- which(colnames(result) == "Well")
  
  # count number of levels of the factors
  param.pos.re <- which(colnames(object) == "Parameter")

  ma.level <- matrix(ncol = 1L,
    nrow = ncol(result[, c(c(1L:c(param.pos.re - 1L), param.pos.re:well.pos))]))
  for (i in 1L:ncol(result[, 1L:well.pos]))
    ma.level[i, ] <- nlevels(result [, i])

  # check for one-level-factors
  level.one <- which(ma.level > 1L)
  xnames <- colnames(result[, level.one])

  ## TODO LEA: where are these checks?
  
  #check if as.labels has more than one factor
  if (length(as.labels)) {
    bad <- which(!as.labels %in% xnames)
    if (length(bad))
      stop("Only one level for factor-variable(s): ",
        paste(as.labels[bad[1L:length(bad)]], collapse = ","))
  }
  # model-fitting
  # default-model
  fmla <- as.formula(paste("Value ~ ", paste(as.labels, collapse = "+")))
  ## TODO LEA: why not set this as argument default?
  # formel kann ja nur mit vlaue anfangen!
  
  
  # model-statement
  if (missing(model)) {
    model <- fmla 
    message("'model'is not specified; all variables in 'as.labels' are used")
    ## TODO LEA: message necessary?
  }

  # fitting the linear model acording m.type
  lmmod <- case(match.arg(m.type),
    lm = lm(model, data = result),
    aov = aov(model, data = result),
    glm = glm(model, data = result)
  )
  
  # and compute the comparisons multiple testing
  mcp.result <- glht(lmmod, linfct = mcp.def)

  # check the number of calculated tests
  if (length(confint(mcp.result)$confint[, 1L]) > 20L)
    warning("number of performed comparisons exceeds 20")
  ## TODO LEA: what's the problem with >20? No power anymore?

  ## TODO LEA: cat necessary?
  # soll da ueberhaupt was ausgegeben werden?
  # lieber in den beispielen zeigen, wie man an diese infos kommt
  cat("
      Resulting model from dataset", deparse(substitute(dataframe)),
      "is:", deparse(model), "
      ")
  mcp.result
}


################################################################################



