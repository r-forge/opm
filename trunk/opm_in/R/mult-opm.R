
library(opm)
library(multcomp)



################################################################################
#' Multiple Comparison of Group Means 
#' 
#' This function invokes funktionality from \code{glht} (from package 
#'\pkg{multcomp}) and thus provides linear hypotheses testing and multiple 
#' comparisons for group means of curve parameter derived via \code{extract}.
#'
#' @param object OPMS objetc or Data frame derived via \code{extract} with 
#'   specified labels (factor-variables) determining experimental groups for 
#'   multiple comparison of means.
#' 
#' @param as.lables \code{NULL}, list or character vector specifying the 
#'   factor-variables which determine the experimental groups to be compared. If
#'   \code{object} is of class \code{OPMS} \code{as.lables} must not be 
#'   \code{Null}, but given as a list. If \code{object} is of class 
#'   \code{data.frame}, \code{as.labels} can be given as a character vector. 
#'   Per default all factor-variables given in \code{object} are used.
#' 
#' @param per.mcp logical determines, if multiple comparison of groups means 
#'   should be performed. if FALSE a reshaped dataframe is returned which 
#'   contains the variables given in \code{as.labels} as factors and can be used
#'   for user more complex model building. 
#' 
#' @param model  a symbolic description of the linear model using \code{lm()} 
#'   to be fitted. See \code{\link{formula}} for details.
#' 
#' @param m.type character to state which one of the following model-types to be
#'   used for model-fitting: \code{lm, aov}
#' 
#' @param mcp.def a specification of the linear hypotheses to be tested 
#'   analogously to \code{linfct} in \code{glht}: Linear functions can be 
#'   specified by either the matrix of coefficients or by symbolic descriptions 
#'   of one or more linear hypotheses. See also \code{\link{contrMat}}.
#' 
#' @param sub.list numerical vector determining, if instead of complete plates 
#'   only a subset of substrates should be used for the comparisons.
#' 
#' @return 
#'   An object of class \code{glht} with \code{\link{print}}-, 
#'   \code{\link{summary}}-, \code{\link{confint}}-, \code{\link{coef}}- and 
#'   \code{\link{vcov}}-methods being available. See \code{\link{glht}} for 
#'   details.
#' 
#' @keywords htest
#' 
#' @seealso multcomp::glht
#' 
#' @details 
#'   Provides suitably formatted data-matrix (flat-file-format) to enable linear
#'   model building and user defined simultaneous multiple testing procedures.
#' 
#' hier beschreiben worauf man achten sollte
#' wie man die kontrastmatrix definiert
#' aufpassen bei der modellwahl -> bisher nur lineare modelle moeglich
#' nicht zu viele gruppen zulassen -> warnung einbauen
#' 
#' 
#' @examples
#' 
#' # OPMS method
#' data("vaas_4")
#' 
#' # Without computation of multiple comparisons of means
#' (xx <- opm_mcp(vaas_4, as.labels = list("Species", "Strain"), 
#'   per.mcp = FALSE))
#'   
#' # comparison with specified model
#' (xx <- opm_mcp(vaas_4, as.labels = list("Species"), m.type = "lm"))
#' 
#' # comparisons of Species pooled over complete plates
#' (xx <- opm_mcp(vaas_4, as.labels = list("Species"), m.type ="lm", 
#'   mcp.def = mcp(Species = "Dunnett")))
#' # plot-method is available
#' par(mar = c(3, 15, 3, 2))
#' plot(xx)
#'
#' # comparison of only A01 - A04 against the intercept
#'   (xx <- opm_mcp(vaas_4, as.labels = list("Species", "Strain"), 
#'     sub.list = c(1:4), model = "Value ~ Well + Species", m.type = "lm"))
#'
#' # user defined a contrastmatrix
#' a <- mcp(Well = "Dunnett")
#  (xx <- opm_mcp(vaas_4, as.labels = list("Species", "Strain"), 
#'   sub.list = c(1:4), m.type = "lm", mcp.def = a, model = "Value ~ Well"))
#' # plot method
#' par(mar = c(3, 20, 3, 2))
#' plot(xx)    
#'
#'
#' # Matrix method
#' x <- extract(vaas_4, as.labels = list("Species", "Strain"), subset = "A",
#'   dataframe = TRUE)
#' 
#' (xx <- opm_mcp(x, as.labels = c("Species"), m.type = "lm"))
#' # plot method is available
#' par(mar = c(3, 15, 3, 2))
#' plot(xx)
#' 
#' # without performing the MCP
#' xx <- opm_mcp(x, per.mcp = FALSE)
#' 
#' # testing for subsets of object 
#' (xx <- opm_mcp(subset(x, Species == "Escherichia coli"), 
#'   mcp.def = mcp(Strain = "Dunnett"), as.labels = c("Strain"), m.type = "lm"))
#' # plot method available
#' par(mar = c(3, 15, 3, 2))
#' plot(xx)
#'


opm_mcp <- function(object, as.labels = NULL, per.mcp = TRUE, 
                    mcp.def, model, 
                    m.type = c("glm", "lm", "aov"),
                    sub.list = NULL) {

  as.labels = as.labels
  
  # matrix- or opms-input
  if (inherits(object, "OPMS")) {
    if (length(as.labels) < 1L) {
      stop("'as.labels'is missing")
    }
    object <- extract(object, as.labels = as.labels, subset = "A", 
              dataframe = TRUE)
  }
  # from here object should be an dataframe
  
  dim.start <- dim(object)
  param.pos <- which(colnames(object) == "Parameter")
  
  # whole plate or subset of wells for comparisons
  if (length(sub.list) > 0) {
    object <- object[, c(1: param.pos, c(sub.list + param.pos))]
  }
  
  # Give error-message, if dataframe does not have the required structure
  # If more than exactly one 'Parameter'-column exists
  if (length(param.pos) != 1L)
    stop("need data frame with exactly one column called 'Parameter'")

  # Give warning, if no numerical variables exist or the 'Parameter'-column 
  # is not at the required position
  if (param.pos == ncol(object))
    warning("no numerical data or 'Parameter'-column at wrong position")

  # Check, if only complete plates are taken into account
  if (ncol(object) - param.pos != 96)
    message("comparisons for only a subset of the plates")

  # include like the what-argument
  cnames <- colnames(object[, 1: param.pos])

  # check, if 'as.labels' is specified; if not give warning and use all 
  # factorial variables for model-building  

    
  # check if 'as.labels' has more than one level
  # give warning, if columns for grouping were stated double
  if (length(as.labels) != length(unique(as.labels)))
    warning("x-variable(s) are not unique")

  # check, if the entries for include exist in the dataframe
  includecol <- colnames(object)[1L: param.pos]   #- 1L
  bad <- which(!as.labels %in% includecol)
  
  if (length(bad))
    stop("cannot find column name: ", as.labels[bad[1L]])

  # reshape dataframe
  
  # helper-variable to avoid non-unique values when setting 'row.names'
  idhelper <- c(1:length(object[, 1]))
  dataframe1 <- cbind(idhelper, object)
  
  # reshaping
  result <- reshape(dataframe1, 
                    direction = "long", 
                    idvar = colnames(dataframe1[, 1:c(param.pos)]),
                    varying = colnames(dataframe1[, c(param.pos + 2): 
                                                      length(dataframe1)]), 
                    v.names = "Value", 
                    timevar = "Well",
                    times = colnames(dataframe1[, c(param.pos + 2): 
                                                  length(dataframe1)]))
  rownames(result) <- NULL

  # factorial columns of 'result'
  result$Well <- as.factor(result$Well)
  well.pos <- which(colnames(result) == "Well")

  if (!per.mcp) {
    print(str(result))
    result
  }
    else
  {
    # count number of levels of the factors
    param.pos.re <- which(colnames(object) == "Parameter")
    
    ma.level <- matrix(ncol = 1, 
      nrow = ncol(result [, c(c(1: c(param.pos.re - 1), 
                                param.pos.re: well.pos))]))
    for (i in 1: ncol(result[, 1:well.pos])) {
      ma.level[i, ] <- nlevels(result [, i])
    }
    # check for one-level-factors
    level.one <- which(ma.level > 1)
    xnames <- colnames(result[, level.one])
    
    #check if as.labels has more than one factor 
    if (length(as.labels) > 0) {
    bad <- which(!as.labels %in% xnames)
    if (length(bad))
      stop("Only one level for factor-variable(s): ", 
              paste(as.labels[bad[1L:length(bad)]], collapse = ","))
    }
    # model-fitting
    # default-model
    fmla <- as.formula(paste("Value ~ ", paste(as.labels, collapse = "+")))

    # model-statement
    if (missing(model)) {
      model <- fmla
      message("'model'is not specified; all variables in 'as.labels' are used")
    }
    
    # if m.type is given
    if (missing(m.type)) {
      stop("no model type is given")
    }
    
    # fitting the linear model acording m.type 
    fit.mod <- function(result, model, m.type) {
      #result, model,
      switch(m.type,
            lm = lm(model, data = result),
            aov = aov(model, data = result),
            glm = glm(model, data = result))
    }
    
    lmmod <- fit.mod(result, model, m.type)
    #and compute the comparisons
    
    # multiple testing:
    mcp.result <- glht(lmmod, linfct = mcp.def)
    
    # check the number of calculated tests
    gr.num <- confint(mcp.result)
    if (length(gr.num$confint[, 1]) > 20) {
      warning("number of performed comparisons exceeds 20")
    }
    
    cat("
        Resulting model from dataset", deparse(substitute(dataframe)),
        "is:", deparse(model),"
        ")
    mcp.result
  }
}
