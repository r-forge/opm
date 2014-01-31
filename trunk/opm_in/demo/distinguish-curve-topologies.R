#' # How can we decide, if curve topologies are significantly different?
#'
#' Assume you have run only four PM plates of the same plate type. Assume you
#' have tested by this the phenotypes of four biological specimens, e.g.
#' bacterial strains. For each strain one plate was utilised. Unfortunately, you
#' do not have replicate plates per bacterial strain.
#'
#' ### The following question then arises:
#' * If four curves look fairly similar, how can we assess some statistical
#' information whether they could be the same or be rather significantly
#' different?
#'
#' In order to assess this, we make use of bootstrapped curve parameters, which
#' allow to determine a 95 % confidence interval
#'
#' The data set `wittmann_et_al` contains Generation-III data for numerous
#' strains of the bacterial species *Achromobacter xylosoxidans* (see the
#' respective publication).
#'
#'
#' Author: *Johannes Sikorski* and *Markus Goeker*
#'
#' ### Load R packages and data

library(opm)
library(opmdata)
data(wittmann_et_al)

#' ### Subset for an appropriate data set for demonstration purpose
#' * see [subset()](http://www.goeker.org/opm/opm_doc/manual/subset.html) for
#' details

wittmann_small <- subset(wittmann_et_al,
                        query = list(strain = c("CCUG 41513", "CCUG 2203"),
                                      replicate = "2")) +
                  subset(wittmann_et_al,
                        query = list(strain = c("LMG 7051", "CCUG 48135"),
                                    replicate = "1"))

#' ### Check the dimensions and metadata
#' * see [dim()](http://www.goeker.org/opm/opm_doc/manual/dim.html) for details
#' * see [to_metadata()]
#' p://www.goeker.org/opm/opm_doc/manual/collect_template.html) for details

dim(wittmann_small)
to_metadata(wittmann_small[, , ])

#' ### Plot the raw kinetic values for well G07 (D-Malic Acid) from the
#' Generation-III plates
#' * see [xy_plot()](http://www.goeker.org/opm/opm_doc/manual/xy_plot.html) for
#' details
#'
#+ Figure1, fig.width = 10, fig.height = 5

xy_plot(wittmann_small[, , "G07"],
        include = list("strain"),
        col = c("red", "green", "blue", "black"),
        legend.fmt = list(space = "right"), lwd = 2, neg.ctrl = 50)

#' ### Result:
#' * The overall curve topology is quite similar.
#' * The following question arises:
#'
#' ### Do strains differ in any of the aggregated curve parameters A, `AUC`, mu,
#' lambda?
#' * To answer this, we make use of the 95% confidence intervals obtained
#' during aggregation of curve parameters using bootstrap procedures.
#'
#' ### Show aggregated data for strain `LMG` 7051, well `G07 (D-Malic Acid)`
#' * the aggregated data contain 95% confidence intervals from bootstrapping
#' * see [`aggregated()`]
#' (http://www.goeker.org/opm/opm_doc/manual/aggregated.html) for details
#' * see [`do_aggr()`]
#' (http://www.goeker.org/opm/opm_doc/manual/do_aggr.html) in order to learn
#' how to bootstrap during curve parameter aggregation

aggregated(subset(wittmann_small[, , "G07"], list(strain = "LMG 7051")),
          full = TRUE)

#' ### Plot confidence interval for the maximum height value (A)
#' * see [ci_plot()](http://www.goeker.org/opm/opm_doc/manual/ci_plot.html) for
#' details
#+ Figure2, fig.width = 5, fig.height = 3.5

ci_plot(wittmann_small[, , "G07"], as.labels = "strain",
        subset = "A", x = "topright", legend.field = NULL, cex = 0.8)

#' * strains "`CCUG` 41513" and "`CCUG` 2203" can not be distinguished by their
#' maximum height (A)
#' * similarly, also strains "`LMG` 7051" and "`CCUG` 48135" can not be
#' distinguished by A
#' * however, both pairs of strains differ significantly in their A value
#'
#' ### Plot confidence interval for the Area under the curve value (`AUC`)
#' * see [ci_plot()](http://www.goeker.org/opm/opm_doc/manual/ci_plot.html)
#' details
#+ Figure3, fig.width = 5, fig.height = 3.5

ci_plot(wittmann_small[, , "G07"], as.labels = "strain",
        subset = "`AUC`", x = "topright", legend.field = NULL, cex = 0.8)

#' * all strains show different `AUC` values
#'
#' ### Plot confidence interval for the steepness of the slope value (mu)
#' * see [ci_plot()](http://www.goeker.org/opm/opm_doc/manual/ci_plot.html)
#' details
#+ Figure4, fig.width = 5, fig.height = 3.5

ci_plot(wittmann_small[, , "G07"], as.labels = "strain",
        subset = "mu", x = "topright", legend.field = NULL, cex = 0.8)

#' * all strains show different mu values
#'
#' ### Plot confidence interval for the lag phase value (lambda)
#' * see [ci_plot()](http://www.goeker.org/opm/opm_doc/manual/ci_plot.html)
#' details
#+ Figure5, fig.width = 5, fig.height = 3.5

ci_plot(wittmann_small[, , "G07"], as.labels = "strain",
        subset = "lambda", x = "topleft", legend.field = NULL, cex = 0.8)

#' * strains "`LMG` 7051" and "`CCUG` 2203" can not be distinguished by their
#' lag phase length
#'
#' # Synopsis
#' * even if no experimental replicates exist, very similar curve topologies
#' can be tested for differences in aggregated curve parameters using 95%
#' confidence interval values derived from bootstrapping


