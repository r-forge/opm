


################################################################################
################################################################################
#
# S3 K-means related methods
#


#' @rdname kmeans
#' @export
#'
to_kmeans <- function(x, ...) UseMethod("to_kmeans")

#' @rdname kmeans
#' @method to_kmeans kmeans
#' @export
#'
to_kmeans.kmeans <- function(x, ...) {
  x
}

#' @rdname kmeans
#' @method to_kmeans kmeanss
#' @export
#'
to_kmeans.kmeanss <- function(x, y, ...) {
  x[[y]]
}

#' @rdname kmeans
#' @method to_kmeans Ckmeans.1d.dp
#' @export
#'
to_kmeans.Ckmeans.1d.dp <- function(x, y, ...) {
  if (!is.numeric(y) || length(y) != length(x$cluster))
    stop("'y' must correspond to the input data from which 'x' originates")
  x <- unclass(x)
  x$tot.withinss <- sum(x$withinss)
  x$totss <- sum(scale(y, scale = FALSE) ^ 2L)
  x$betweenss <- x$totss - x$tot.withinss
  x$centers <- as.matrix(x$centers)
  x <- x[c("cluster", "centers", "totss", "withinss", "tot.withinss",
    "betweenss", "size")]
  x$iter <- 1L # new entry as of R 3.0.1 patched; trivially 1 here
  x$ifault <- 0L
  class(x) <- "kmeans"
  x
}


################################################################################


#' @rdname kmeans
#' @export
#'
calinski <- function(x, ...) UseMethod("calinski")

#' @rdname kmeans
#' @method calinski kmeans
#' @export
#'
calinski.kmeans <- function(x, ...) {
  r.2 <- (x$totss - x$tot.withinss) / x$totss
  # We do not use "$centers" here because there are as many centers per
  # cluster as matrix columns if a matrix was input
  k <- length(unique(x$cluster))
  n <- length(x$cluster)
  (r.2 / (k - 1L)) / ((1L - r.2) / (n - k))
}

#' @rdname kmeans
#' @method calinski Ckmeans.1d.dp
#' @export
#'
calinski.Ckmeans.1d.dp <- function(x, y, ...) {
  calinski(to_kmeans(x, y), ...)
}

#' @rdname kmeans
#' @method calinski kmeanss
#' @export
#'
calinski.kmeanss <- function(x, ...) {
  vapply(X = x, FUN = calinski, FUN.VALUE = 1, ...)
}


################################################################################


#' @rdname kmeans
#' @method plot kmeanss
#' @export
#'
plot.kmeanss <- function(x, xlab = "Number of clusters",
    ylab = "Calinski-Harabasz statistics", ...) {
  x <- as.numeric(names(y <- calinski(x)))
  plot(x, y, xlab = xlab, ylab = ylab, ...)
  invisible(y)
}


################################################################################


#' Work with k-means results
#'
#' Calculate or plot the Calinski-Harabasz statistics from \code{kmeans}
#' results. The result of \code{plot} is a simple scatter plot which can be
#' modified with arguments passed to \code{plot} from the \pkg{graphics}
#' package. Alternatively, determine the borders between clusters of
#' one-dimensional data, create a histogram in which these borders are plotted,
#' or convert an object to one of class \code{kmeans}.
#'
#' @param x Object of class \code{kmeans}, \sQuote{Ckmeans.1d.dp} or
#'   \code{kmeanss}. For \code{plot}, only the latter.
#' @param y Vector of original data subjected to clustering. Automatically
#'   determined for the \code{kmeanss} methods. For \code{to_kmeans}, original
#'   numeric vector that was used to create a \sQuote{Ckmeans.1d.dp} object, or
#'   index of an element of a \code{kmeanss} object.
#' @param k Numeric vector or \code{NULL}. If non-empty, it indicates the number
#'   of groups (previously used as input for \code{kmeans}) for which vertical
#'   lines should be drawn in the plot that represent the cluster borders. If
#'   empty, the smallest non-trivial number of clusters is chosen.
#' @param col Graphical parameter passed to \code{abline}. If several values of
#'   \code{k} are given, \code{col} is recycled as necessary.
#' @param lwd Like \code{col}.
#' @param lty Like \code{col}.
#' @param main Passed to \code{hist.default}.
#' @param xlab Character scalar passed to \code{hist.default} or to \code{plot}
#'   from the \pkg{graphics} package.
#' @param ylab Character scalar passed to \code{plot} from the \pkg{graphics}
#'   package.
#' @param ... Optional arguments passed to and from other methods. For the
#'   \code{hist} method, optional arguments passed to \code{hist.default}.
#' @export
#' @rdname kmeans
#' @aliases kmeans
#' @return
#'   \code{to_kmeans} creates an object of class \code{kmeans}.
#'
#'   \code{borders} creates a numeric vector or list of such vectors.
#'
#'   The return value of the \code{hist} method is like \code{hist.default};
#'   see there for details.
#'
#'   \code{calinksi} returns a numeric vector with one element per
#'   \code{kmeans} object. \code{plot} returns it invisibly. Its
#'   \sQuote{names} attribute indicates the original numbers of clusters
#'   requested.
#' @keywords cluster hplot manip
#' @family kmeans-functions
#' @details The borders are calculated as the mean of the maximum of the cluster
#'   with the lower values and the minimum of the neighbouring cluster with the
#'   higher values. The \code{hist} method plots a histogram of one-dimensional
#'   data subjected to k-means partitioning in which these borders can be drawn.
#'
#'   \code{y} must also be in the order it has been when subjected to
#'   clustering, but this is not checked. Using \code{kmeanss} objects thus
#'   might preferable in most cases because they contain a copy of the input
#'   data.
#' @seealso graphics::hist graphics::abline Ckmeans.1d.dp::Ckmeans.1d.dp
#' @examples
#'
#' x <- as.vector(extract(vaas_4, as.labels = NULL, subset = "A"))
#' x.km <- run_kmeans(x, k = 1:10)
#'
#' # plot() method
#' # the usual arguments of plot() are available
#' show(y <- plot(x.km, col = "blue", pch = 19))
#' stopifnot(is.numeric(y), names(y) == 1:10)
#'
#' # borders() method
#' (x.b <- borders(x.km)) # => list of numeric vectors
#' stopifnot(is.list(x.b), length(x.b) == 10, sapply(x, is.numeric))
#' stopifnot(sapply(x.b, length) == as.numeric(names(x.b)) - 1)
#'
#' # hist() methods
#' y <- hist(x.km[[2]], x, col = "blue", lwd = 2)
#' stopifnot(inherits(y, "histogram"))
#' y <- hist(x.km, 3:4, col = c("blue", "red"), lwd = 2)
#' stopifnot(inherits(y, "histogram"))
#'
#' # to_kmeans() methods
#' x <- c(1, 2, 4, 5, 7, 8)
#' summary(y <- kmeans(x, 3))
#' stopifnot(identical(y, to_kmeans(y)))
#' # see particularly run_kmeans() which uses this internally if clustering is
#' # done with Ckmeans.1d.dp::Ckmeans.1d.dp()
#'
borders <- function(x, ...) UseMethod("borders")

#' @rdname kmeans
#' @method borders kmeans
#' @export
#'
borders.kmeans <- function(x, y, ...) {
  if (sum(siz <- x$size) != length(y))
    stop("'y' must be a vector with the same number of items than 'x'")
  if (length(siz) == 1L)
    return(numeric())
  ranges <- vapply(seq_along(siz), function(i) range(y[x$cluster == i]),
    numeric(2L))
  colMeans(matrix(sort.int(ranges)[c(-1L, -length(ranges))], nrow = 2L))
}

#' @rdname kmeans
#' @method borders Ckmeans.1d.dp
#' @export
#'
borders.Ckmeans.1d.dp <- function(x, y, ...) {
  borders(to_kmeans(x), y, ...)
}

#' @rdname kmeans
#' @method borders kmeanss
#' @export
#'
borders.kmeanss <- function(x, ...) {
  sapply(X = x, FUN = borders, y = attr(x, "input"), ..., simplify = FALSE)
}

#' @rdname kmeans
#' @method hist kmeans
#' @export
#'
hist.kmeans <- function(x, y, col = "black", lwd = 1L, lty = 1L, main = NULL,
    xlab = "Clustered values", ...) {
  b <- borders(x, y)
  result <- hist(y, main = main, xlab = xlab, ...)
  mapply(FUN = abline, v = b, col = col, lwd = lwd, lty = lty,
    SIMPLIFY = FALSE, USE.NAMES = FALSE)
  invisible(result)
}

#' @rdname kmeans
#' @method hist Ckmeans.1d.dp
#' @export
#'
hist.Ckmeans.1d.dp <- function(x, y, ...) {
  hist(to_kmeans(x), y, ...)
}

#' @rdname kmeans
#' @method hist kmeanss
#' @export
#'
hist.kmeanss <- function(x, k = NULL, col = "black", lwd = 1L, lty = 1L,
    main = NULL, xlab = "Clustered values", ...) {
  smallest_k <- function(x) {
    y <- (y <- as.integer(names(x)))[y > 1L]
    case(length(y), integer(), min(y))
  }
  result <- hist(y <- attr(x, "input"), main = main, xlab = xlab, ...)
  if (!length(k) && !length(k <- smallest_k(x)))
    return(invisible(result))
  b <- lapply(as.character(k), function(key) borders(x[[key]], y))
  mapply(FUN = abline, v = b, col = col, lwd = lwd, lty = lty,
    SIMPLIFY = FALSE, USE.NAMES = FALSE)
  invisible(result)
}


################################################################################
################################################################################
#
# S4 K-means related methods
#


#' Conduct k-means partitioning
#'
#' Run a k-means partitioning analysis. This function is used by
#' \code{\link{discrete}} in \sQuote{gap} mode to automatically determine the
#' range of ambiguous data. If applied to such one-dimensional data, it uses an
#' exact algorithm from the \pkg{Ckmeans.1d.dp} package.
#'
#' @param object Numeric vector or matrix.
#' @param k Numeric vector. Number of clusters requested.
#' @param nstart Numeric scalar. Ignored if \sQuote{Ckmeans.1d.dp} is called.
#'   Otherwise passed to \code{kmeans} from the \pkg{stats} package.
#' @param cores Numeric scalar indicating the number of cores to use.
#' @param ... List of optional arguments passed to \code{kmeans} from the
#'   \pkg{stats} package.
#' @return S3 object of class \code{kmeanss}, basically a named list of
#'   \code{kmeans} objects.
#' @family kmeans-functions
#' @seealso stats::kmeans Ckmeans.1d.dp::Ckmeans.1d.dp
#' @keywords cluster
#' @export
#' @references Wang, H., Song, M. 2011 Ckmeans.1d.dp: Optimal k-means clustering
#'   in one dimension by dynamic programming. \emph{The R Journal} \strong{3},
#'   p. 29--33.
#' @examples
#' x <- as.vector(extract(vaas_4, as.labels = NULL, subset = "A"))
#' summary(x.km <- run_kmeans(x, k = 1:10)) # => 'kmeanss' object
#' stopifnot(inherits(x.km, "kmeanss"), length(x.km) == 10)
#' stopifnot(sapply(x.km, class) == "kmeans", names(x.km) == 1:10)
#'
setGeneric("run_kmeans",
  function(object, k, ...) standardGeneric("run_kmeans"))

setMethod("run_kmeans", c("numeric", "numeric"), function(object, k,
    cores = 1L) {
  result <- mclapply(X = prepare_k(k), FUN = Ckmeans.1d.dp, x = object,
    mc.cores = cores)
  structure(.Data = lapply(X = result, FUN = to_kmeans, y = object),
    class = "kmeanss", input = object)
}, sealed = SEALED)

setMethod("run_kmeans", c("matrix", "numeric"), function(object, k,
    cores = 1L, nstart = 10L, ...) {
  result <- if (ncol(object) < 2L)
    run_kmeans(as.vector(object), k, cores)
  else
    structure(.Data = mclapply(prepare_k(k), function(centers) {
      kmeans(x = object, centers = centers, nstart = nstart, ...)
    }, mc.cores = cores), class = "kmeanss")
  attr(result, "input") <- object
  result
}, sealed = SEALED)


################################################################################



