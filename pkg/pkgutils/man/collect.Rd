\name{collect}
\alias{collect}
\alias{collect.list}
\alias{collect.matrix}
\title{Collect information}
\usage{
  collect(x, what, ...)

  \method{collect}{list} (x,
    what = c("counts", "occurrences", "values", "elements", "datasets", "rows"),
        min.cov = 1L, keep.unnamed = FALSE, dataframe = FALSE,
    optional = TRUE,
    stringsAsFactors = default.stringsAsFactors(), ...)

  \method{collect}{matrix} (x, what = c("columns", "rows"),
    empty = "?", ...)
}
\arguments{
  \item{x}{List or matrix.}

  \item{what}{Character scalar indicating how to collect
  information. The following values are supported by the
  list method: \describe{ \item{counts}{For all non-list
  elements of \code{x}, count their occurrences.}
  \item{occurrences}{Like \sQuote{counts}, but only
  indicate presence or absence.} \item{values}{Simplify all
  direct elements of \code{x}, irrespective of whether or
  not they are lists, for including them as rows in a data
  frame.  Their names determine the columns. See
  \code{keep.unnamed} for the action in the case of missing
  names.} \item{elements}{Like \sQuote{elements}, but
  collect only the non-list elements of \code{x}, i.e.
  flatten \code{x} in the first step.}
  \item{datasets}{Convert all elements to data frames or
  matrices, then merge them using rows and column names. In
  case of conflict, the last ones win.  Here, the behaviour
  of other arguments is special if all elements of \code{x}
  are atomic. See below.} \item{rows}{Like \code{datasets},
  but all rows are kept. This is like \code{rbind} from the
  \pkg{base} package but it also augments missing columns
  where necessary.} } The matrix method currently only
  supports \code{columns}, which means assorting the values
  to the columns anew based on the majority of their
  occurrences, and \code{rows}, which does the same for the
  rows. This can be used to clean up messy data.}

  \item{min.cov}{Numeric scalar indicating the minimal
  coverage required in the resulting presence-absence
  matrix. Columns with a fewer number of non-zero entries
  are removed.}

  \item{keep.unnamed}{Logical scalar indicating whether
  names should be inserted for elements of \code{x} that
  miss them. If \code{NA}, they are skipped, but with a
  warning; if \code{FALSE}, they are skipped silently.
  This only has an effect in conjunction with the last
  three values of \code{what}. If \kbd{datasets} are
  chosen, it usually has only an effect if all elements of
  \code{x} are atomic.}

  \item{dataframe}{Logical scalar indicating whether a data
  frame should be produced instead of a matrix.}

  \item{optional}{See \code{as.data.frame} from the
  \pkg{base} package.}

  \item{stringsAsFactors}{See \code{as.data.frame} from the
  \pkg{base} package.}

  \item{empty}{Character scalar used as intermediary
  placeholder for empty and missing values.}

  \item{...}{Optional arguments passed to and from other
  methods (if requested to \code{as.data.frame}).}
}
\value{
  The list method of \code{flatten} returns a non-nested
  list. The \code{collect} methods yield a data frame or a
  matrix.
}
\description{
  Methods for collecting information from list-like objects
  into a matrix or data frame or for re-assigning values to
  columns in a matrix.
}
\details{
  The list method of \code{flatten} is based on
  \url{https://stackoverflow.com/questions/8139677/} with
  some slight improvements.
}
\examples{
## collect()
x <- list(X = list(A = 1:3, B = 7L, C = list('c1', 1:3)),
  Y = list(A = 1:3, 11, B = -1L, D = "?"))

## collect values into a data frame or matrix
(got <- collect(x, "values", dataframe = TRUE))
stopifnot(LETTERS[1:4] == colnames(got))
stopifnot(names(x) == rownames(got))
stopifnot(is.list(got$A), is.integer(got$B), is.list(got$C),
  is.factor(got$D))
stopifnot(!is.na(got$A), !is.na(got$B), anyNA(got$C), anyNA(got$D))
# include the unnamed ones
got <- collect(x, "values", dataframe = TRUE, keep.unnamed = TRUE)
stopifnot(dim(got) == c(2, 5))
# simplify to matrix
(got <- collect(x, "values", dataframe = FALSE))
stopifnot(is.matrix(got), mode(got) == "list")

## collect elements into a data frame or matrix
(got <- collect(x, "elements", dataframe = TRUE))
stopifnot(dim(got) == c(2, 9), colnames(x) == rownames(got),
  is.data.frame(got))
(got <- collect(x, "elements", dataframe = FALSE))
stopifnot(dim(got) == c(2, 9), colnames(x) == rownames(got),
  !is.data.frame(got))

## count or just note occurrences
(got <- collect(x, "counts", dataframe = FALSE))
stopifnot(dim(got) == c(2, 8), rownames(got) == names(x),
  setequal(colnames(got), unlist(x)), any(got > 1))
(got <- collect(x, "occurrences", dataframe = FALSE))
stopifnot(dim(got) == c(2, 8), rownames(got) == names(x),
  setequal(colnames(got), unlist(x)), !any(got > 1))

## convert to data frames and insert everything in a single one
(got <- collect(x, "datasets", optional = FALSE, dataframe = TRUE))
stopifnot(dim(got) == c(3, 6), is.data.frame(got))

## a more useful application is to merge matrices
m1 <- matrix(1:4, ncol = 2, dimnames = list(c("A", "B"), c("x", "y")))
m2 <- matrix(1:4, ncol = 2, dimnames = list(c("C", "B"), c("x", "z")))
(got <- collect(list(m1, m2), "datasets"))
# values missing in some matrix yield NA
stopifnot(dim(got) == c(3, 3), anyNA(got))
}
\seealso{
  base::unlist base::as.data.frame base::rbind

  Other coding-functions: \code{\link{L}},
  \code{\link{LL}}, \code{\link{assert}},
  \code{\link{case}},     \code{\link{check}}, \code{\link{contains}},
  \code{\link{flatten}}, \code{\link{listing}},
  \code{\link{map_names}}, \code{\link{map_values}},
  \code{\link{must}}, \code{\link{set}}, \code{\link{sql}},
    \code{\link{unnest}}
}
\keyword{manip}

