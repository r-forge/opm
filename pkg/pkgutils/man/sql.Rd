\name{sql}
\alias{sql}
\alias{sql.data.frame}
\alias{sql.formula}
\title{Create \acronym{SQL} \code{SELECT} and \code{UPDATE} statements}
\usage{
  sql(x, ...)

  \method{sql}{data.frame} (x, where, table,
    set = setdiff(colnames(x), where), ...)

  \method{sql}{formula} (x, ...)
}
\arguments{
  \item{x}{Data frame or formula.}

  \item{where}{Character vector giving the name of the data
  frame and database table columns used to select rows.}

  \item{table}{Character scalar indicating the name of the
  database table to be updated.}

  \item{set}{Character vector giving the name of the data
  frame and database table columns to be updated.}

  \item{...}{Optional arguments passed between methods.}
}
\value{
  Character vector.
}
\description{
  Helper function converting \R code into \acronym{SQL}
  \code{SELECT} statements and data frames into
  \code{UPDATE} statements.
}
\details{
  The formula method saves some typing, particularly in the
  case of complex queries, but it does not support joins.
  \R operators are mostly directly translated except for
  those with the highest precedence. Infix operators are
  translated literally. The control structures \code{if}
  and \code{function} yield \code{CASE} constructs.

  To use the data frame method to update a column, say,
  \code{"x"} that is also used to select rows, include
  \code{"x"} in the \code{where} argument and
  \code{"new.x"} in the \code{update} argument.
}
\examples{
## formula method
x <- projects(id, strain = strain_number, if (gram_stain == "N.D.")
    "?"
  else
    gram_stain) ~ project_name == "KMG-1" &
  (coalesce(strain_number, 3) > -4000 | !is_type_strain) &
  img_object_id == NULL
(y <- sql(x))
stopifnot(is.character(y), length(y) == 1L)
}
\seealso{
  Other coding-functions: \code{\link{L}},
  \code{\link{LL}}, \code{\link{assert}},
  \code{\link{case}},     \code{\link{check}}, \code{\link{collect}},
    \code{\link{contains}}, \code{\link{flatten}},
  \code{\link{listing}}, \code{\link{map_names}},
  \code{\link{map_values}}, \code{\link{must}},
  \code{\link{set}}, \code{\link{unnest}}
}
\keyword{character}
\keyword{database}

