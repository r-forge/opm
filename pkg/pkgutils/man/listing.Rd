\name{listing}
\alias{flatten}
\alias{listing}
\alias{unnest}
\title{Nicer message listings and flattening or expansion of objects}
\arguments{
  \item{object}{Usually a list. The default method just
  returns \code{object} if it is atomic but raises an error
  otherwise. For \code{unnest}, a data frame, list or
  character vector.}

  \item{use.names}{Logical scalar passed to \code{unlist}
  from the \pkg{base} package.}

  \item{x}{For the default method, an object convertible
  via \code{unclass} to one of the object classes that have
  explicit methods. For the character-vector method, in the
  default style mode, its \sQuote{names} attribute is used
  as the first column of the resulting listing; if it is
  \code{NULL} or if \code{force.numbers} is \code{TRUE},
  numbers are inserted. The \sQuote{double} method is
  controlled by the \sQuote{digits} entry of \code{options}
  from the \pkg{base} package.}

  \item{header}{\code{NULL} or character vector. Prepended
  to the result.}

  \item{footer}{\code{NULL} or character vector. Appended
  to the result.}

  \item{prepend}{Logical, numeric or character scalar. The
  two main uses are: \describe{ \item{Default mode:}{The
  value is prepended to each line except \code{header} and
  \code{footer}. If numeric, the number of spaces.
  \code{TRUE} causes tabs to be used, \code{FALSE} turns
  prepending off. If in \sQuote{character} mode, used
  directly.} \item{If \code{style} is \sQuote{sentence}:}{
  In that case, a logical scalar decides about whether
  names are prepended before joining the vector elements. A
  character scalar is used as template for \code{sprintf},
  which gets \code{names(x)} passed as second and \code{x}
  as third argument. (This order can be inverted, see next
  argument.)} }}

  \item{style}{Character scalar. The main options are:
  \describe{ \item{\sQuote{table} or \sQuote{list}:}{Passed
  to \code{formatDL}.} \item{\sQuote{sentence}:}{A
  comma-separated list is created from \code{x}, the last
  separator according to \code{last.sep}.}
  \item{\sQuote{m4} or \sQuote{M4}}{acronym{GNU}
  \command{m4} macro definitions using double or single
  quoting, respectively, for the expansions are created. A
  warning is issued if the macro strings are invalid, which
  is always the case of \code{x} has no names;
  \code{prepend} is ignored.} \item{Otherwise:}{A template
  for \code{sprintf} is assumed taking two additional
  arguments, first \code{names(x)} and then \code{x}.} }
  Note that names and values of \code{x} are exchanged
  beforehand if \code{style} is run through \code{I} from
  the \pkg{base} package.}

  \item{collapse}{Character scalar used to join the
  resulting vector elements.  It is by default also applied
  for joining \code{header} and \code{footer} footer with
  them (if provided). This can be turned off using
  hf.collapse.  By default this is an empty string for
  \sQuote{sentence} style, the newline character
  otherwise.}

  \item{force.numbers}{Logical scalar. Always use numbers
  instead of the \sQuote{names} attribute?}

  \item{last.sep}{Character scalar indicating what should
  be used as last separator if \code{style} is
  \sQuote{sentence}. \sQuote{both} means \sQuote{and} and
  comma, \sQuote{two} means \sQuote{or} and comma.}

  \item{hf.collapse}{Character scalar or empty. If distinct
  from \code{collapse}, used for separately for joining
  \code{header} and \code{footer} (if provided).}

  \item{...}{Optional other arguments passed to
  \code{formatDL}. For \code{unnest}, optional arguments
  passed to \code{data.frame}.}

  \item{sep}{Character scalar passed as \code{split}
  argument to \code{strsplit}.}

  \item{col}{Character vector with the names of the columns
  to be passed to \code{strsplit} for splitting.}

  \item{fixed}{Logical scalar passed to \code{strsplit}.}

  \item{stringsAsFactors}{Logical scalar passed to
  \code{data.frame}.}
}
\value{
  Character scalar in the case of \code{listing}, data
  frame in the case of \code{unnest}.
}
\description{
  Create some kind of listing, used, e.g., in (error)
  messages or warnings. Alternatively, make an object
  \sQuote{flat}, such as by creating a non-nested list from
  a list, or expand it after splitting certain components.
}
\examples{
## listing()

# default style
x <- structure(letters[1:5], names = LETTERS[1:5])
(y <- listing(x, "Five letters:", "...end here", 1))
stopifnot(length(y) == 1, y ==
  "Five letters:\n A: a\n B: b\n C: c\n D: d\n E: e\n...end here")

# 'sentence' style
(y <- listing(letters[1:3], style = "sentence", last.sep = "both"))
stopifnot(y == "a, b, and c", length(y) == 1)
(y <- listing(letters[1:3], style = I("sentence"), last.sep = "both"))
stopifnot(y == "1, 2, and 3", length(y) == 1)
(y <- listing(letters[1:3], style = "sentence", prepend = TRUE))
stopifnot(y == "1: a, 2: b and 3: c", length(y) == 1)
(y <- listing(letters[1:3], style = I("sentence"), prepend = TRUE))
stopifnot(y == "a: 1, b: 2 and c: 3", length(y) == 1)
(y <- listing(letters[1:3], style = "sentence", prepend = "\%s=>\%s"))
stopifnot(y == "1=>a, 2=>b and 3=>c", length(y) == 1)
(y <- listing(letters[1:3], style = "sentence", last.sep = "two"))
stopifnot(y == "a, b, or c", length(y) == 1)

# with explicit sprintf template
(y <- listing(x, style = "\%s, \%s", collapse = "; ", prepend = "!"))
stopifnot(y == "!A, a; !B, b; !C, c; !D, d; !E, e", length(y) == 1)
(y <- listing(x, style = I("\%s, \%s"), collapse = "; ", prepend = "!"))
stopifnot(y == "!a, A; !b, B; !c, C; !d, D; !e, E", length(y) == 1)

# create m4 macro definitions
(y <- listing(x, style = "m4"))
stopifnot(grepl("^(define\\\([^)]+\\\)dnl\n?)+$", y), length(y) == 1)

# other 'x' arguments
stopifnot(listing(x) == listing(as.list(x)))
old.opt <- options(digits = 3)
stopifnot(listing(pi) == "1: 3.14") # controlled by getOption("digits")
options(old.opt)

## flatten()
x <- list(a = list(b = 1:5, c = letters[1:5]), d = LETTERS[1:3],
  e = list(pi))
(y <- flatten(x)) # sublists removed, non-list elements kept
stopifnot(is.list(y), length(y) == 4, !sapply(y, is.list))
# atomic objects are not modified by default
stopifnot(identical(letters, flatten(letters)))
}
\seealso{
  base::message base::warning base::stop base::formatDL

  Other coding-functions: \code{\link{L}},
  \code{\link{LL}}, \code{\link{assert}},
  \code{\link{case}},   \code{\link{check}},
  \code{\link{collect}},   \code{\link{contains}},
  \code{\link{map_names}}, \code{\link{map_values}},
  \code{\link{must}}
}
\keyword{utilities}

%% BEGIN INSERTION BY repair_S4_docu.rb
\docType{methods}
\alias{flatten-methods}
\alias{flatten,ANY-method}
\alias{flatten,list-method}
\alias{listing-methods}
\alias{listing,ANY-method}
\alias{listing,character-method}
\alias{listing,factor-method}
\alias{listing,list-method}
\alias{listing,numeric-method}
\alias{unnest-methods}
\alias{unnest,character-method}
\alias{unnest,data.frame-method}
\alias{unnest,list-method}
\usage{
  \S4method{flatten}{ANY}(object, ...) 
  \S4method{flatten}{list}(object, use.names = TRUE, ...) 

  \S4method{listing}{ANY}(x, ...) 
  \S4method{listing}{character}(x, header = NULL, footer = NULL,
    prepend = FALSE, style = "list", collapse = if (style == "sentence")
      ""
    else
      "\n", force.numbers = FALSE,
    last.sep = c("and", "both", "comma", "or", "two"), hf.collapse = collapse,
    ...) 
  \S4method{listing}{factor}(x, ...) 
  \S4method{listing}{list}(x, ...) 
  \S4method{listing}{numeric}(x, ...) 

  \S4method{unnest}{character}(object, sep, fixed = TRUE, ...,
    stringsAsFactors = FALSE) 
  \S4method{unnest}{data.frame}(object, sep, col = colnames(object),
    fixed = TRUE, ..., stringsAsFactors = FALSE) 
  \S4method{unnest}{list}(object, ..., stringsAsFactors = FALSE) 
}
%% END INSERTION BY repair_S4_docu.rb
