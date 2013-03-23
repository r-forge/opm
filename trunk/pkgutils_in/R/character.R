

################################################################################


#' Manipulate character vectors
#'
#' A function for performing a variety of operations on a character vector that
#' yield a character vector of the same length.
#'
#' @param x Character vector. Some values of \code{how} allow for other objects
#'   if convertible via \code{as.character}.
#' @param how Character scalar determining the conducted conversion. Possible
#'   values are:
#'   \describe{
#'     \item{space}{Trim whitespace at the ends and reduce other runs of
#'     consecutive whitespace characters to as single space.}
#'     \item{word}{Trim non-word characters and underscores at the ends and
#'     reduce other runs of consecutive non-word characters and underscore to a
#'     single underscore.}
#'     \item{rword}{Like \sQuote{word}, but using dots instead of underscores.}
#'     \item{title}{Convert entire strings to title case.}
#'     \item{revert}{Revert characters in strings.}
#'     \item{chars}{Reduce to unique characters and sort them.}
#~     \item{capply}{Apply function \code{using} to vector separately containing
#~     the single characters and join the result again.}
#'     \item{echo}{Repeat entire strings, the number given by \code{using}.}
#'     \item{cecho}{Repeat single characters, the number given by \code{using}.}
#'     \item{rpad}{Pad with spaces on the right, either to the maximum number of
#'     characters or, if given, up to a length of \code{using}, trimming the
#'     strings where necessary.}
#'     \item{lpad}{Like \sQuote{rpad}, but on the left side.}
#'     \item{bpad}{Like \sQuote{rpad}, but on both sides, preferring the right
#'     side (with a single space) where necessary.}
#'   }
#' @param using Vector or function. Depending on the value of \code{how} either
#'   required, optional or ignored.
#' @return Character vector of the length of \code{x} (if it was a vector).
#'   Attributes, if any, are preserved in that case.
#' @family character-functions
#' @keywords character manip
#' @export
#' @examples
#'
#' # exemplar character vector
#' x <- c("Huey, Dewey, and Louie", "Donald Duck ", " ", " Daisy", NA)
#' names(x) <- LETTERS[1:length(x)]
#'
#' # cleaning whitespace
#' (y <- strcon(x, "space"))
#' stopifnot(is.character(y), is.na(y) == is.na(x),
#'   x[1] == y[1], nchar(x[2:4]) > nchar(y[2:4]), identical(names(x), names(y)))
#'
#' # reducing to word characters
#' (y <- strcon(x, "word"))
#' stopifnot(is.character(y), is.na(y) == is.na(x),
#'   nchar(x[1:4]) > nchar(y[1:4]), identical(names(x), names(y)))
#'
#' # reducing to R-word characters
#' (y <- strcon(x, "rword"))
#' stopifnot(is.character(y), is.na(y) == is.na(x),
#'   nchar(x[1:4]) > nchar(y[1:4]), identical(names(x), names(y)))
#'
#' # converting to title case (per character scalar)
#' (y <- strcon(x, "title"))
#' stopifnot(is.character(y), is.na(y) == is.na(x),
#'   x[c(1, 2, 4)] != y[c(1, 2, 4)], x[3] == y[3], nchar(x) == nchar(y),
#'   identical(names(x), names(y)))
#'
#' # reverting the strings
#' (y <- strcon(x, "revert"))
#' stopifnot(is.character(y), is.na(y) == is.na(x),
#'   x[c(1, 2, 4)] != y[c(1, 2, 4)], x[3] == y[3], nchar(x) == nchar(y),
#'   identical(names(x), names(y)))
#'
#' # getting the unique characters
#' (y <- strcon(x, "chars"))
#' stopifnot(is.character(y), is.na(y) == is.na(x),
#'   x[1:2] != y[1:2], x[3:4] == y[3:4], nchar(x) >= nchar(y),
#'   identical(names(x), names(y)))
#'
#~ # applying arbitrary function to single-character vector
#~ (y <- strcon(x, "capply", identity))
#~ stopifnot(identical(y, x))
#~ (y <- strcon(x, "capply", rev.default))
#~ stopifnot(identical(y, strcon(x, "revert")))
#'
#' # repeat the strings several times
#' (y <- strcon(x, "echo", 0))
#' stopifnot(is.character(y), is.na(y) == is.na(x), nchar(y[1:4]) == 0)
#' (y <- strcon(x, "echo", 1))
#' stopifnot(identical(y, x))
#' (y <- strcon(x, "echo", 2))
#' stopifnot(is.character(y), is.na(y) == is.na(x),
#'   nchar(y[1:4]) == nchar(x[1:4]) * 2)
#' # note that the 'using' argument would be recycled if longer than 1
#'
#' # repeat each character several times
#' (y <- strcon(x, "cecho", 0))
#' stopifnot(is.character(y), is.na(y) == is.na(x), nchar(y[1:4]) == 0)
#' (y <- strcon(x, "cecho", 1))
#' stopifnot(identical(y, x))
#' (y <- strcon(x, "echo", 2))
#' stopifnot(is.character(y), is.na(y) == is.na(x),
#'   nchar(y[1:4]) == nchar(x[1:4]) * 2)
#' # note that the 'using' argument would be recycled if longer than 1
#'
#' # padding with spaces on the right
#' (y <- strcon(x, "rpad", 2))
#' stopifnot(is.character(y), is.na(y) == is.na(x), nchar(y) == 2,
#'   identical(names(x), names(y)))
#' (y <- strcon(x, "rpad", 0))
#' stopifnot(is.character(y), is.na(y) == is.na(x), nchar(y[1:4]) == 0,
#'   identical(names(x), names(y)))
#' (y <- strcon(x, "rpad"))
#' stopifnot(is.character(y), is.na(y) == is.na(x),
#'   nchar(y[1:4]) == max(nchar(y[1:4])), identical(names(x), names(y)))
#'
#' # padding with spaces on the left
#' (y <- strcon(x, "lpad", 2))
#' stopifnot(is.character(y), is.na(y) == is.na(x), nchar(y) == 2,
#'   identical(names(x), names(y)))
#' (y <- strcon(x, "lpad", 0))
#' stopifnot(is.character(y), is.na(y) == is.na(x), nchar(y[1:4]) == 0,
#'   identical(names(x), names(y)))
#' (y <- strcon(x, "lpad"))
#' stopifnot(is.character(y), is.na(y) == is.na(x),
#'   nchar(y[1:4]) == max(nchar(y[1:4])), identical(names(x), names(y)))
#'
#' # padding with spaces on both sides
#' (y <- strcon(x, "bpad", 2))
#' stopifnot(is.character(y), is.na(y) == is.na(x), nchar(y) == 2,
#'   identical(names(x), names(y)))
#' (y <- strcon(x, "bpad", 0))
#' stopifnot(is.character(y), is.na(y) == is.na(x), nchar(y[1:4]) == 0,
#'   identical(names(x), names(y)))
#' (y <- strcon(x, "bpad"))
#' stopifnot(is.character(y), is.na(y) == is.na(x),
#'   nchar(y[1:4]) == max(nchar(y[1:4])), identical(names(x), names(y)))
#' 
strcon <- function(x,
    how = c("space", "word", "rword", "title", "revert", "chars", #"capply",
      "echo", "cecho", "rpad", "lpad", "bpad"),
    using) {
  copy_na <- function(x, y) {
    x[is.na(y)] <- NA_character_
    x
  }
  copy_attr <- function(x, y) {
    attributes(x) <- attributes(y)
    x
  }
  do_pad <- function(x, to, where) {
    if (missing(to))
      to <- max(nchar(x[!is.na(x)]), 0L)
    lacking <- pmax(to - nchar(y <- substring(x, 1L, to), allowNA = TRUE), 0L)
    copy_na(switch(where, paste(rep_all(" ", lacking), y, sep = ""),
      paste(y, rep_all(" ", lacking), sep = ""),
      paste(rep_all(" ", floor(lacking / 2)), y, 
        rep_all(" ", ceiling(lacking / 2)), sep = "")), x)
  }
  join <- function(x, y) {
    copy_na(unlist(lapply(x, paste, collapse = ""), recursive = FALSE), y)
  }
  rep_all <- function(x, y) {
    join(mapply(rep.int, x, y, USE.NAMES = FALSE, SIMPLIFY = FALSE), x)
  }
  rep_each <- function(x, y) {
    join(mapply(rep, strsplit(x, "", fixed = TRUE), each = y,
      USE.NAMES = FALSE, SIMPLIFY = FALSE), x)
  }
  char_apply <- function(X, FUN) {
    join(lapply(X = strsplit(X, "", fixed = TRUE), FUN = FUN), X)
  }
  titlecase <- function(x) {
    substring(x, 1L, 1L) <- toupper(substring(x, 1L, 1L))
    substring(x, 2L) <- tolower(substring(x, 2L))
    x
  }
  replace <- function(x, begin, end, middle, by) {
    x <- sub(end, "", sub(begin, "", x, perl = TRUE), perl = TRUE)
    gsub(middle, by, x, perl = TRUE)
  }
  case(match.arg(how),
    space = replace(x, "^\\s+", "\\s+$", "\\s+", " "),
    word = replace(x, "^[\\W_]+", "[\\W_]+$", "[\\W_]+", "_"),
    rword = replace(x, "^[\\W_.]+", "[\\W_.]+$", "[\\W_.]+", "."),
    title = copy_attr(titlecase(x), x),
    revert = char_apply(x, rev.default),
    chars = char_apply(x, function(y) sort.int(unique.default(y))),
#     capply = char_apply(x, using),
    echo = copy_attr(rep_all(x, using), x),
    cecho = copy_attr(rep_each(x, using), x),
    lpad = copy_attr(do_pad(x, using, 1L), x),
    rpad = copy_attr(do_pad(x, using, 2L), x),
    bpad = copy_attr(do_pad(x, using, 3L), x)
  )
}


################################################################################


#' Scan character vectors
#'
#' String scanning is the inverse of splitting: the pattern defines the
#' substrings to collect, not those to split at.
#'
#' @param x Character vector to be scanned, or convertible to such.
#' @param pattern Character scalar containing the scan pattern.
#' @param ignore.case Logical scalar passed to \code{gregexpr} from the 
#'   \pkg{base} package.
#' @param perl As above.
#' @param fixed As above.
#' @param useBytes As above.
#' @export
#' @return List of character vectors.
#' @keywords character manip
#' @seealso base::grexexpr
#' @family character-functions
#' @examples
#' x <- c("Huey, Dewey, and Louie", "Donald Duck ", " ", " Daisy", NA)
#' names(x) <- LETTERS[1:length(x)]
#' (y <- strscan(x, "\\S+"))
#' stopifnot(is.list(y), identical(names(y), names(x)), 
#'   sapply(y, is.character), sapply(y, length) == c(4, 2, 0, 1, 1), 
#'   is.na(y[[5]]))
#'
strscan <- function(x, pattern, ignore.case = FALSE, perl = TRUE,
    fixed = FALSE, useBytes = FALSE) {
  found <- gregexpr(pattern, x, ignore.case, perl, fixed, useBytes)
  #found[isna <- is.na(x)] <- NA_character_
  mapply(function(y, m) {
    if (all(is.na(m)))
      NA_character_
    else if (any(f <- m > 0L))
      substring(y, m[f], m[f] + attr(m, "match.length")[f] - 1L)
    else
      character()
#     mm <- attr(m, "match.length")[is.match <- m > 0L]
#     #substr(rep.int(y, length(m <- m[is.match])), m, m + mm - 1L)
#     m <- m[is.match]
#     substring(y, m, m + mm - 1L)
  }, x, found, SIMPLIFY = FALSE, USE.NAMES = TRUE)
}


################################################################################


#' Partition character vectors
#'
#' String partitioning results in a matrix or data frame with one row per
#' element of the input character vector. It contains in its three columns (i)
#' the part of the input string before the position at which the pattern
#' matched; (ii) the part covered by the pattern; (iii) the part after the
#' matched section. If no match occurred, the first column contains the input
#' string, the other contain empty strings or \code{NA} values, depending on
#' the value of \code{simplify}.
#'
#' @param x Character vector to be partitioned.
#' @param pattern Character scalar containing the partitoning pattern.
#' @param simplify Logical scalar indicating whether the result should be a
#'   matrix without column names, or a data frame with the column names
#'   \sQuote{pre_match}, \sQuote{match} and \sQuote{post_match}. If so, the
#'   \sQuote{match} and \sQuote{post_match} columns of non-matches contain
#'   \code{NA} values instead of empty strings.
#' @param ignore.case Logical scalar passed to \code{regexpr} from the 
#'   \pkg{base} package.
#' @inheritParams strscan
#' @export
#' @return Character matrix or data frame, with three columns.
#' @keywords character manip
#' @family character-functions
#' @examples
#' x <- c("Huey, Dewey, and Louie", "Donald Duck ", " ", " Daisy", NA)
#' names(x) <- LETTERS[1:length(x)]
#' (y <- strpart(x, "\\s+"))
#' stopifnot(is.data.frame(y), identical(rownames(y), names(x)),
#'   sapply(y, is.character), dim(y) == c(5, 3),
#'   complete.cases(y) == c(TRUE, TRUE, TRUE, TRUE, FALSE))
#'
strpart <- function(x, pattern, ignore.case = FALSE, perl = TRUE,
    fixed = FALSE, useBytes = FALSE, simplify = FALSE) {
  m <- regexpr(pattern, x, ignore.case, perl, fixed, useBytes)
  nc <- nchar(x, allowNA = TRUE)
  mm <- m + attr(m, "match.length") - 1L
  nomatch <- is.na(x) | m < 0L
  # move no-match coordinates beyond end of string
  mm[nomatch] <- m[nomatch] <- nc[nomatch] + 1L
  x <- cbind(substr(x, 1L, m - 1L), substr(x, m, mm), substr(x, mm + 1L, nc))
  if (L(simplify))
    return(x)
  x[nomatch, c(2L, 3L)] <- NA_character_
  x <- as.data.frame(x, stringsAsFactors = FALSE)
  colnames(x) <- c("pre_match", "match", "post_match")
  x
}


################################################################################



