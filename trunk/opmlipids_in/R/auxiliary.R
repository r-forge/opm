

################################################################################
################################################################################
#
# Internal helper functions of the package
#


# Prepend a backslash if necessary.
to_macro <- function(x) {
  bad <- !grepl("^\\\\", x, FALSE, TRUE)
  x[bad] <- sprintf("\\%s", x[bad])
  x
}


macro <- function(x) attr(x, "macro")


# Interpret sample ID as used at DSMZ. Return Four-column data frame with
# character vectors as elements. This is hard to generalise and thus internal.
interpret_dsmz_sample_id <- function(x) {
  cultivation_components <- function(x) {
    interpret <- function(x) {
      duration <- grepl("^\\d+d$", x, TRUE, TRUE)
      x <- ifelse(duration, tolower(x), toupper(x))
      names(x) <- rep.int("Medium", length(x))
      names(x)[x %in% c("NDK", "VDK")] <- "Timepoint"
      names(x)[duration] <- "Duration"
      x
    }
    x <- sub("^\\s+", "", sub("\\s+$", "", x, FALSE, TRUE), FALSE, TRUE)
    x <- lapply(strsplit(x, "\\s*,\\s*", FALSE, TRUE), interpret)
    collect(x, "values", dataframe = TRUE, stringsAsFactors = FALSE)
  }
  x <- sub("^\\s+", "", x, FALSE, TRUE)
  m <- regexpr("\\([^()]+\\)", x, FALSE, TRUE)
  result <- data.frame(Organism = ifelse(m > 0L, substr(x, 1L, m - 1L), x),
    stringsAsFactors = FALSE)
  result$Organism <- sub("\\s+$", "", result$Organism, FALSE, TRUE)
  x <- substr(x, m + 1L, m + attr(m, "match.length") - 2L)
  cbind(result, cultivation_components(x))
}


# Where 'x' contains fatty-acid names.
fs_to_html <- function(x) {
  x <- safe_labels(x, "html")
  gsub("(?<=\\bC)(\\d+:\\d+\\b[^/]*)", "<sub>\\1</sub>", x, FALSE, TRUE)
}


# Where 'plate.type' and 'what' are character scalars.
get_for <- function(plate.type, what) {
  get(what, get(plate.type, PLATE_TYPE_SETTINGS))
}


# See plate_type() for the usage.
str_head <- function(x) sub("[\\W_].*", "", x[[1L]], FALSE, TRUE)


# Where 'x' is a FAMES or similar object.
close_index_gaps <- function(x) {
  if (any(bad <- vapply(x, is.null, NA))) {
    warning("closing gaps in indexes")
    return(x[!bad])
  }
  x
}


# Where 'x' is a list.
joinable <- function(x) {
  if (!all(vapply(x, is, NA, "FAME") | vapply(x, is, NA, "FAMES")))
    return(FALSE)
  pt <- vapply(x, plate_type, "")
  all(duplicated.default(pt[!is.na(pt)])[-1L])
}


# Where 'x' is a list.
join_if_possible <- function(x) {
  if (joinable(x))
    new("FAMES", plates = unlist(lapply(x, plates), FALSE, FALSE))
  else
    x
}


# Find the closest occurrence of the elements of a vector in another one. From
# http://stackoverflow.com/questions/10160400/r-find-nearest-index .
nearest <- function(x, table) {
  idx <- findInterval(x, table, FALSE, TRUE)
  idx + (2 * x > table[idx] + table[idx + 1L])
}


# Find unnamed fatty acids and accordingly re-calculate all percentages. 'x' is
# a data frame as stored in the 'measurements' slot of a FAME object.
recalculate_percents <- function(x) {
  interpolate_rfact <- function(x) {
    if (any(x$Interpol <- is.na(x$RFact) & !is.na(x$RT))) {
      m <- lm(log(x$RFact) ~ log(x$RT))
      x$RFact <- ifelse(x$Interpol, exp(predict(m, x)), x$RFact)
    }
    x
  }
  x <- interpolate_rfact(x)
  skip <- grepl("^\\s*(<\\s+min|>\\s+max)\\s+rt\\s*$", x$Comment1, TRUE,
    TRUE) | grepl("^\\s*Summed\\s+Feature\\s+\\d+\\s*$", x[, "Peak Name"],
    TRUE, TRUE)
  val <- ifelse(skip, NA_real_, x$Response * x$RFact)
  val <- 100 * val / sum(val, na.rm = TRUE)
  x$VALUE2 <- val
  is.nn <- is.na(x$VALUE) & !is.na(x$VALUE2)
  nn <- ifelse(nzchar(x$`Peak Name`), x$`Peak Name`,
    sprintf("unknown %0.2f", x$ECL))
  nn[is.nn] <- norm_fa(nn[is.nn], TRUE)
  rownames(x) <- make.unique(ifelse(is.nn, nn, rownames(x)), APPENDIX)
  x
}


