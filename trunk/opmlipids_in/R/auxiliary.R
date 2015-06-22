

################################################################################
################################################################################
#
# Internal helper functions of the package
#


trim_whitespace <- function(x) {
  sub("\\s+$", "", sub("^\\s+", "", x, FALSE, TRUE), FALSE, TRUE)
}


################################################################################


# For checking data frames (or lists) and generating nicer error messages.
# This is also in use in 'opmlipids', try keeping that in sync.
assert_elements <- function(x, expected) {
  element_is <- function(x, name, classfun) classfun(x[[name]])
  ok <- names(expected) %in% names(x)
  result <- sprintf("element '%s' is missing", names(expected)[!ok])
  expected <- expected[ok]
  ok <- mapply(FUN = element_is, name = names(expected), MoreArgs = list(x = x),
    classfun = lapply(sprintf("is.%s", expected), match.fun))
  c(result, sprintf("element '%s' is not of class '%s'",
    names(expected)[!ok], expected[!ok]))
}


################################################################################


# Interpret sample ID as used at DSMZ. Return four-column data frame with
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
  result[, "Organism"] <- sub("\\s+$", "", result[, "Organism"], FALSE, TRUE)
  x <- substr(x, m + 1L, m + attr(m, "match.length") - 2L)
  cbind(result, cultivation_components(x))
}


################################################################################


# Where 'x' contains fatty-acid names.
fs_to_html <- function(x) {
  x <- safe_labels(x, "html")
  gsub("(?<=\\bC)(\\d+:\\d+\\b[^/]*)", "<sub>\\1</sub>", x, FALSE, TRUE)
}


################################################################################


# Where 'plate.type' and 'what' are character scalars.
get_for <- function(plate.type, what) {
  get(what, get(plate.type, PLATE_TYPE_SETTINGS))
}


################################################################################


# See plate_type() for the usage.
str_head <- function(x) sub("[\\W_].*", "", x[[1L]], FALSE, TRUE)


################################################################################


# Where 'x' is a FAMES or similar object.
close_index_gaps <- function(x) {
  if (any(bad <- vapply(x, is.null, NA))) {
    warning("closing gaps in indexes")
    return(x[!bad])
  }
  x
}


################################################################################


# Where 'x' is a list.
joinable <- function(x) {
  if (!all(vapply(x, is, NA, "FAME") | vapply(x, is, NA, "FAMES")))
    return(FALSE)
  pt <- vapply(x, plate_type, "")
  all(duplicated.default(pt[!is.na(pt)])[-1L])
}


################################################################################


# Where 'x' is a list.
join_if_possible <- function(x) {
  if (joinable(x))
    new("FAMES", plates = unlist(lapply(x, plates), FALSE, FALSE))
  else
    x
}


################################################################################


# Find the closest occurrence of the elements of a vector in another one. From
# http://stackoverflow.com/questions/10160400/r-find-nearest-index .
nearest <- function(x, table) {
  idx <- findInterval(x, table, FALSE, TRUE)
  idx + (2 * x > table[idx] + table[idx + 1L])
}


################################################################################


# Read a single RTF file and return a list of FAME objects if 'level' is 6. For
# lower levels, return according intermediary objects. 'delete'
read_midi_rtf <- function(con, level = 6L,
  delete = c("fonttbl", "colortbl", "stylesheet", "*", "header", "footer",
    "pict", "pntxta", "pntxtb"), ...) {

  # Prepend a backslash if necessary.
  to_macro <- function(x) {
    bad <- !grepl("^\\\\", x, FALSE, TRUE)
    x[bad] <- sprintf("\\%s", x[bad])
    x
  }

  macro <- function(x) attr(x, "macro")

  # A simple RTF parser. RTF cannot be generated back from the resulting object
  # because delimiters get removed from the control words.
  parse_rtf <- function(x) {
    x <- gsub("(\\\\[a-z]+)-?\\d+", "\\1", x, FALSE, TRUE)
    parseLatex(gsub("%", "\\%", x, FALSE, FALSE, TRUE))
  }

  # Newer version, but since flatten_rtf() has been improved this is currently
  # the limiting step. Perhaps this can be omitted and replaced by deleting
  # stuff after splitting into paragraphs? But the usual blocks to delete do not
  # necessarily occur at a given level.
  subset_rtf <- function(x, delete) {
    must_delete <- function(x, y) is.character(x[[1L]]) && any(x[[1L]] %in% y)
    delete_recursively <- function(x, delete) {
      bad <- is.block <- vapply(x, is.list, NA)
      bad[bad] <- vapply(x[bad], must_delete, NA, delete)
      is.block <- is.block & !bad
      x[is.block] <- lapply(x[is.block], delete_recursively, delete)
      x[bad] <- NULL
      x
    }
    delete_recursively(x, to_macro(delete))
  }

  # A 'flat RTF' object is a character vector with an equal-length 'macro' tag.
  flatten_rtf <- function(object) {
    tags <- rapply(object, attr, which = "latex_tag")
    if (length(tags) != length(x <- unlist(object, TRUE, FALSE)))
      stop("malformatted RTF object: not as many tags as elements")
    if (length(bad <- setdiff(tags, c("MACRO", "TEXT"))))
      stop("malformatted RTF object: unknown tag ", bad[1L])
    attr(x, "macro") <- tags == "MACRO"
    x
  }

  # Create a list of 'flat RTF' objects.
  cut_flat_rtf <- function(x, breaks) {
    breaks <- sections(x %in% to_macro(breaks) & macro(x), FALSE)
    mapply(structure, .Data = split.default(x, breaks), SIMPLIFY = FALSE,
      macro = split.default(macro(x), breaks), USE.NAMES = FALSE)
  }

  # Simplify elements of x, which is a list of RTF paragraphs, depending on
  # whether or not the element holds an RTF table.
  simplify_paragraphs <- function(x) {
    # Reduce non-table paragraphs to the informative content in a named
    # character vector.
    simplify_nontable_paragraph <- function(x) {
      x <- paste0(x[!macro(x)], collapse = "\n")
      x <- strsplit(x, " {5,}|\n", FALSE, TRUE)
      x <- trim_whitespace(unlist(x, FALSE, FALSE))
      if (!length(x <- x[nzchar(x)]))
        return(character())
      x <- strsplit(x, "\\s*:\\s*", FALSE, TRUE)
      len <- vapply(x, length, 0L)
      x[pos] <- lapply(x[pos <- len == 1L], c, NA_character_)
      x[pos] <- lapply(x[pos <- len > 2L], function(y)
        c(y[1L], paste0(y[-1L], collapse = ":")))
      x <- do.call(rbind, x)
      structure(x[, 2L], names = x[, 1L])
    }
    # Table rows are located between \\trowd and \\trow, cell content is located
    # before \\cell. We assume there is only one table per paragraph. Empty
    # strings are added in place of empty cells.
    simplify_table_paragraph <- function(x) {
      x <- lapply(cut_flat_rtf(x, c("trowd", "row")), function(x)
        trim_whitespace(ifelse(macro(x), "", x)[which(x == "\\cell") - 1L]))
      do.call(rbind, x) # zero-length rows disappear at this step
    }
    is.table <- vapply(x, function(y) "\\trowd" %in% y[macro(y)], NA)
    x[is.table] <- lapply(x[is.table], simplify_table_paragraph)
    x[!is.table] <- lapply(x[!is.table], simplify_nontable_paragraph)
    x[vapply(x, length, 0L) > 0L]
  }

  # Interpret elements of x, which is a list of simplified RTF paragraphs,
  # depending on whether or not the element is a matrix.
  interpret_paragraphs <- function(x) {
    parse_table <- function(x) { # Convert MIDI matrix to data frame.
      x[grepl("^-+$", x, FALSE, TRUE)] <- NA_character_
      headers <- x[1L, ]
      x <- as.data.frame(x[-1L, , drop = FALSE], stringsAsFactors = FALSE)
      colnames(x) <- headers
      rownames(x) <- NULL
      if (all(c("Library", "Sim Index", "Entry Name") %in% headers)) {
        x[, "Sim Index"] <- as.double(chartr(",", ".", x[, "Sim Index"]))
        attr(x, "kind") <- "Matches"
      } else {
        attr(x, "kind") <- "Measurements"
      }
      x
    }
    tables <- lapply(x[is.mat <- vapply(x, is.matrix, NA)], parse_table)
    x <- unlist(lapply(x[!is.mat], as.list), FALSE, TRUE)
    for (table in tables) {
      previous <- x[[kind <- attr(table, "kind")]]
      if (!is.null(previous) && !all(is.na(previous)))
        stop(kind, " entry already present")
      attr(table, "kind") <- NULL
      x[[kind]] <- table
    }
    x
  }

  make_midi_fame <- function(x, filename) {
    wanted <- c("Measurements", "Method", "Sample ID", "ID Number", "Type")
    if (length(bad <- which(!match(wanted, names(x), 0L))))
      stop(sprintf("entry '%s' is missing", wanted[bad[1L]]))
    measurements <- as(x$Measurements, "MIDI")
    x$Measurements <- NULL
    x[[get_for("MIDI", "file.entry")]] <- filename
    new("FAME", measurements = measurements, metadata = x)
  }

  x <- parse_rtf(readLines(con, ...))
  if (level < 1L)
    return(x) # 'LaTeX' object
  x <- flatten_rtf(subset_rtf(x, delete))
  if (level < 2L)
    return(x) # flat RTF: character vector plus logical 'macro' attribute
  x <- cut_flat_rtf(x, "page")
  if (level < 3L)
    return(x) # list of flat-RTF objects, one per page
  x <- lapply(x, cut_flat_rtf, "par")
  if (level < 4L)
    return(x) # list of lists of flat-RTF objects, one per paragraph
  x <- lapply(x, simplify_paragraphs)
  x <- x[vapply(x, length, 0L) > 0L]
  if (level < 5L)
    return(x) # simplified list of lists of flat-RTF objects, one per paragraph
  x <- lapply(x, interpret_paragraphs)
  if (level < 6L)
    return(x) # list of lists, data frames already created
  lapply(x, make_midi_fame, if (is.character(con))
      con
    else
      "-") # one FAME object per page
}


################################################################################


# Convert names of fatty acids from the MIDI system to a standardized way to
# express them. 'x' is a character vector.
norm_fa <- function(x, warn) {
  y <- FA_MAP[x]
  if (any(isna <- is.na(y))) {
    ok <- x[isna] %in% FA_MAP
    y[isna][ok] <- x[isna][ok]
    isna[isna][ok] <- FALSE
    if (any(isna)) {
      if (warn)
        warning("could not find these fatty-acid names:\n",
          paste0(x[isna], collapse = "\n"))
      y[isna] <- x[isna]
    }
  }
  unname(y)
}


################################################################################

