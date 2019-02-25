################################################################################


# Create portable file names. Try rescuing meaningful special characters.
#
new_name <- function(x, lower) {

  # rescue German umlauts etc. before removing accents
  for (from in names(UMLAUT_REPLACEMENT))
    x <- gsub(from, UMLAUT_REPLACEMENT[[from]], x, FALSE, FALSE, TRUE)

  # remove all accents (http://userguide.icu-project.org/transforms/general);
  # this is supposed to also correctly replace ligatures and German scharf-S
  # as well as French, Polish, Turkish and Scandinavian special characters,
  # among others; for characters to modify otherwise see above
  x <- stri_trans_general(x, "Any-Latin; Latin-ASCII")

  # replace selected ASCII characters by short descriptions (in ASCII)
  for (from in names(ASCII_REPLACEMENT))
    x <- gsub(from, ASCII_REPLACEMENT[[from]], x, FALSE, FALSE, TRUE)

  # replace remaining disallowed characters by underscores
  x <- gsub("[^A-Za-z0-9._-]+", "_", x, FALSE, TRUE)

  # remove leading hyphens (possible confusion with command-line options),
  # leading periods and leading underscores; same for trailing characters
  x <- sub("^[._-]+", "", x, FALSE, TRUE)
  x <- sub("[._-]+$", "", x, FALSE, TRUE)

  # reduce runs of underscores to single underscore; same for periods and
  # hyphens
  x <- gsub("_{2,}", "_", x, FALSE, TRUE)
  x <- gsub("-{2,}", "-", x, FALSE, TRUE)
  x <- gsub("\\.{2,}", ".", x, FALSE, TRUE)

  # replace dashes and periods surrounded by underscores
  for (from in c("_-_", "-_", "_-"))
    x <- gsub(from, "-", x, FALSE, FALSE, TRUE)
  for (from in c("_._", "._", "_."))
    x <- gsub(from, ".", x, FALSE, FALSE, TRUE)

  if (lower)
    tolower(x)
  else
    x
}


################################################################################


# Helps when traversing directories.
#
remove_dot_dirs <- function(x) x[!is.element(x, c(".", ".."))]


################################################################################


# Should return a named character vector representing the mapping.
#
suggest_renaming <- function(path, lower) {
  path <- sort.int(path, NULL, NA, TRUE)
  base <- basename(path)
  rename <- if (lower) # must examine everything
      rep_len(TRUE, length(base))
    else
      !grepl("^[A-Za-z0-9._-]+$", base, FALSE, TRUE)
  structure(.Data = file.path(dirname(path[rename]),
    new_name(base[rename], lower)), .Names = path[rename])
}


################################################################################
