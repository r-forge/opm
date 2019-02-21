sanitize <- function(paths = getwd(), lower = FALSE, directories = TRUE,
    enforce = 0L) {

  if (is.data.frame(paths)) {

    stopifnot(is.character(paths[, "From"]), is.character(paths[, "To"]),
      !anyNA(paths), !anyDuplicated.default(paths[, "From"]),
      # an attempt to assess whether the original ordering is still present
      !is.unsorted(attr(paths, "row.names")), is.character(paths[, "Problem"]),
      is.numeric(enforce), length(enforce) == 1L)

    if (enforce < 2L) {
      ok <- !nzchar(paths[, "Problem"])
      e <- duplicated.default(paths[ok, "To"])
      if (any(e))
        paths[ok, "Problem"][e] <- "duplicate target"
    }

    if (enforce < 1L) {
      ok <- !nzchar(paths[, "Problem"])
      e <- file.exists(paths[ok, "To"])
      if (any(e))
        paths[ok, "Problem"][e] <- "target exists"
    }

    if (enforce < 0L) {
      NULL
    } else {
      ok <- !nzchar(paths[, "Problem"])
      e <- !file.rename(paths[ok, "From"], paths[ok, "To"])
      if (any(e))
        paths[ok, "Problem"][e] <- "attempt failed"
    }

    return(paths)

  }

  stopifnot(is.character(paths), is.logical(lower), length(lower) == 1L,
    is.logical(directories), length(directories) == 1L)

  paths <- sort.int(unique.default(normalizePath(paths)), NULL, NA, TRUE)
  isdir <- file.info(paths, extra_cols = FALSE)[, "isdir"]

  result <- c(
    suggest_renaming(paths[!isdir], lower),
    # running list.files through lapply is faster than calling it directly
    lapply(lapply(X = paths[isdir], FUN = list.files, recursive = TRUE,
      full.names = TRUE, all.files = TRUE), suggest_renaming, lower),
    if (directories) c(
      lapply(lapply(X = paths[isdir], FUN = list.dirs),
        suggest_renaming, lower),
      suggest_renaming(remove_dot_dirs(paths[isdir]), lower)
    ),
    recursive = TRUE
  )

  if (is.null(names(result))) # if the result is empty
    names(result) <- rep_len("", length(result))

  different <- names(result) != result
  if (!all(different)) # can happen if lower=TRUE
    result <- result[different]

  data.frame(From = names(result), To = unname(result),
    Problem = rep_len("", length(result)), stringsAsFactors = FALSE)

}

