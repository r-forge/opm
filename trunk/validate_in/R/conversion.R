

################################################################################


setAs("ATOMIC_VALIDATOR", "list", function(from) {
  structure(list(from@what), names = from@how)
})


setAs("list", "ATOMIC_VALIDATOR", function(from) {
  new("ATOMIC_VALIDATOR", how = names(from)[[1L]], what = from[[1L]])
})


setAs("ATOMIC_VALIDATOR", "function", function(from) {
  safe_nchar <- function(x) ifelse(is.na(x), NA_integer_, nchar(x))
  switch(from@how,
    enum = function(x) all(x %in% from@what), # like kwalify
    lower_bound = function(x) all(x >= from@what), # kwalify: 'min'
    min_chars = function(x) all(safe_nchar(x) >= from@what), # kwalify: 'length'
    min_elems = function(x) length(x) >= from@what, # kwalify: 'min-elems'
    max_chars = function(x) all(safe_nchar(x) <= from@what), # kwalify: 'length'
    max_elems = function(x) length(x) <= from@what, # kwalify: 'max-elems'
    pattern = function(x) all(vapply(from@what, grepl, logical(length(x)),
      x = x, perl = TRUE)), # like kwalify
    pattern_fixed = function(x) all(vapply(from@what, grepl, logical(length(x)),
      x = x, fixed = TRUE)),
    pattern_ignoring_case = function(x) all(vapply(from@what, grepl,
      logical(length(x)), x = x, perl = TRUE, ignore.case = FALSE)),
    sorted = function(x) if (from@what)
        !is.unsorted(x)
      else
        is.unsorted(x),
    type = function(x) typeof(x) %in% from@what, # like kwalify
    unique = function(x) if (from@what)
        !any(duplicated(x))
      else
        any(duplicated(x)),
    upper_bound = function(x) all(x <= from@what), # kwalify: 'max'
    function(x) stop(sprintf("uninterpretable entry in 'how' slot: '%s'",
      from@how))
  )
})


################################################################################


setAs("ATOMIC_VALIDATION", "logical",
  function(from) structure(from@result, names = from@how))


setAs("ATOMIC_VALIDATION", "list", function(from) stop("not yet implemented"))


################################################################################


setAs("ATOMIC_VALIDATORS", "list", function(from) {
  unlist(lapply(from@checks, as, "list"), FALSE, TRUE)
})


setAs("list", "ATOMIC_VALIDATORS", function(from) new("ATOMIC_VALIDATORS",
  checks = mapply(new, how = names(from), what = from, SIMPLIFY = FALSE,
    MoreArgs = list(Class = "ATOMIC_VALIDATOR"), USE.NAMES = FALSE)))


################################################################################


setAs("ATOMIC_VALIDATIONS", "logical", function(from) {
  vapply(from@checks, as, NA, "logical")
})


################################################################################


setAs("ELEMENT_VALIDATIONS", "logical", function(from) {
  result <- c(present = from@present, vapply(from@checks, as, NA, "logical"))
  if (!result[1L])
    result[-1L] <- NA
  result
})


################################################################################

