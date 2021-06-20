################################################################################


# Non-public function that asserts that 'x' is an atomic vector of length 1
# whose element is non-missing.
#
assert_scalar <- function(x) {
  stopifnot(is.atomic(x), length(x) == 1L)
  x
}


# Non-public function that does the LPSN-specific download work.
#
download_lpsn_json <- function(object, endpoint, query) {
  internal <- get("dsmz_internal", object)
  base_url <- if (internal)
      "http://api.pnu-dev.dsmz.local"
    else
      "https://api.lpsn.dsmz.de"
  access_token <- get("access_token", object)
  # for debugging replace 'FALSE' by 'internal'
  result <- download_json(base_url, endpoint, query, access_token, FALSE)
  class(result) <- c(sprintf("lpsn_%s_result", endpoint), "lpsn_result")
  result
}


################################################################################
