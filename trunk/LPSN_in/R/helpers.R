################################################################################


# Non-public function that does the LPSN-specific download work.
#
download_lpsn_json <- function(object, endpoint, query) {
  internal <- get("dsmz_internal", object)
  url <- if (length(query))
      compose_url(if (internal)
          "http://api.pnu-dev.dsmz.local"
        else
          "https://api.lpsn.dsmz.de", endpoint, query)
    else
      endpoint # here we assume that the full URL is already given
  verbose <- force_integer(Sys.getenv("DSMZ_API_VERBOSE", ""))
  if (internal)
    verbose <- verbose + 1L
  result <- download_json_with_retry(url, object, verbose)
  class(result) <- "lpsn_result"
  result
}


################################################################################
