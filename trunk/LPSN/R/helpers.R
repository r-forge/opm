assert_scalar <- function(x) {
  stopifnot(is.atomic(x), length(x) == 1L)
  x
}

download_lpsn_json <- function(object, endpoint, query) {
  internal <- get("dsmz_internal", object)
  base_url <- if (internal)
      "http://api.pnu-dev.dsmz.local"
    else
      "https://api.lpsn.dsmz.de"
  access_token <- get("access_token", object)
  result <- download_json(base_url, endpoint, query, access_token, FALSE)
  class(result) <- c(sprintf("lpsn_%s_result", endpoint), "lpsn_result")
  result
}

