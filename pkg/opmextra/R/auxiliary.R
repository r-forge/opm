quote_protected <- function(x, s) {
  sprintf(sprintf("%s%%s%s", s, s),
    gsub(s, sprintf("%s%s", s, s), x, FALSE, FALSE, TRUE))
}
