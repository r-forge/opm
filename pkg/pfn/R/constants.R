UMLAUT_REPLACEMENT <- c(
  # German umlauts (stri_trans_general would convert to single character)
  "\u{00C4}" = "AE",
  "\u{00E4}" = "ae",
  "\u{00D6}" = "OE",
  "\u{00F6}" = "oe",
  "\u{00DC}" = "UE",
  "\u{00FC}" = "ue",
  # Scandinavian (stri_trans_general would convert to single character)
  "\u{00D8}" = "OE",
  "\u{00F8}" = "oe"
)

ASCII_REPLACEMENT <- c(
  # from https://github.com/splitbrain/sanity/blob/master/sanity.pl
  "&" = "_and_",
  "@" = "_at_",
  # some additions; "big" supposed to mean "important" but shorter
  "?" = "_maybe_",
  "!" = "_big_",
  "*" = "_star_",
  "$" = "_dollar_",
  "%" = "_percent_",
  "+" = "_plus_",
  # there does not seem to be another English abbreviation for "number"
  "#" = "_No_"
)
