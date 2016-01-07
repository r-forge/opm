prepare_class_names <- function(x) UseMethod("prepare_class_names")

prepare_class_names.character <- function(x) {
  x <- unique.default(c("character", x))
  if ("ANY" %in% x)
    "ANY"
  else
    x
}

