summary.bacdive_result <- function(object, ...) {
  c(
    list(
      class = paste0(class(object), collapse = " < "),
      parts = paste0(names(object), collapse = ", ")
    ),
    lapply(object, function(x)
      if (is.numeric(x) && length(x) == 1L)
        x
      else
        length(x) > 0L
    )
  )
}

print.bacdive_result <- function(x, ...) {
  print_summary(x, ...)
}

