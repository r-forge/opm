setGeneric("cor.test")

setMethod("cor.test", "OPMS", function(x, y = NULL, method = "pearson",
    use = "everything", subset = opm_opt("curve.param"), ...) {
  x <- extract(object = x, subset = subset, dataframe = FALSE, ci = FALSE,
    as.groups = NULL, as.labels = NULL, ...)
  if (length(y))
    y <- x[, y, drop = FALSE]
  cor(x, y, use, method)
}, sealed = SEALED)

setMethod("cor.test", "MOPMX", function(x, ...) {
  oapply(object = x, fun = cor.test, simplify = FALSE, ...)
}, sealed = SEALED)

