################################################################################


#' Print
#'
#' Print \code{\link{OPM}} or \code{\link{OPMS}} summaries to the screen.
#'
#' @param x Object of class \sQuote{OPM_Summary} or \sQuote{OPMS_Summary}.
#' @param ... Optional arguments passed to \code{formatDL}.
#' @return \code{x} is returned invisibly.
#' @keywords internal
#' @name print
#'
NULL

#' @rdname print
#' @method print OPMD_Listing
#' @export
#'
print.OPMD_Listing <- function(x, ...) {
  cat(formatDL(x = names(x), y = x, ...), sep = "\n")
  invisible(x)
}

#' @rdname print
#' @method print OPMS_Listing
#' @export
#'
print.OPMS_Listing <- function(x, ...) {
  for (name in rownames(x)) {
    cat(name, gsub(".", "-", name, FALSE, TRUE), sep = "\n")
    cat(formatDL(x = colnames(x), y = x[name, ], ...), sep = "\n")
    cat("\n")
  }
  invisible(x)
}

#' @rdname print
#' @method print OPM_Summary
#' @export
#'
print.OPM_Summary <- function(x, ...) {
  lapply(X = formatDL(x = names(x), y = unlist(x), ...), FUN = cat, sep = "\n")
  invisible(x)
}

#' @rdname print
#' @method print OPMS_Summary
#' @export
#'
print.OPMS_Summary <- function(x, ...) {
  for (i in seq_along(x)) {
    cat(i, sep = "\n")
    print(x[[i]])
    cat("\n")
  }
  tmpl <- "=> %s object with %i plates (%i aggregated, %i discretized)"
  tmpl <- paste(tmpl, "of type '%s', %i well(s) and about %i time point(s).")
  y <- attr(x, "overall")
  cat(sprintf(tmpl, "OPMS", y$Dimensions[1L], y$Aggregated, y$Discretized,
    y$Plate.type, y$Dimensions[3L], y$Dimensions[2L]), sep = "\n")
  invisible(x)
}

#' @rdname print
#' @method print MOPMX_Summary
#' @export
#'
print.MOPMX_Summary <- function(x, ...) {
  NextMethod()
  cat("", sprintf(
    "=> MOPMX object with %i element(s), details are shown above.", nrow(x)),
    " Access the elements with [[ or $ to apply specific methods.",
    sep = "\n")
  invisible(x)
}

#' @rdname print
#' @method print print_easy
#' @export
#'
print.print_easy <- function(x, ...) {
  to_map <- function(items) if (is.null(names(items)))
    items
  else
    as.list(items)
  cat(as.yaml(if (is.list(x))
    rapply(x, to_map, "ANY", NULL, "replace")
  else
    to_map(x)))
  invisible(x)
}


################################################################################


#' Plotting helper functions acting on numeric data
#'
#' Range numbers, i.e. divide them by their maximum. In \sQuote{extended} mode,
#' the minimum is subtracted beforehand. It is possible to replace ranging by
#' standardization (z-scores). Alternatively, guess a suitable \code{cex}
#' parameter for \code{\link{level_plot}} (0.5 is fine for the original number
#' of wells, 96), or determine number of rows/columns in plot for given number
#' of fields, or determine an optimal range for plotting, or draw a confidence
#' interval, or returnthe maximal value of an object plus a certain offset.
#'
#' @param object Numeric vector or array, or numeric scalar, or
#'   \code{\link{OPMX}} object. For \code{draw_ci}, a four-element numeric
#'   vector containing (i) the left margin of the CI; (ii) the point estimate;
#'   (iii) the right margin; (iv) the position on the y axis. The point estimate
#'   can be \code{NA} at any time; whether the margins can also be \code{NA}
#'   depends on \code{na.action}.
#' @param by Numeric scalar (width/height relation).
#' @param extended Logical scalar. Subtract the minimum in both numerator and
#'   denominator of the ranging formula? If \code{zscores} is \code{TRUE}, the
#'   meaning is different.
#' @param zscores Logical scalar. Calculate z-scores instead of ranging? If
#'   \code{extended} is \code{FALSE}, this is done using the mean and the
#'   standard deviation; otherwise, the median and the MAD are used.
#' @param na.rm Logical scalar. Remove \code{NA} values when calculating the
#'   relevant aggregated values (minimum, maximum, mean, standard deviation,
#'   median and/or MAD)?
#' @param fac Numeric scalar. After conducting the proper ranging process,
#'   \code{object} is multiplied by \code{fac}.
#' @param target Numeric scalar. Target difference between min and max. If
#'   \code{NULL}, this is simply derived from the range of \code{object}.
#' @param align Character scalar. Where to put the real values relative to min
#'   and max of \code{target}.
#' @param offset Numeric scalar. A minimal distance to the margins.
#' @param prop.offset Numeric scalar. As an alternative to \code{offset}, it can
#'   be specified as a proportion of \code{target}.
#' @param col Character scalar. Name of the colour to be used.
#' @param cex Numeric scalar. Magnification for CI margin symbols and point
#'   estimate. Also affects line width, and proportionally so.
#' @param na.action Character scalar. What to do if a margin value is \code{NA}.
#' @param theor.max Logical scalar. Use the theoretical or the real improved
#'   maximum? If \code{TRUE}, \code{by} is ignored.
#' @param ... Optional arguments passed between the methods.
#' @return Numeric vector or matrix. \code{draw_ci} invisibly returns
#'   \code{object}. For \code{improved_max}, let \code{n} be the smallest
#'   integer value for which \code{n * by >= object} holds. The result is then
#'   equal to \code{(n + 1) * by}.
#' @keywords internal
#'
setGeneric("ranging", function(object, ...) standardGeneric("ranging"))

setMethod("ranging", "numeric", function(object, extended = !zscores,
    zscores = FALSE, na.rm = TRUE, fac = 1) {
  LL(extended, zscores, na.rm)
  result <- if (zscores) {
    if (extended) {
      center <- median(object, na.rm = na.rm)
      (object - center) / mad(object, center = center, na.rm = na.rm)
    } else {
      (object - mean(object, na.rm = na.rm)) / sd(object, na.rm = na.rm)
    }
  } else {
    if (extended) {
      min.object <- min(object, na.rm = na.rm)
      (object - min.object) / (max(object, na.rm = na.rm) - min.object)
    } else {
      object / max(abs(object), na.rm = na.rm)
    }
  }
  must(result * fac)
}, sealed = SEALED)

setMethod("ranging", "array", function(object, ...) {
  map_values(object = object, mapping = ranging, ...)
}, sealed = SEALED)

setMethod("ranging", "list", function(object, ...) {
  relist(ranging(unlist(as.relistable(object)), ...))
}, sealed = SEALED)

#= guess_cex ranging

#' @rdname ranging
#'
setGeneric("guess_cex", function(object, ...) standardGeneric("guess_cex"))

setMethod("guess_cex", "numeric", function(object) {
  0.5 * sqrt(96 / object)
}, sealed = SEALED)

#= best_layout ranging

#' @rdname ranging
#'
setGeneric("best_layout",
  function(object, ...) standardGeneric("best_layout"))

setMethod("best_layout", "numeric", function(object, by = 0.75) {
  LL(object, by)
  if (object < 0)
    stop("a negative number of fields makes no sense")
  if (object < 2)
    return(c(object, object))
  large <- ceiling(sqrt((1 / by) * object)) # => error unless 'by' is numeric
  small <- ceiling(object / large)
  c(large, small)
}, sealed = SEALED)

#= best_range ranging

#' @rdname ranging
#'
setGeneric("best_range",
  function(object, ...) standardGeneric("best_range"))

setMethod("best_range", "numeric", function(object, target,
    align = c("center", "left", "right"),
    offset = 0, prop.offset = 0) {
  orig.range <- range(object)
  orig.diff <- orig.range[2L] - orig.range[1L]
  case(length(target), target <- orig.diff, LL(target))
  LL(offset, prop.offset)
  if (offset == 0)
    offset <- target * prop.offset
  total <- target + 2 * offset
  if (total < orig.diff) {
    fmt <- "target (%s) + 2 * offset (%s) smaller than original range (%s)"
    stop(sprintf(fmt, target, offset, orig.diff))
  }
  case(match.arg(align),
    center = {
      add <- total / 2
      mean(orig.range) + c(-add, add)
    },
    left = orig.range[1L] + c(-offset, target + offset),
    right = orig.range[2L] + c(-target - offset, offset)
  )
}, sealed = SEALED)

#= improved_max ranging

#' @rdname ranging
#'
setGeneric("improved_max",
  function(object, ...) standardGeneric("improved_max"))

setMethod("improved_max", "numeric", function(object, by = 10) {
  LL(by)
  m <- max(object)
  while (by >= m)
    by <- by / 10
  ceiling(m / by) * by + by # => error unless 'by' is numeric
}, sealed = SEALED)

setMethod("improved_max", "OPMX", function(object, theor.max = TRUE, by = 10) {
  if (is.double(L(theor.max)))
    return(theor.max)
  if (theor.max)
    return(THEOR_RANGE[[2L]])
  improved_max(max(object), by)
}, sealed = SEALED)


#= draw_ci ranging

#' @rdname ranging
#'
setGeneric("draw_ci", function(object, ...) standardGeneric("draw_ci"))

setMethod("draw_ci", "numeric", function(object, col = "blue", cex = 1,
    na.action = c("warn", "error", "ignore")) {
  LL(object, .wanted = 4L)
  if (anyNA(c(left <- object[1L], right <- object[3L]))) {
    msg <- "cannot draw CI because left or right margin is 'NA'"
    case(match.arg(na.action),
      warn = warning(msg),
      error = stop(msg),
      ignore = NULL
    )
  }
  if (is.na(y <- object[4L]))
    stop("position on y axis must be provided")
  segments(x0 = left, y0 = y, x1 = right, y1 = y, lwd = cex, col = col)
  text(x = left, y = y, labels = "(", col = col, cex = cex)
  text(x = right, y = y, labels = ")", col = col, cex = cex)
  if (!is.na(point <- object[2L]))
    points(x = point, y = y, col = col, lwd = cex, pch = 19L, cex = cex)
  invisible(object)
}, sealed = SEALED)


################################################################################


#' Plotting helper functions acting on character data
#'
#' Helper functions to determine the value of a measurement interpretable as
#' negative control or to create a title used as value of the \sQuote{main}
#' argument of the plotting functions.
#'
#' @param object \code{\link{OPMX}} object.
#' @param neg.ctrl If \code{NULL} or \code{FALSE}, ignore \code{data} and return
#'   \code{NULL}. If \code{TRUE}, call \code{minmax} with \code{data} as sole
#'   argument. If a character scalar, call \code{max} with \code{data} as first
#'   and \code{neg.ctrl} as second argument. If \code{neg.ctrl} is a numeric
#'   value, it is returned.
#' @param settings See the \code{main} argument of \code{\link{xy_plot}}.
#' @return Numeric or character scalar or \code{NULL}.
#' @keywords internal
#'
setGeneric("negative_control",
  function(object, ...) standardGeneric("negative_control"))

setMethod("negative_control", "OPMX", function(object, neg.ctrl) {
  if (!length(neg.ctrl) || is.numeric(neg.ctrl))
    neg.ctrl
  else if (is.character(neg.ctrl)) {
    result <- vapply(neg.ctrl, function(x)
      tryCatch(expr = minmax(object, neg.ctrl), error = function(e) {
        warning("cannot get negative control from selected position ",
        "(deleted?); error was: ", conditionMessage(e), call. = FALSE)
        -1
      }), 0)
    result[result >= 0]
  } else if (is.logical(neg.ctrl)) {
    if (L(neg.ctrl))
      minmax(object)
    else
      NULL
  } else {
    stop("object 'neg.ctrl' must be either empty or a 'character', 'logical' ",
      "or 'numeric' vector")
  }
}, sealed = SEALED)

#= main_title negative_control

#' @rdname negative_control
#'
setGeneric("main_title", function(object, ...) standardGeneric("main_title"))

setMethod("main_title", "OPMX", function(object, settings) {
  if (is.character(settings) || is.expression(settings))
    settings <- list(predef = settings)
  else if (is.logical(settings))
    settings <- list(use = settings)
  else if (is.numeric(settings))
    settings <- list(max = settings)
  else
    settings <- as.list(settings)
  if (!is.null(settings$predef) && nzchar(settings$predef))
    return(settings$predef) # nzchar() works for expressions, too
  settings <- insert(settings, use = TRUE, full = TRUE, .force = FALSE)
  if (settings$use) {
    settings$use <- NULL
    do.call(plate_type, c(list(object = object), settings))
  } else {
    NULL
  }
}, sealed = SEALED)


################################################################################


#' Safely select colours or create colour regions.
#'
#' Call \code{\link{select_colors}} and if this does not work return the input
#' argument as-is. Alternatively, create default colour regions for use with
#' \code{\link{level_plot}}.
#'
#' @param set Character vector passed to \code{\link{select_colors}}.
#' @param colours Character or integer vector with at least two distinct
#'   colours. If \code{NULL} or empty, default colours are chosen.
#' @param space Passed to \code{colorRampPalette}.
#' @param bias Passed to \code{colorRampPalette}.
#' @param n Passed to the function returned by \code{colorRampPalette}.
#' @return Character vector of colour codes.
#' @keywords internal
#'
try_select_colors <- function(set) {
  tryCatch(expr = select_colors(set), error = function(e) set)
}

#' @rdname try_select_colors
#'
default_color_regions <- function(colors, space, bias, n) {
  colorRampPalette(colors = unique(colors), space = space, bias = bias)(n)
}


