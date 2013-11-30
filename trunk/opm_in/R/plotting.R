

################################################################################
################################################################################
#
# Methods for summary(), show() and print()
#


#' Summarise \acronym{OPMX} or \acronym{MOPMX} objects
#'
#' Generate a summary (which also prints nicely to the screen), or display an
#' \code{\link{OPM}}, \code{\link{OPMS}} or \code{\link{MOPMX}} object on
#' screen.
#'
#' @param object \code{\link{OPM}}, \code{\link{OPMS}} or \code{\link{MOPMX}}
#'   object.
#' @param ... Optional arguments to be included in the output.
#' @export
#' @return For the \code{\link{OPM}} method, a named list of the class
#'   \code{OPM_Summary}, returned invisibly. The \sQuote{metadata} entry is
#'   the number of non-list elements in \code{\link{metadata}}. For the
#'   \code{\link{OPMS}} method, a list of such lists (one per plate), also
#'   returned invisibly, with the class set to \code{OPMS_Summary} and some
#'   information on the entire object in the attribute \sQuote{overall}.
#' @details Currently the \code{show} methods are just wrappers for the
#'   \code{summary} methods for these objects with an additional call to
#'   \code{print}. The \acronym{CMAT} method is only for internal use.
#' @family plotting-functions
#' @keywords attribute
#' @seealso base::summary base::formatDL methods::show base::print
#' @examples
#'
#' # OPM method
#' (x <- summary(vaas_1))
#' stopifnot(is.list(x), is.object(x))
#' vaas_1 # calls show()
#'
#' # OPMS method
#' (x <- summary(vaas_4))
#' stopifnot(is.list(x), length(x) == 4L, all(sapply(x, is.list)),
#'   is.object(x))
#' vaas_4 # calls show()
#'
setGeneric("summary")

setMethod("summary", OPM, function(object, ...) {
  result <- list(
    Class = class(object),
    `From file` = csv_data(object, what = "filename"),
    `Hours measured` = hours(object),
    `Number of wells` = length(wells(object)),
    `Plate type` = plate_type(object),
    Position = csv_data(object, what = "position"),
    `Setup time` = csv_data(object, what = "setup_time"),
    Metadata = sum(rapply(object@metadata, function(item) 1L)),
    Aggregated = has_aggr(object),
    Discretized = has_disc(object),
    ...
  )
  class(result) <- "OPM_Summary"
  result
}, sealed = SEALED)

setMethod("summary", OPMS, function(object, ...) {
  result <- lapply(X = object@plates, FUN = summary, ...)
  attr(result, "overall") <- list(Dimensions = dim(object),
    Aggregated = sum(has_aggr(object)), Discretized = sum(has_disc(object)),
    Plate.type = plate_type(object))
  class(result) <- "OPMS_Summary"
  result
}, sealed = SEALED)

setMethod("summary", MOPMX, function(object, ...) {
  select_parts <- function(x) if (is.null(y <- attr(x, "overall")))
      c(list(Length = 1L, Plate.type = x[["Plate type"]]),
        x[c("Aggregated", "Discretized")])
    else
      c(list(Length = y[["Dimensions"]][[1L]]),
        y[c("Plate.type", "Aggregated", "Discretized")])
  if (length(object)) {
    result <- lapply(lapply(X = object, FUN = summary, ...), select_parts)
    result <- do.call(rbind, lapply(result, as.data.frame))
    if (!is.null(n <- names(object)))
      rownames(result) <- make.names(n, TRUE)
  } else {
    result <- as.data.frame(matrix(NA, 0L, 4L, FALSE))
    colnames(result) <- c("Length", "Plate.type", "Aggregated", "Discretized")
  }
  class(result) <- c("MOPMX_Summary", oldClass(result))
  result
}, sealed = SEALED)

#= show summary

setMethod("show", OPMX, function(object) {
  print(summary(object))
}, sealed = SEALED)

setMethod("show", MOPMX, function(object) {
  print(summary(object))
}, sealed = SEALED)

setMethod("show", CMAT, function(object) {
  if (typeof(object) == "list") {
    object[] <- lapply(object, paste, collapse = "/")
    storage.mode(object) <- "character"
  }
  callNextMethod()
}, sealed = SEALED)


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
  cat(formatDL(x = x, ...), sep = "\n")
  invisible(x)
}

#' @rdname print
#' @method print OPMS_Listing
#' @export
#'
print.OPMS_Listing <- function(x, ...) {
  for (name in rownames(x)) {
    cat(name, gsub(".", "-", name, FALSE, TRUE), sep = "\n")
    cat(formatDL(x[name, ], ...), sep = "\n")
    cat("\n")
  }
  invisible(x)
}

#' @rdname print
#' @method print OPM_Summary
#' @export
#'
print.OPM_Summary <- function(x, ...) {
  lapply(formatDL(x = names(x), y = unlist(x), ...), FUN = cat, sep = "\n")
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
  cat(sprintf(tmpl, OPMS, y$Dimensions[1L], y$Aggregated, y$Discretized,
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
    " Access the elements with [[ or $ to apply certain methods.",
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
    } else
      (object - mean(object, na.rm = na.rm)) / sd(object, na.rm = na.rm)
  } else {
    if (extended) {
      min.object <- min(object, na.rm = na.rm)
      (object - min.object) / (max(object, na.rm = na.rm) - min.object)
    } else
      object / max(abs(object), na.rm = na.rm)
  }
  must(result * fac)
}, sealed = SEALED)

setMethod("ranging", MOA, function(object, ...) {
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
  ceiling(max(object) / by) * by + by # => error unless 'by' is numeric
}, sealed = SEALED)

setMethod("improved_max", OPMX, function(object, theor.max = TRUE, by = 10) {
  if (L(theor.max))
    return(THEOR_RANGE[2L])
  improved_max(max(object), by)
}, sealed = SEALED)


#= draw_ci ranging

#' @rdname ranging
#'
setGeneric("draw_ci", function(object, ...) standardGeneric("draw_ci"))

setMethod("draw_ci", "numeric", function(object, col = "blue", cex = 1,
    na.action = c("warn", "error", "ignore")) {
  LL(object, .wanted = 4L)
  if (any(is.na(c(left <- object[1L], right <- object[3L])))) {
    msg <- "cannot draw CI because left or right margin is 'NA'"
    case(match.arg(na.action),
      warn = warning(msg),
      error = stop(msg),
      ignore = NULL
    )
  }
  if (is.na(y <- object[4L]))
    stop("position on y axis must be provided")
  segments(left, y, right, y, lwd = cex, col = col)
  text(left, y, "(", col = col, cex = cex)
  text(right, y, ")", col = col, cex = cex)
  if (!is.na(point <- object[2L]))
    points(point, y, col = col, lwd = cex, pch = 19L, cex = cex)
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

setMethod("negative_control", OPMX, function(object, neg.ctrl) {
  if (!length(neg.ctrl) || is.numeric(neg.ctrl))
    neg.ctrl
  else if (is.character(neg.ctrl)) {
    result <- vapply(neg.ctrl, function(x)
      tryCatch(minmax(object, neg.ctrl), error = function(e) {
        warning("cannot get negative control from selected position ",
        "(deleted?); error was: ", conditionMessage(e))
        -1
      }), numeric(1L))
    result[result >= 0]
  } else if (is.logical(neg.ctrl)) {
    if (L(neg.ctrl))
      minmax(object)
    else
      NULL
  } else
    stop("object 'neg.ctrl' must be either empty or a 'character', 'logical' ",
      "or 'numeric' vector")
}, sealed = SEALED)

#= main_title negative_control

#' @rdname negative_control
#'
setGeneric("main_title", function(object, ...) standardGeneric("main_title"))

setMethod("main_title", OPMX, function(object, settings) {
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
  } else
    NULL
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
  tryCatch(select_colors(set), error = function(e) set)
}

#' @rdname try_select_colors
#'
default_color_regions <- function(colors, space, bias, n) {
  colorRampPalette(colors = unique(colors), space = space, bias = bias)(n)
}


################################################################################
################################################################################
#
# Main user-visible plotting functions
#


#' X-Y plot
#'
#' Customised plotting of a single \acronym{PM} plate or multiple plates, using
#' \code{xyplot} from the \pkg{lattice} package.
#'
#' @param x \code{\link{OPM}} or \code{\link{OPMS}} object.
#'
#' @param col For the \code{\link{OPM}} method, just a character scalar (colour
#'   name) determining the line colour.
#'
#'   For the \code{\link{OPMS}} method, either a character vector with colour
#'   codes or one of the arguments of \code{\link{select_colors}} (for picking
#'   one of the predefined colour sets).
#'
#'   It is an error if fewer colours are chosen than the number of plate
#'   grouping levels (see the \code{\dots} argument below). For user-chosen
#'   colour sets, keep in mind that the sets are not checked for duplicates, and
#'   see \code{max_rgb_contrast} from the \pkg{pkgutils} package as a method for
#'   optimally arranging user-defined colours.
#' @param lwd Numeric scalar determining the line width.
#'
#' @param neg.ctrl Determine the height of a horizontal baseline drawn in each
#'   panel. \itemize{
#'   \item If \code{NULL} or \code{FALSE}, no baseline will be drawn.
#'   \item If \code{TRUE}, the baseline's height is the value of
#'   \code{\link{minmax}}.
#'   \item If a character scalar, \code{neg.ctrl} is interpreted as the name of
#'   the wells regarded as negative control, and the baseline's height becomes
#'   the value of \code{\link{minmax}} applied to these wells only.
#'   \item Set \code{neg.ctrl} to a numeric value for assigning the height
#'   directly (at your own risk).
#'   }
#' @param base.col Character scalar. Baseline colour (ignored if no baseline is
#'   drawn).
#' @param base.lwd Numeric scalar determining the width of the baseline (ignored
#'   if no baseline is drawn).
#'
#' @param main The settings controlling the construction of the main title. If a
#'   list, a named list with the following entries (if missing, they are
#'   replaced by the respective defaults):
#'   \describe{
#'     \item{predef}{Character scalar or expression. Predefined title. If set,
#'       the other entries are ignored.}
#'     \item{use}{Logical scalar. If \code{FALSE}, returns \code{NULL}.}
#'     \item{...}{Other arguments are passed to \code{\link{plate_type}}.}
#'   }
#'   If \code{settings} is not a list but a character scalar or an expression,
#'   this is used as the \kbd{predef} entry of the above-mentioned list. If
#'   not a list but a logical scalar, it is used as the \sQuote{use} entry of
#'   this list. If not a list but a numeric value, it is used as the
#'   \sQuote{max} entry of this list.
#' @param xlab Character scalar. Title of x-axis. Use \code{NULL} to turn it
#'   off.
#' @param ylab Character scalar. Title of y-axis. Use \code{NULL} to turn it
#'   off.
#'
#' @param theor.max Logical scalar. Use the theoretical maximum as maximum of
#'   the y-axis? If \code{FALSE}, use the empirical maximum with a small offset.
#'
#' @param draw.grid Logical scalar. Insert background grid?
#'
#' @param space Character scalar indicating the position of the legend; either
#'   \sQuote{top}, \sQuote{bottom}, \sQuote{left} or \sQuote{right}. Might be
#'   overwritten by \code{legend.fmt}.
#'
#' @param strip.fmt List controlling the format of the description strip above
#'   each panel. For instance, the background colour is set using the
#'   \kbd{bg} key. For further details, see \code{strip.custom} from the
#'   \pkg{lattice} package. Note that the \strong{content} of these descriptions
#'   is determined by arguments passed from \code{xy_plot} to
#'   \code{\link{wells}}; see there for details.
#' @param striptext.fmt List controlling the textual description at the top of
#'   each panel. For instance, the relative text size is set using the \kbd{cex}
#'   key, the colour by \sQuote{col}, the font by \sQuote{font} and the number
#'   of lines by \sQuote{lines}. The latter might be of interest in conjunction
#'   with the \code{paren.sep} argument of \code{\link{wells}}. See the argument
#'   \code{par.strip.text} of \code{xyplot} from the \pkg{lattice} package for
#'   details.
#'
#' @param legend.fmt List controlling where and how to draw the legend. The
#'   content of the legend (mainly a description of the assignment of the
#'   colours to the curves) is determined automatically. See argument
#'   \sQuote{key} of \code{xyplot} from the \pkg{lattice} package for details.
#' @param legend.sep Character scalar. Relevant only if more than one columns of
#'   metadata have been selected; will then be used as separator to join their
#'   names in the legend.
#' @param draw.legend Logical scalar. If \code{FALSE}, no legend is drawn, and
#'   the two aforementioned arguments are ignored.
#'
#' @param ... Arguments that are passed to \code{\link{flatten}}. For the
#'   \code{\link{OPMS}} method, \code{include} is particularly important: the
#'   selected metadata are joined into a single factor, and the assignment of
#'   plates to this factor's levels determines the curve colour for each plate.
#'   That is, each combination of metadata entries as chosen using
#'   \code{include} yields one colour. If no metadata are selected (the
#'   default), each plate gets a colour of its own. Also note that arguments
#'   passed via \code{\link{flatten}} to \code{\link{wells}} can be given here
#'   which determine the content of the panel description.
#'
#' @param f Formula (for the data-frame method).
#' @param groups Character vector (for the data-frame method).
#'
#' @details The optimal number of rows and columns is estimated  from the number
#'   of selected wells. An optimal font size of the panel headers is also chosen
#'   automatically, but can also be adapted by the user, much like most aspects
#'   of the resulting graphics output.
#'
#'   In the case of the \code{\link{OPMS}} method, if metadata are selected,
#'   curve colours are determined according to the combinations of these
#'   metadata entries, otherwise each plate gets its own colour.
#'
#'   The data-frame method is not intended for phenotype microarray data. It is
#'   currently \strong{undocumented} and potentially subject to frequent changes
#'   or even removal. Users interested in the method should contact the authors.
#'
#' @export
#' @family plotting-functions
#' @return An object of class \sQuote{trellis}. See \code{xyplot} from the
#'   \pkg{lattice} package for details.
#' @references Sarkar, D. 2008 \emph{Lattice: Multivariate Data Visualization
#'   with R.} New York: Springer, 265 p.
#' @references Vaas, L. A. I., Sikorski, J., Michael, V., Goeker, M., Klenk
#'   H.-P. 2012 Visualization and curve parameter estimation strategies for
#'   efficient exploration of Phenotype Microarray kinetics. \emph{PLoS ONE}
#'   \strong{7}, e34846.
#' @keywords hplot
#' @seealso lattice::xyplot
#' @examples
#'
#' # OPM method
#' \dontrun{
#' xy_plot(vaas_1) # note the default main title built from the plate type
#' }
#'
#' x <- vaas_1[, 11:22]
#' # Yields a warning message: we have deleted the default negative control.
#' xy_plot(x)
#' # Turn the baseline off => no warning:
#' xy_plot(x, neg.ctrl = NULL)
#' # Or guess a baseline:
#' xy_plot(x, neg.ctrl = 100)
#' # Some like it ugly:
#' xy_plot(x, neg.ctrl = 100, col = "pink", base.col = "yellow", main = "Ugly")
#'
#' # OPMS method
#' \dontrun{
#' # Colour by species and strain; note default main title
#' xy_plot(vaas_4, include = c("Species", "Strain"))
#' # Use the largest of the negative-control maxima as baseline
#' xy_plot(vaas_4, include = c("Species", "Strain"),
#'   neg.ctrl = max(vaas_4, "A01"))
#' }
#'
setGeneric("xy_plot", function(x, ...) standardGeneric("xy_plot"))

setMethod("xy_plot", OPM, function(x, col = "midnightblue", lwd = 1,
    neg.ctrl = "A01", base.col = "grey10", base.lwd = lwd,
    main = list(), xlab = "Time [h]", ylab = "Value [OmniLog units]",
    theor.max = TRUE, draw.grid = TRUE,
    strip.fmt = list(), striptext.fmt = list(),
    ...) {

  ## BEGIN must be synchronized with xy_plot,OPMS

  # Setup
  layout <- best_layout(dim(x)[2L])
  y.max <- improved_max(x, theor.max)
  main <- main_title(x, main)
  neg.ctrl <- negative_control(x, neg.ctrl)

  # Adding default to settings lists. insert() is used here: for some reason
  # the later entries have precedence in striptext.fmt
  strip.fmt <- insert(as.list(strip.fmt), bg = "grey90")
  striptext.fmt <- insert(as.list(striptext.fmt), cex = 1.5 / sqrt(layout[2L]),
    lines = 1.25)

  ## END must be synchronized with xy_plot,OPMS

  # Plot
  xyplot(
    # Principally unchangeable arguments
    create_formula("`%s` ~ `%s` | `%s`",
      RESERVED_NAMES[c("value", "time", "well")]),
    data = flatten(x, ...), type = "l", layout = layout,
    as.table = TRUE,
    # Curve colour and panel height
    col = col, ylim = c(0, y.max),
    # Axis annotation
    scales = list(x = list(rot = 90)),
    # Main annotation
    main = main, xlab = xlab, ylab = ylab,
    # Description above each panel
    strip = do.call(strip.custom, strip.fmt), par.strip.text = striptext.fmt,
    # The panels
    panel = function(...) {
      if (draw.grid)
        panel.grid(h = -1, v = -1)
      if (length(neg.ctrl))
        panel.abline(neg.ctrl, 0, col = base.col, lwd = base.lwd)
      panel.xyplot(..., lwd = lwd)
    })

}, sealed = SEALED)

setMethod("xy_plot", OPMS, function(x, col = opm_opt("colors"), lwd = 1,
    neg.ctrl = "A01", base.col = "black", base.lwd = lwd,
    main = list(), xlab = "Time [h]", ylab = "Value [OmniLog units]",
    theor.max = TRUE, draw.grid = TRUE, space = "top",
    strip.fmt = list(), striptext.fmt = list(),
    legend.fmt = list(), legend.sep = " ", draw.legend = TRUE,
    ...) {

  ## BEGIN must be synchronized with xy_plot,OPM

  # Setup
  layout <- best_layout(dim(x)[3L])
  y.max <- improved_max(x, theor.max)
  main <- main_title(x, main)
  neg.ctrl <- negative_control(x, neg.ctrl)

  # Adding default to settings lists. insert() is used here: for some reason
  # the later entries have precedence in striptext.fmt
  strip.fmt <- insert(as.list(strip.fmt), bg = "grey90")
  striptext.fmt <- insert(as.list(striptext.fmt), cex = 1.5 / sqrt(layout[2L]),
    lines = 1.25)

  ## END must be synchronized with xy_plot,OPM

  # OPMS-specific addition of defaults
  legend.fmt <- insert(as.list(legend.fmt), space = space, .force = FALSE)

  # Selection of a colour set
  col <- try_select_colors(col)

  # Conversion
  data <- flatten(x, ...)

  # Assignment of colours to plates
  param <- flattened_to_factor(object = data, sep = legend.sep)
  key.text <- levels(param)
  if (length(col) < length(key.text))
    stop("colour should be by plate or metadata, but there are too few colours")
  key.col <- col[seq_along(key.text)]
  col <- col[param]

  names(data)[match(RESERVED_NAMES[["plate"]], names(data))] <- "_GROUPING"

  # Plot
  xyplot(
    # Principally unchangeable arguments
    create_formula("`%s` ~ `%s` | `%s`",
      RESERVED_NAMES[c("value", "time", "well")]),
    data = data, type = "l", layout = layout,
    as.table = TRUE, groups = `_GROUPING`,
    # Curve colours and panel height
    col = col, ylim = c(0, y.max),
    # Axis annotation
    scales = list(x = list(rot = 90)),
    # Description above each panel
    strip = do.call(strip.custom, strip.fmt), par.strip.text = striptext.fmt,
    # Main annotation
    main = main, ylab = ylab, xlab = xlab,
    # Legend
    key = if (draw.legend)
      c(list(col = key.col, text = list(key.text)), legend.fmt)
    else
      NULL,
    panel = function(...) {
      if (draw.grid)
        panel.grid(h = -1, v = -1)
      if (length(neg.ctrl))
        panel.abline(neg.ctrl, 0, col = base.col, lwd = base.lwd)
      panel.xyplot(..., lwd = lwd)
    }
  )

}, sealed = SEALED)

setMethod("xy_plot", "data.frame", function(x, f, groups,
    col = opm_opt("colors"), lwd = 1, neg.ctrl = NULL, base.col = "black",
    base.lwd = lwd, main = groups, xlab = elem(f, 3L:2L), ylab = elem(f, 2L),
    draw.grid = TRUE, space = "top", strip.fmt = list(), striptext.fmt = list(),
    legend.fmt = list(), legend.sep = " ", draw.legend = TRUE, ...) {

  elem <- function(x, i) {
    pos <- 1L
    while (length(x) > 1L) {
      x <- x[[i[[pos]]]]
      if (pos < length(i))
        pos <- pos + 1L
    }
    as.character(x)
  }

  f <- as.formula(f)
  groups <- as.character(groups)

  # Layout
  xvar <- as.factor(x[, elem(f, 3L:3L)])
  layout <- best_layout(length(levels(xvar)))

  # Put grouping variable together
  pos <- match(groups, names(x))
  if (any(isna <- is.na(pos)))
    stop(sprintf("could not find '%s' in 'x'", groups[isna][1L]))
  x$`_GROUPING` <- do.call(paste, c(x[, pos, drop = FALSE], sep = legend.sep))
  x$`_GROUPING` <- as.factor(x$`_GROUPING`)

  # Assignment of colours
  col <- try_select_colors(col)
  key.text <- levels(x$`_GROUPING`)
  if (length(key.text) > length(col))
    stop("number of colours must be at least as large as number of groups")
  key.col <- col[seq_along(key.text)]
  col <- key.col

  # Adding default to settings lists. insert() is used here: for some reason
  # the later entries have precedence in striptext.fmt
  strip.fmt <- insert(as.list(strip.fmt), bg = "grey90")
  striptext.fmt <- insert(as.list(striptext.fmt), cex = 1.5 / sqrt(layout[2L]),
    lines = 1.25)

  legend.fmt <- insert(as.list(legend.fmt), space = space, .force = FALSE)

  xyplot(
    # Principally unchangeable arguments
    x = f, data = x, type = "l", layout = layout,
    as.table = TRUE, groups = `_GROUPING`,
    # Curve colours (panel height is omitted)
    col = col,
    # Axis annotation
    scales = list(x = list(rot = 90)),
    # Description above each panel
    strip = do.call(strip.custom, strip.fmt), par.strip.text = striptext.fmt,
    # Main annotation
    main = main, ylab = ylab, xlab = xlab,
    # Legend
    key = if (draw.legend)
      c(list(col = key.col, text = list(key.text)), legend.fmt)
    else
      NULL,
    panel = function(...) {
      if (draw.grid)
        panel.grid(h = -1, v = -1)
      if (length(neg.ctrl))
        panel.abline(neg.ctrl, 0, col = base.col, lwd = base.lwd)
      panel.xyplot(..., lwd = lwd)
    },
    ...
  )
}, sealed = SEALED)


################################################################################


#' Level plot
#'
#' Level plot for \code{\link{OPM}} and \code{\link{OPMS}} objects using the
#' function from the \pkg{lattice} package.
#'
#' @param x \code{\link{OPM}} or \code{\link{OPMS}}  object.
#'
#' @param main The settings controlling the construction of the main title.
#'   Works like the \code{main} argument of \code{\link{xy_plot}}.
#' @param colors Character vector indicating the colours (at least two).
#'
#' @param panel.headers \code{NULL}, logical scalar, expression or character
#'   vector. \code{NULL} and \code{FALSE} turn panel headers off. \code{TRUE}
#'   causes the panel headers to be constructed from the plate numbers or those
#'   metadata that were included by \code{\link{flatten}} (see there). Character
#'   vectors and expressions are directly used for the text within these panel
#'   headers. Currently ignored by the \code{\link{OPM}} method.
#' @param cex Numeric scalar. Magnification of axis annotation. If \code{NULL},
#'   automatically adapted to the number of wells (at least a good guess is
#'   made).
#'
#' @param strip.fmt List controlling the format of the description strip above
#'   each panel. For instance, the background colour is set using the \kbd{bg}
#'   key. For further details, see \code{strip.custom} from the \pkg{lattice}
#'   package. \code{strip.fmt} is ignored if panel.headers is \code{FALSE} and
#'   currently always ignored by the \code{\link{OPM}} method.
#' @param striptext.fmt List controlling the format of the text within the strip
#'   above each panel. See \code{\link{xy_plot}} for details, which has an
#'   argument of the same name.
#' @param legend.sep Character scalar. This works like the eponymous argument to
#'   \code{\link{flatten}} (see there); it is ignored unless metadata are chosen
#'   for constructing the panel headers.
#'
#' @param space Character scalar passed to \code{colorRampPalette} from the
#'   \pkg{grDevices} package. These and the following arguments are for
#'   fine-tuning the colour palette used for plotting.
#' @param bias Numeric scalar also passed to \code{colorRampPalette}.
#' @param num.colors Numeric scalar passed to the function returned by
#'   \code{colorRampPalette}.
#'
#' @param ... Arguments that are passed to \code{\link{flatten}}.
#'
#' @export
#' @return An object of class \sQuote{trellis}. See \code{levelplot} from the
#'   \pkg{lattice} package for details.
#' @family plotting-functions
#' @keywords hplot
#'
#' @references Jacobsen, J. S., Joyner, D. C., Borglin, S. E., Hazen, T. C.,
#'   Arkin, A. P. et al. 2007 Visualization of growth curve data from phenotype
#'   microarray experiments. \emph{11th International Conference on Information
#'   Visualization (IV07).} Zuerich, Switzerland, July 4-6 2007. Published by
#'   the IEEE Computer Society.
#' @references Sarkar, D. 2008 \emph{Lattice: Multivariate Data Visualization
#'   with R.} New York: Springer, 265 p.
#' @references Vaas, L. A. I., Sikorski, J., Michael, V., Goeker, M., Klenk
#'   H.-P. 2012 Visualization and curve parameter estimation strategies for
#'   efficient exploration of Phenotype Microarray kinetics. \emph{PLoS ONE}
#'   \strong{7}, e34846.
#'
#' @seealso lattice::levelplot grDevices::colorRampPalette
#' @examples
#'
#' # OPM method
#' level_plot(vaas_1, main = "Levelplot example")
#'
#' # OPMS method
#' \dontrun{
#' # headers include species and strain
#' level_plot(vaas_4, include = c("Species", "Strain"))
#' }
#'
setGeneric("level_plot", function(x, ...) standardGeneric("level_plot"))

setMethod("level_plot", OPM, function(x, main = list(),
    colors = opm_opt("color.borders"), panel.headers = FALSE, cex = NULL,
    strip.fmt = list(), striptext.fmt = list(), legend.sep = " ",
    space = "Lab", bias = 0.5, num.colors = 200L, ...) {
  if (is.null(cex))
    cex <- guess_cex(dim(x)[2L])
  main <- main_title(x, main)
  levelplot(create_formula("`%s` ~ `%s` * `%s`",
      RESERVED_NAMES[c("value", "time", "well")]),
    data = flatten(x, ...), main = main,
    col.regions = default_color_regions(colors, space, bias, num.colors),
    scales = list(cex = cex, lineheight = 10))
}, sealed = SEALED)

setMethod("level_plot", OPMS, function(x, main = list(),
    colors = opm_opt("color.borders"), panel.headers = TRUE, cex = NULL,
    strip.fmt = list(), striptext.fmt = list(), legend.sep = " ",
    space = "Lab", bias = 0.5, num.colors = 200L, ...) {
  dims <- dim(x)
  if (is.null(cex))
    cex <- guess_cex(dims[3L])
  data <- flatten(x, ...)
  if (is.null(panel.headers) || (is.logical(panel.headers) && !panel.headers))
    strip.fmt <- FALSE
  else {
    if (is.logical(panel.headers))
      panel.headers <- flattened_to_factor(object = data, sep = legend.sep)
    if (!is.expression(panel.headers))
      panel.headers <- as.character(panel.headers)
    strip.fmt <- insert(as.list(strip.fmt), bg = "grey90",
      factor.levels = panel.headers)
    strip.fmt <- do.call(strip.custom, strip.fmt)
  }
  levelplot(create_formula("`%s` ~ `%s` * `%s` | `%s`",
      RESERVED_NAMES[c("value", "time", "well", "plate")]),
    data = data, main = main_title(x, main),
    col.regions = default_color_regions(colors, space, bias, num.colors),
    strip = strip.fmt, as.table = TRUE, layout = c(dims[1L], 1L),
    par.strip.text = as.list(striptext.fmt),
    scales = list(cex = cex, lineheight = 10))
}, sealed = SEALED)


################################################################################


#' Plot point estimates with \acronym{CI}s
#'
#' Draw point estimates with their confidence intervals.  Used for comparing
#' aggregated values together with their confidence intervals between plates.
#' This method can in most cases \strong{not} be applied to entire plates but to
#' selected wells only.
#'
#' @param object \code{\link{OPMS}} object or (rarely) a data frame. If an
#'   \code{\link{OPMS}} object, it is in most cases necessary to restrict the
#'   plates to at most about one dozen wells. See \code{\link{[}} for how to
#'   achieve this.
#'
#'   The data frame method is not normally directly called by an \pkg{opm} user
#'   but via the \code{\link{OPMS}} method, unless it is used after
#'   \code{\link{extract}} was applied to a data frame for calculating point
#'   estimates and confidence intervals from groups of observations. See there
#'   for details.
#'
#'   Otherwise, the data frame should be as exported by the \code{\link{OPMS}}
#'   method of \code{\link{extract}} with \code{ci} set to \code{TRUE}. There
#'   must be a column named \code{\link{param_names}("split.at")} followed by
#'   columns with only numeric values. Columns before that split column, if any,
#'   are used for grouping. The rows must entirely comprise triplets
#'   representing (i) the point estimate, (ii) the lower and (iii) the upper
#'   confidence interval.
#'
#' @param as.labels List. Metadata to be joined and used to draw a legend.
#'   Ignored if \code{NULL}.
#' @param subset Character scalar. The parameter to plot. Only a single one can
#'   be selected. See \code{\link{param_names}} for the options.
#'
#' @param rowname.sep Character scalar. Used when joining explanatory columns
#'   into row labels of the plots.
#'
#' @param prop.offset Numeric scalar. A proportional offset that is added to the
#'   vertical range of the panels (after determining the maximum range among all
#'   panels to ensure consistency within the plot).
#' @param align Character scalar. How to apply the offset; in addition to the
#'   default, \sQuote{left} and \sQuote{right} are possible.
#'
#' @param col Character scalar. Colour to be used.
#'
#' @param na.action Character scalar. What to do if a confidence interval
#'   contains \code{NA} values; one of \sQuote{ignore}, \sQuote{warn} and
#'   \sQuote{error}.
#'
#' @param draw.legend Logical scalar. Ignored if there are no explanatory
#'   columns.
#' @param legend.field Two-element numeric vector. Indicates the panel in which
#'   the legend is drawn. Subsequent arguments work then relative to this panel.
#'   If \code{legend.field} has less then two fields, the number of panels is
#'   set to 1 (the entire plot), and the legend is drawn relative to that.
#' @param x Legend position, passed to \code{legend} from the \pkg{graphics}
#'   package. Ignored unless \code{draw.legend} is \code{TRUE}.
#' @param xpd Logical scalar. Also passed to that function.
#' @param vline Numeric scalar with the position on the y-axis of a vertical
#'   line to be drawn. Ignored if \code{NULL}.
#' @param crr Numeric scalar (column-row-ratio) interpreted as number of columns
#'   per number of rows. Determines the arrangement of the subplots.
#' @param ... Optional arguments passed to \code{legend}, or arguments
#'   passed from the \code{\link{OPMS}} method to the data frame method.
#'
#' @param split.at Character vector. See \code{\link{extract}}.
#'
#' @details The default placement of the legend is currently not necessarily
#'   very useful. When plotting entire \acronym{PM} plates, the \sQuote{mar}
#'   parameter of \code{par} most likely would need to be set to a lower value,
#'   but it is recommended to plot only subsets of plates, i.e. selected wells.
#'
#' @references Vaas LAI, Sikorski J, Michael V, Goeker M, Klenk H-P. 2012
#'   Visualization and curve parameter estimation strategies for efficient
#'   exploration of Phenotype Microarray kinetics. \emph{PLoS ONE} \strong{7},
#'   e34846.
#'
#' @return Character vector describing the plot's legend, returned invisibly.
#' @export
#' @family plotting-functions
#' @seealso graphics::plot
#' @keywords hplot
#' @examples
#' x <- ci_plot(vaas_4[, , 1:3], as.labels = list("Species", "Strain"),
#'   subset = "A", x = "bottomright", legend.field = NULL)
#' # note that the values on the y axes are drawn to scale
#' x
#' stopifnot(is.character(x), identical(length(x), 4L))
#' # ... and that the return value contains the legend (even if it is not drawn)
#'
#' ## See also the examples for the data-frame method of extract().
#'
setGeneric("ci_plot", function(object, ...) standardGeneric("ci_plot"))

setMethod("ci_plot", "data.frame", function(object, rowname.sep = " ",
    prop.offset = 0.04, align = "center", col = "blue", na.action = "warn",
    draw.legend = TRUE, legend.field = c(1, 1), x = "topleft", xpd = TRUE,
    vline = 0, split.at = param_names("split.at"), crr = 0.75, ...) {

  single_plot <- function(col.pos) {
    plot(x = NULL, y = NULL, xlim = ranges[, col.pos], ylim = ylim,
      main = colnames(object)[col.pos], yaxt = "n", xlab = "", ylab = "")
    if (length(vline))
      abline(v = vline, lty = 2L, col = "gray60")
    axis(2L, at = chunk.pos, labels = row.names)
    vapply(chunk.pos, FUN = function(pos) {
      pe <- object[pos, col.pos]
      left <- object[pos + 1L, col.pos]
      right <- object[pos + 2L, col.pos]
      draw_ci(c(left, pe, right, pos), col = col, na.action = na.action)
    }, numeric(4L))
  }

  # Determine the position used for splitting the data frame
  param.pos <- assert_splittable_matrix(object, split.at)

  # Check the triplet structure and determine all triplet start positions
  if (nrow(object) %% 3L != 0L)
    stop("need data frame with 3 * n rows")
  chunk.pos <- seq_len(nrow(object))
  chunk.pos <- chunk.pos[chunk.pos %% 3L == 1L]
  row.names <- as.character(seq_along(chunk.pos))

  # Reorder the matrix and construct the legend if necessary
  if (param.pos > 1L) {
    factor.pos <- seq.int(1L, param.pos - 1L)
    ordering <- do.call(order, as.list(object[, factor.pos, drop = FALSE]))
    object <- object[ordering, , drop = FALSE]
    legend <- as.matrix(object[chunk.pos, factor.pos, drop = FALSE])
    legend <- apply(legend, 1L, paste, collapse = rowname.sep)
    legend <- paste(row.names, legend, sep = ": ")
  } else
    legend <- NULL

  # Reduce to the numeric part of matrix
  object <- as.matrix(object[, seq.int(param.pos + 1L, ncol(object)),
    drop = FALSE])

  # Determine field range (which is set to be uniform)
  ranges <- apply(object, 2L, range, na.rm = TRUE)
  max.range <- max(apply(ranges, 2L, FUN = function(x) x[2L] - x[1L]))
  ranges <- apply(ranges, 2L, FUN = best_range, target = max.range,
    align = align, prop.offset = prop.offset)
  ylim <- best_range(chunk.pos, target = NULL, prop.offset = prop.offset)

  # Panel layout and plotting of individual panels
  old.par <- par(mfcol = best_layout(ncol(object), crr))
  on.exit(par(old.par))
  lapply(seq_len(ncol(object)), FUN = single_plot)

  # Legend
  if (draw.legend && !is.null(legend)) {
    if (length(legend.field) > 1L)
      par(mfg = legend.field[1L:2L])
    else
      par(mfcol = c(1L, 1L))
    legend(x = x, legend = legend, xpd = xpd, ...)
  }
  invisible(legend)

}, sealed = SEALED)

setMethod("ci_plot", OPMS, function(object, as.labels,
    subset = opm_opt("curve.param"), ...) {
  ci_plot(extract(object, as.labels = as.labels, subset = subset,
    dataframe = TRUE, ci = TRUE), split.at = param_names("split.at"), ...)
}, sealed = SEALED)


################################################################################


#' Heat map
#'
#' A wrapper for \code{heatmap} from the \pkg{stats} package and
#' \code{heatmap.2} from the \pkg{gplots} package with some adaptations likely
#' to be useful for OmniLog\eqn{\textsuperscript{\textregistered}}{(R)} data.
#' The data-frame and \code{\link{OPMS}} methods extract a numeric matrix from a
#' given data frame or \code{\link{OPMS}} object and pass the result to the
#' matrix method.
#'
#' @param object Matrix, data frame or \code{\link{OPMS}} object. The matrix
#'   method is mainly designed for curve-parameter matrices as created by
#'   \code{\link{extract}} but can be used with any numeric matrix. If a data
#'   frame, it must contain at least one column with numeric data.
#'
#' @param as.labels Character, numeric or logical vector indicating the
#'   positions of the columns to be joined and used as row labels. If
#'   \code{NULL} or empty, the row names of \code{object} are used. See
#'   \code{\link{extract}} for details.
#'
#' @param as.groups Character, numeric or logical vector indicating the
#'   positions of the columns to be joined and used as group indicators. If
#'   \code{NULL} or empty, groups are ignored.
#'
#' @param sep Character scalar determining how to join row and group names. See
#'   \code{\link{extract}} for details.
#'
#' @param subset Character scalar passed to the \code{\link{OPMS}} method of
#'   \code{\link{extract}}.
#' @param extract.args Optional list of arguments passed to that method.
#'
#' @param hclustfun Determines the clustering method used. If a function, used
#'   directly. If a character scalar, used as the \sQuote{method} argument of
#'   \code{hclust}. If a list, passed as argument list to \code{hclust}.
#' @param distfun Determines the distance method used. If a function, used
#'   directly. If a character scalar, used as the \sQuote{method} argument of
#'   \code{dist}. If a list, passed as argument list to \code{dist}.
#'
#' @param scale Character scalar. See \code{heatmap} for details. The default
#'   was changed to no rescaling because the curve parameters estimated from
#'   OmniLog\eqn{\textsuperscript{\textregistered}}{(R)} data have the same
#'   scale. If the relative changes per substrate are of interest,
#'   \sQuote{column} should be used.
#'
#' @param r.groups Determines the plotting of a colour bar indicating row
#'   groups.
#'
#'   If \code{NULL}, ignored. If a function, applied to the row names of
#'   \code{object}; should then yield one group name for each row name. If a
#'   character scalar, the name of an attribute of \code{object} that contains
#'   the row group affiliations (ignored if this is not found). Otherwise,
#'   coerced to \sQuote{character} mode.
#'
#'   Finally the groups are converted to a factor and used for selecting from
#'   \code{r.col}.
#' @param r.col Character vector of colour names used by \code{r.groups}.
#'   Ignored if that is \code{NULL}.
#' @param c.groups Determines the plotting of a colour bar indicating column
#'   groups.
#'
#'   If \code{NULL}, ignored. If a function, applied to the column names of
#'   \code{object}; should then yield one group name for each column name. If a
#'   character scalar, the name of an attribute of \code{object} that contains
#'   the column group affiliations (ignored if this is not found). Otherwise,
#'   coerced to \sQuote{character} mode.
#'
#'   Finally the groups are converted to a factor and used for selecting from
#'   \code{c.col}.
#' @param c.col Character vector of colour names used by \code{c.groups}.
#'   Ignored if that is \code{NULL}.
#'
#' @param magnif Numeric vector. Factor(s) used per default by \code{cexRow} and
#'   \code{cexCol}.
#' @param cexRow Magnification of the row labels.
#' @param cexCol Magnification of the column labels.
#'
#' @param borders Numeric vector. Factor(s) used per default by \code{margin}
#'   and \code{cexCol}.
#' @param margins Two-element numeric vector determining the relative size of
#'   the margin (i) at the bottom and (ii) at the left.
#'
#' @param col Character vector containing the proper heat map colours.
#' @param asqr Logical scalar indicating whether the data should be treated
#'   with the arcsine-square root transformation. This usually only makes
#'   sense for proportion data. If \code{NA}, percentages are assumed.
#' @param lmap Numeric scalar with at least three elements, or empty. If empty,
#'   ignored. Otherwise used for mapping logical values to numeric values. See
#'   \code{\link{map_values}} for details. Ignored if the data are not logical.
#'
#' @param ... Optional arguments passed to \code{heatmap} or \code{heatmap.2}.
#'   Note that some defaults of \code{heatmap.2} are overwritten even though
#'   this is not transparent from the argument list of \code{heat_map}. If set
#'   explicitly, the default \code{heatmap.2} behaviour is restored.
#'   \code{\dots} also represents all arguments passed from the
#'   \code{\link{OPMS}} or data-frame methods to the matrix method.
#'
#' @param use.fun Character scalar. If \kbd{gplots}, it is attempted to load
#'   the \pkg{gplots} package and use its \code{heatmap.2} function (the
#'   default). If this fails, a warning is issued, and \code{heatmap} from the
#'   \pkg{stats} package (the default) is called instead.
#'
#' @export
#' @return A list as output by \code{heatmap} or \code{heatmap.2} with the
#'   additional entries \code{rowColMap} or \code{colColMap} giving the
#'   mapping(s) of group names to colours as named character vector(s), if this
#'   feature was used.
#'
#' @family plotting-functions
#' @seealso stats::heatmap gplots::heatmap.2
#' @keywords hplot
#'
#' @examples
#'
#' # Matrix method (usually unnecessary, see below)
#' x <- extract(vaas_4, as.labels = list("Strain"),
#'   as.groups = list("Species"))
#' hm <- heat_map(x)
#' stopifnot(identical(metadata(vaas_4, "Species"), names(hm$rowColMap)))
#'
#' # 'OPMS' method (more convenient)
#' hm.2 <- heat_map(vaas_4, as.labels = "Strain", as.groups = "Species")
#' stopifnot(identical(hm[-3], hm.2[-3]))
#'
#' # Data-frame method
#' x <- extract(vaas_4, as.labels = list("Species", "Strain"), dataframe = TRUE)
#' hm <- heat_map(x, as.labels = "Strain", as.groups = "Species")
#' stopifnot(identical(metadata(vaas_4, "Species"), names(hm$rowColMap)))
#'
setGeneric("heat_map", function(object, ...) standardGeneric("heat_map"))

setMethod("heat_map", "matrix", function(object,
    hclustfun = "ward", distfun = "euclidean", scale = "none",
    r.groups = "row.groups", r.col = opm_opt("colors"),
    c.groups = "col.groups", c.col = opm_opt("colors"),
    magnif = 4, cexRow = magnif[1L] / sqrt(nrow(object)),
    cexCol = magnif[length(magnif)] / sqrt(ncol(object)),
    borders = c(0.55, 0.75),
    margins = if (use.fun[1L] == "gplots")
      c(borders[1L] * cexCol * max(nchar(colnames(object))),
      borders[length(borders)] * cexRow * max(nchar(rownames(object))))
    else
      c(5, 5),
    col = opm_opt("heatmap.colors"), asqr = FALSE, lmap = 1L:3L,
    ...,
    use.fun = c("gplots", "stats")) {

  get_fun <- function(infun, usefun) {
    if (is.character(infun))
      function(x) usefun(x, method = infun)
    else if (is.list(infun))
      function(x) do.call(usefun, c(list(x), infun))
    else
      infun
  }

  get_side_colors <- function(groups, colors, for.rows) {
    if (is.null(groups))
      return(NULL)
    if (is.function(groups)) {
      groups <- if (for.rows)
        groups(rownames(object))
      else
        groups(colnames(object))
    } else if (is.character(groups)) {
      if (length(groups) == 1L) {
        groups <- attr(object, groups)
        if (is.null(groups))
          return(NULL)
      }
    } else
      groups <- as.character(groups)
    colors <- try_select_colors(colors)
    groups <- as.factor(groups)
    if (length(colors) < length(levels(groups)))
      stop("more groups than colours given")
    structure(colors[groups], names = as.character(groups))
  }

  do_asqr <- function(x, percent) {
    if (percent) {
      if (any(x < 0, na.rm = TRUE) || any(x > 100, na.rm = TRUE))
        warning("in 'percent' mode, 'x' should be between 0 and 100")
      else if (all(x <= 1, na.rm = TRUE))
        warning("percentages expected, but everything < 1")
      return(100 * asin(sqrt(x / 100)))
    }
    asin(sqrt(x))
  }

  clustfun <- get_fun(hclustfun, hclust)
  dfun <- get_fun(distfun, dist)
  arg.list <- list(scale = scale, cexRow = cexRow, cexCol = cexCol,
    hclustfun = clustfun, distfun = dfun, margins = margins, col = col, ...)

  row.side.colors <- get_side_colors(r.groups, r.col, for.rows = TRUE)
  if (!is.null(row.side.colors))
    arg.list$RowSideColors <- row.side.colors
  col.side.colors <- get_side_colors(c.groups, c.col, for.rows = FALSE)
  if (!is.null(col.side.colors))
    arg.list$ColSideColors <- col.side.colors

  case(match.arg(use.fun),
    gplots = {
      if (suppressMessages(suppressWarnings(require(gplots, quietly = TRUE,
          warn.conflicts = FALSE)))) {
        arg.list <- insert(arg.list, trace = "none", .force = FALSE)
        heatmap_fun <- gplots::heatmap.2
      } else {
        warning("package 'gplots' requested, but not available")
        heatmap_fun <- heatmap
      }
    },
    stats = heatmap_fun <- heatmap
  )

  if (typeof(object) == "logical")
    if (length(lmap))
      object[] <- map_values(c(object), lmap)
    else
      storage.mode(object) <- "integer"

  if (is.na(L(asqr)) || asqr)
    object[] <- do_asqr(object, is.na(asqr))

  result <- do.call(heatmap_fun, c(list(x = object), arg.list))
  result$colColMap <- col.side.colors
  result$rowColMap <- row.side.colors
  invisible(result)

}, sealed = SEALED)

setMethod("heat_map", "data.frame", function(object, as.labels,
    as.groups = NULL, sep = " ", ...) {
  invisible(heat_map(extract_columns(object, what = "numeric", direct = FALSE,
    as.labels = as.labels, as.groups = as.groups, sep = sep), ...))
}, sealed = SEALED)

setMethod("heat_map", OPMS, function(object, as.labels,
    subset = opm_opt("curve.param"), as.groups = NULL, sep = " ",
    extract.args = list(), ...) {
  extract.args <- insert(as.list(extract.args), list(object = object,
    as.labels = as.labels, as.groups = as.groups, subset = subset,
    dataframe = FALSE, ci = FALSE, sep = sep), .force = TRUE)
  invisible(heat_map(do.call(extract, extract.args), ...))
}, sealed = SEALED)


################################################################################


#' Radial plot
#'
#' A wrapper for \code{radial.plot} from the \pkg{plotrix} package with some
#' adaptations likely to be useful for
#' OmniLog\eqn{\textsuperscript{\textregistered}}{(R)} data.
#'
#' @param object \code{\link{OPMS}} object (with aggregated values) to be
#'   plotted. Alternatively a data frame or a numeric matrix, but the methods
#'   for these objects are rarely needed.
#' @param as.labels For the \code{\link{OPMS}} method, a metadata selection
#'   defining the data to be joined and used as row names, ultimately yielding
#'   the legend. If \code{NULL} or empty, ignored. See \code{\link{extract}} for
#'   details. For the data-frame method, a selection of columns used in the same
#'   way.
#' @param subset For the \code{\link{OPMS}} method, a character scalar passed to
#'   the \code{\link{OPMS}} method of \code{\link{extract}}. The data-frame
#'   method uses this to select the columns; currently only the default makes
#'   sense. For the matrix method, a selection of columns to be plotted.
#' @param sep Character scalar determining how to join row names. See
#'   \code{\link{extract}} for details.
#' @param extract.args Optional list of arguments passed to the
#'   \code{\link{OPMS}} method of \code{\link{extract}}. Currently ignored by
#'   the other methods.
#'
#' @param rp.type Character scalar. Among the possible values \sQuote{p} for
#'   polygons, \sQuote{s} for symbols and \sQuote{r} for radial lines, the
#'   latter is not recommended. These and the following arguments are passed to
#'   \code{plotrix::radial.plot}. See there for details.
#' @param radlab Logical scalar. Rotation of the outer labels to a radial
#'   orientation might safe some space in the graphic.
#' @param show.centroid Logical scalar.
#' @param show.grid.labels Logical scalar. Indicates whether and where display
#'   labels for the grid are shown.
#' @param lwd Numeric scalar for the line width.
#' @param mar Numeric vector of length 4 determining the margins of the plot.
#' @param line.col Character or numeric vector for determining the line colour.
#' @param point.symbols Like the following arguments, passed to
#'   \code{radial.plot} from the \pkg{plotrix} package. See there for details.
#'   Explicitly provided here to silence some \code{radial.plot} warnings
#'   occurring as of \R 3.0.0.
#' @param point.col Indicates the colour(s) of the symbols.
#' @param poly.col Indicates the colour for filling the drawn polygons, if any.
#'   Use \code{NA} for no fill (recommended).
#' @param main The main title of the plot.
#' @param ... Optional other arguments passed to \code{radial.plot} from the
#'   \pkg{plotrix} package.
#' @param draw.legend Logical scalar. Whether to draw a legend. Ignored unless
#'   \code{object} has row names (because these are used to generate the
#'   description).
#' @param x Legend position, passed to \code{legend} from the \pkg{graphics}
#'   package. Ignored unless \code{draw.legend} is \code{TRUE}.
#' @param y Optional Second legend coordinate. Also passed to that function.
#' @param xpd Logical scalar. Also passed to that function.
#' @param pch Integer scalar. Also passed to that function.
#' @param legend.args List of optional other arguments passed to that function.
#'
#' @export
#' @family plotting-functions
#' @seealso plotrix::radial.plot graphics::legend
#' @keywords hplot
#'
#' @return A vector with the row names of \code{object} as names and the
#'   corresponding colours as values, equivalent to the legend; \code{NULL} if
#'   no row names are present.
#'
#' @details The default positioning of the legend is not necessarily very
#'   useful, but suitable combinations of \code{margin}, \code{x} and \code{y}
#'   can be found for given data sizes. Plotting entire plates usually makes not
#'   much sense (see the examples).
#'
#'   The data frame and \code{\link{OPMS}} methods extract a numeric matrix from
#'   a given data frame or \code{\link{OPMS}} object and pass the result to the
#'   matrix method.
#'
#' @examples
#'
#' data("vaas_4")
#'
#' ## 'OPMS' method
#' # Note that this visualization is not useful when applied to too many wells.
#' # Thus, we here use only a subset of the wells for plotting.
#' (y <- radial_plot(vaas_4[, , 1:5], as.labels = list("Species", "Strain"),
#'   main = "Test", x = 200, y = 200))
#'
#' # with some fine tuning; note the centroids
#' (y <-radial_plot(vaas_4[, , 1:5], as.labels = list("Species", "Strain"),
#'   main = "Test", x = 200, y = 200, rp.type = "s", show.centroid = TRUE))
#'
#' ## Data-frame method (rarely needed)
#' x <- extract(vaas_4, as.labels = list("Species", "Strain"), dataframe = TRUE)
#' (yy <- radial_plot(x[, 1:8], as.labels = c("Species", "Strain"),
#'   main = "Test"))
#' stopifnot(identical(y, yy)) # should also yield the same picture than above
#' stopifnot(is.character(yy), names(yy) == paste(x$Species, x$Strain))
#'
#' ## Matrix method (rarely needed)
#' x <- extract(vaas_4, as.labels = list("Species", "Strain"))
#' (yy <- radial_plot(x[, 1:5], main = "Test"))
#' stopifnot(identical(y, yy)) # should also yield the same picture than above
#' stopifnot(is.character(yy), names(yy) == rownames(x))
#'
setGeneric("radial_plot", function(object, ...) standardGeneric("radial_plot"))

setMethod("radial_plot", "matrix", function(object, as.labels = NULL,
    subset = TRUE, sep = " ", extract.args = list(), rp.type = "p",
    radlab = FALSE, show.centroid = TRUE, show.grid.labels = 1, lwd = 3,
    mar = c(2, 2, 2, 2), line.col = opm_opt("colors"), draw.legend = TRUE,
    x = "bottom", y = NULL, xpd = TRUE, pch = 15, legend.args = list(),
    point.symbols = 15, point.col = opm_opt("colors"), poly.col = NA,
    main = paste0(as.labels, sep = sep), ...) {
  LL(radlab, show.centroid, show.grid.labels, draw.legend, xpd, pch)
  line.col <- try_select_colors(line.col)
  point.col <- try_select_colors(point.col)
  changed.par <- NULL
  on.exit(if (!is.null(changed.par))
    par(changed.par))
  changed.par <- radial.plot(lengths = object[, subset, drop = FALSE],
    labels = colnames(object), rp.type = rp.type, radlab = radlab,
    show.centroid = show.centroid, lwd = lwd, mar = mar,
    show.grid.labels = show.grid.labels, line.col = line.col,
    point.symbols = point.symbols, point.col = point.col, poly.col = poly.col,
    main = main, ...)
  if (!is.null(rn <- rownames(object))) {
    if (draw.legend) {
      legend.args <- insert(as.list(legend.args), x = x, y = y, col = line.col,
        legend = rn, pch = pch, .force = TRUE)
      do.call(legend, legend.args)
    }
    result <- suppressWarnings(cbind(rn, line.col))
    result <- result[seq_len(nrow(object)), , drop = FALSE]
    result <- structure(result[, 2L], names = as.vector(result[, 1L]))
  } else
    result <- NULL
  invisible(result)
}, sealed = SEALED)

setMethod("radial_plot", "data.frame", function(object, as.labels,
    subset = "numeric", sep = " ", extract.args = list(), ...) {
  invisible(radial_plot(extract_columns(object, what = subset,
    direct = FALSE, as.labels = as.labels, sep = sep), ...))
}, sealed = SEALED)

setMethod("radial_plot", OPMS, function(object, as.labels,
    subset = opm_opt("curve.param"), sep = " ", extract.args = list(), ...) {
  extract.args <- insert(as.list(extract.args), list(object = object,
    as.labels = as.labels, as.groups = NULL, subset = subset,
    dataframe = FALSE, ci = FALSE, sep = sep), .force = TRUE)
  invisible(radial_plot(do.call(extract, extract.args), ...))
}, sealed = SEALED)


################################################################################


#' Parallel plot
#'
#' Customised plotting of estimated curve parameter values from single or
#' multiple \acronym{PM} plates using \code{parallelplot} from the \pkg{lattice}
#' package with some adaptations likely to be useful for
#' OmniLog\eqn{\textsuperscript{\textregistered}}{(R)} data.
#'
#' @param x An \code{\link{OPMA}} or \code{\link{OPMS}} object with aggregated
#'   data. This and the following argument can swap their places.
#'
#' @param data Any kind of object that can be used for selecting
#'   \code{\link{metadata}}. Either \code{NULL}, a character vector, a list of
#'   character vectors or a formula indicating which metadata should be
#'   included and used to determine the shape of the plot. The next argument
#'   by default accesses the first metadata entry. If numeric, \code{panel.var}
#'   also accesses the results from applying \code{data}.
#'
#'   Most flexibility is available if \code{data} is a formula. For instance, as
#'   usual the \code{J} pseudo-function can be used to join metadata entries.
#'   Further, if a left part is present, this can indicate the parameters that
#'   should be plotted on the Y-axes (in place of the \code{pnames} argument;
#'   see below for further details). As usual, the right part of the formula
#'   states the meta-information to be included.
#'
#' @param groups Character or numerical scalar determining which metadata entry
#'   or other information, such as the well indexes, (see the examples) is used
#'   for assigning colours to the curves. If a numeric scalar, it refers to the
#'   position of the (potentially merged) metadata entry within \code{data}. If
#'   that argument were empty, a numeric \code{groups} argument would be
#'   ignored. Empty \code{groups} arguments are always ignored; the (constant)
#'   plate type is then used for creating a header.
#'
#' @param panel.var Character or numeric vector indicating which metadata entry
#'   or other information, such as the well indexes, (see the examples) is used
#'   for creating sub-panels. If a numeric vector, it refers to the position of
#'   the (potentially merged) metadata entry within \code{data}. If that
#'   argument were empty, a numeric \code{panel.var} argument would be ignored.
#'
#' @param pnames Character vector or formula to select the curve parameters for
#'   plotting. It has to comprise at least two of the names given by
#'   \code{\link{param_names}()}. If explicitly provided, this argument
#'   overrules the left side, if any, of a formula given as \code{data}
#'   argument. (But the left side, if any, of such a formula would overrule the
#'   default.)
#'
#' @param col Character or numerical scalar or vector. This and the following
#'   arguments work like the eponymous arguments of \code{\link{xy_plot}}. See
#'   there for details.
#' @param strip.fmt List.
#' @param striptext.fmt List.
#' @param legend.fmt List.
#' @param legend.sep Character scalar.
#' @param draw.legend Logical scalar.
#' @param space Character scalar.
#'
#' @param ... Optional arguments passed to \code{parallelplot} from the
#'   \pkg{lattice} package.
#' @details
#' The main applications of this function are ...
#' \strong{[Further details to be added by Lea Vaas]}.
#'
#' @export
#' @family plotting-functions
#' @return An object of class \sQuote{trellis}. See \code{xyplot} from the
#'   \pkg{lattice} package for details.
#' @references Sarkar, D. 2008 \emph{Lattice: Multivariate Data Visualization
#'   with R.} New York: Springer, 265 p.
#' @keywords hplot
#' @seealso lattice::xyplot lattice::parallelplot
#' @author Lea A.I. Vaas
#' @examples
#'
#' ## OPM objects
#'
#' parallelplot(vaas_1)
#' parallelplot(vaas_1, data = list("Species", "Strain"))
#' # ... no effect on selection but on header
#'
#' # value of 'groups' not found in the data: per default no metadata are used
#' x <- try(parallelplot(vaas_1, groups = "Species"), silent = TRUE)
#' stopifnot(inherits(x, "try-error"))
#' # same problem: metadata selected but 'groups' is not contained
#' x <- try(parallelplot(vaas_1, data = list("Species", "Strain"),
#'   groups = "missing"), silent = TRUE)
#' stopifnot(inherits(x, "try-error"))
#' # ... thus it is safer to use a positional 'groups' argument
#'
#' ## OPMS objects
#'
#' # per default metadata are ignored
#' parallelplot(vaas_4[, , 1:10])
#' # otherwise selecting metadata is as usual
#' parallelplot(vaas_4[, , 1:10], data = ~ J(Species, Strain))
#' parallelplot(vaas_4[, , 1:10], data = list("Species", "Strain"))
#'
#' # value of 'groups' not found in the data: per default no metadata are used
#' x <- try(parallelplot(vaas_4[, , 1:10], groups = "Species"), silent = TRUE)
#' stopifnot(inherits(x, "try-error"))
#' # now 'groups' is all present but not a character scalar
#' x <- try(parallelplot(vaas_4[, , 1:10], data = list("Species", "Strain"),
#'   groups = c("Strain", "Species")), silent = TRUE)
#' stopifnot(inherits(x, "try-error"))
#' # here 'groups' is positional but beyond the last element
#' x <- try(parallelplot(vaas_4[, , 1:10], data = list("Species", "Strain"),
#'   groups = 3), silent = TRUE)
#' stopifnot(inherits(x, "try-error"))
#'
#' # 'groups' and 'panel.var' arguments that work
#' parallelplot(vaas_4[, , 1:10], data = ~ J(Species, Strain),
#'   panel.var = "Species", groups = "Strain")
#' parallelplot(vaas_4[, , 1:10], data = "Species", panel.var = "Species",
#'   groups = NULL)
#' parallelplot(vaas_4[, , 1:10], data = list("Species", "Strain"),
#'   panel.var = "Species")
#'
#' # use of non-metadata information: here the names of the wells
#' parallelplot(vaas_4[, , 1:10], data = "Species", panel.var = "Well",
#'   groups = "Species")
#'
#' # selection of parameters via 'pnames'
#' parallelplot(vaas_4[, , 1:10], pnames = ~ A + AUC + mu,
#'   data = ~ Species + Strain, panel.var = "Species",
#'   col = c("black", "red"), groups = "Species")
#' x <- try(parallelplot(vaas_4[, , 1:10], pnames = "A",
#'   data = ~ Species + Strain, panel.var = "Species",
#'   col = c("black", "red"), groups = "Species"), silent = TRUE)
#' stopifnot(inherits(x, "try-error")) # => at least two 'pnames' needed
#'
#' # selecting the parameters via the left side of a 'data' formula
#' parallelplot(vaas_4[, , 1:10], data = A + AUC ~ J(Species, Strain))
#' parallelplot(vaas_4[, , 1:10], data = A + AUC ~ J(Species, Strain),
#'   groups = "Species")
#'
#' # 'pnames' explicitly given => left side of formula ignored
#' parallelplot(vaas_4[, , 1:10], data = A + AUC ~ J(Species, Strain),
#'   pnames = c("A", "mu", "AUC"), groups = "Species")
#'
#' # again: at least two 'pnames' needed
#' x <- try(parallelplot(vaas_4[, , 1:10], data = AUC ~ J(Species, Strain),
#'   groups = "Species"), silent = TRUE)
#' stopifnot(inherits(x, "try-error"))
#'
setGeneric("parallelplot")

setMethod("parallelplot", c("missing", OPMX), function(x, data, ...) {
  parallelplot(data, NULL, ...)
}, sealed = SEALED)

setMethod("parallelplot", c("NULL", OPMX), function(x, data, ...) {
  parallelplot(data, x, ...)
}, sealed = SEALED)

setMethod("parallelplot", c("vector", OPMX), function(x, data, ...) {
  parallelplot(data, x, ...)
}, sealed = SEALED)

setMethod("parallelplot", c("formula", OPMX), function(x, data, ...) {
  parallelplot(data, x, ...)
}, sealed = SEALED)

setMethod("parallelplot", c(OPMX, "missing"), function(x, data, ...) {
  parallelplot(x, NULL, ...)
}, sealed = SEALED)

setMethod("parallelplot", c(OPMX, "ANY"), function(x, data, groups = 1L,
  panel.var = NULL, pnames = param_names(), col = opm_opt("colors"),
  strip.fmt = list(), striptext.fmt = list(), legend.fmt = list(),
  legend.sep = " ", draw.legend = TRUE, space = "top", ...) {

  # Get the used column names from the 'data' argument
  final_dataframe_names <- function(x) {
    if (is.null(x))
      return(x)
    x <- metadata_key(x)
    combined <- attr(x, "combine") # a list mapping new name to old names
    x <- names(x)
    for (i in seq_along(combined)) { # does nothing if combined has zero length!
      j <- match(combined[[i]], x) # find position of old name and replace it
      x[j] <- c(names(combined)[i], rep.int(NA_character_, length(j) - 1L))
    }
    x[!is.na(x)]
  }
  # Convert left side of formula to character vector
  extract_left_side <- function(x) {
    if (!inherits(x, "formula") || length(x) < 3L)
      character()
    else
      all.vars(x[[2L]])
  }
  # Assign first element of 'n' to 'x' ('groups'/'panel.var'), if that
  # is numeric and 'n' is non-empty.
  fetch_from_md_names <- function(x, n) {
    if (!length(x))
      return(x)
    if (is.numeric(x) || is.logical(x))
      if (length(n))
        n[x]
      else
        NULL
    else
      x
  }

  x <- as.data.frame(x = x, include = data, sep = NULL, settings = FALSE)

  # Process the 'param' argument
  if (missing(pnames))
    if (length(tmp <- extract_left_side(data)))
      pnames <- tmp
    else
      pnames <- match.arg(pnames, several.ok = TRUE)
  else {
    if (is.language(pnames))
      pnames <- all.vars(pnames)
    pnames <- match.arg(pnames, param_names(), TRUE)
  }
  if (length(pnames) < 2L)
    stop("'pnames' has to be at least of length 2")

  md.names <- final_dataframe_names(data)

  # Process 'groups'
  groups <- fetch_from_md_names(groups, md.names)
  if (!length(groups))
    groups <- make.names(CSV_NAMES[["PLATE_TYPE"]])
  else if (length(groups) > 1L)
    stop("'groups' argument must be of length 1")
  pos <- match(groups, names(x), 0L)
  if (!pos)
    stop("value of 'groups' not found in the column names of the data")
  # Renaming for lattice. Must be in sync with the processing of 'panel.var'.
  names(x)[pos] <- "_GROUPING"

  # Legend format
  strip.fmt <- insert(as.list(strip.fmt), bg = "grey90")
  striptext.fmt <- insert(as.list(striptext.fmt),
    cex = 1.5 / sqrt(9), lines = 1.25)
  legend.fmt <- insert(as.list(legend.fmt), space = space, .force = FALSE)

  # Legend text and colours
  col <- try_select_colors(col)
  key.text <- levels(x$`_GROUPING`)
  if (length(col) < length(key.text))
    stop("colour should be by plate or metadata, but there are too few colours")
  col <- col[seq_along(key.text)]

  # Build basic formula and process 'panel.var'
  f <- paste0("~ x[", deparse(pnames), "]")
  panel.var <- fetch_from_md_names(panel.var, md.names)

  # Add content of 'panel.var' to formula if it is provided
  if (length(panel.var)) {
    panel.var[match(groups, panel.var)] <- "_GROUPING"
    f <- paste(f, "|", paste0(sprintf("`%s`", panel.var), collapse = " + "))
  }
  f <- formula(f)

  parallelplot(
    x = f, data = x, as.table = TRUE, groups = `_GROUPING`, col = col,
    strip = do.call(strip.custom, strip.fmt), par.strip.text = striptext.fmt,
    key = if (draw.legend)
        c(list(col = col, text = list(key.text)), legend.fmt)
      else
        NULL, ...
  )
}, sealed = SEALED)


################################################################################
