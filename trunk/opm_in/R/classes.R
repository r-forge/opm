

################################################################################
################################################################################
#
# Class definitions and associated functions
#


#' Virtual classes of the \pkg{opm} package
#'
#' Classes that are virtual and thus are not directly dealt with by an \pkg{opm}
#' user: \acronym{WMD}, \acronym{WMDS}, \acronym{FOE}, \acronym{OPMX} and
#' \code{YAML_VIA_LIST}.
#'
#' @details
#' \acronym{WMD} is an acronym for \sQuote{with metadata}.
#' This is a virtual class facilitating the management of metadata. No objects
#' can be created from it because metadata without data make not much sense. It
#' is used by its child classes such as \code{\link{OPM}}.
#'
#' Conceptually, this class treats metadata as arbitrarily nested lists
#' with arbitrary content. Containers of objects that inherit from this class
#' are not forced to contain the same metadata entries. Problems might arise
#' if such data are queried and attempted to be converted to, e.g., data
#' frames because some values might be missing. But metadata can be queried
#' beforehand for the keys as well as the values they contain, and other
#' methods support setting, modifying and deleting metadata.
#'
#' For \code{\link{OPM}} and the other \pkg{opm} classes that use it,
#' \sQuote{metadata} refers to information that, in contrast to, e.g.,
#' \code{\link{csv_data}} must be added by the user \strong{after} reading
#' OmniLog\eqn{\textsuperscript{\textregistered}}{(R)} \acronym{CSV} files.
#' Metadata might already be present in \acronym{YAML} files created by the
#' \pkg{opm} package, however.
#'
#' \acronym{WMDS} is virtual class containing a collection of \acronym{WMD}
#' objects (the name \acronym{WMDS} is just the plural of \acronym{WMD}). It
#' shares many methods with \acronym{WMD} but they often return a collection
#' of the return values of the according \acronym{WMD} method.
#'
#' \acronym{WMDX} is the class union of \acronym{WMD} and \acronym{WMDS}.
#'
#' \acronym{FOE} is an acronym for \sQuote{formula or expression}. This is a
#' virtual class facilitating the implementation of functionality for both
#' formulae and expressions. Methods defined for objects from the class can be
#' applied to either kind of object. See \code{\link{metadata.set}} and
#' \code{\link{map_metadata}} for usage examples.
#'
#' \acronym{OPMX} stands for \sQuote{\acronym{OPM} or \acronym{OPMS}}. It is a
#' virtual class containing helper methods mainly for plotting \code{\link{OPM}}
#' and \code{\link{OPMS}} objects. See \code{\link{show}} and \code{\link{sort}}
#' for usage examples.
#'
#' Similarly, \acronym{XOPMX} unifies \code{\link{OPMS}} and
#' \code{\link{MOPMX}}.
#'
#' See \code{\link{to_yaml}} for a usage example of \code{YAML_VIA_LIST}.
#' This is a virtual class facilitating the conversion to \acronym{YAML} format
#' (or its subset, \acronym{JSON}). It can currently be used by any class that
#' can be coerced to a list.
#'
#' @name WMD
#' @aliases WMD-class
#' @docType class
#' @exportClass WMD
#' @seealso methods::Methods base::matrix base::array base::expression
#'   stats::formula
#' @family classes
#' @keywords methods classes
#' @examples
#' showClass("WMD")
#' showClass("WMDS")
#' showClass("OPMX")
#' showClass("XOPMX")
#' showClass("FOE")
#' showClass("YAML_VIA_LIST")
#'
setClass("WMD",
  slots = c(metadata = "list"),
  contains = "VIRTUAL",
  sealed = SEALED
)

#' @rdname WMD
#' @name WMDS
#' @aliases WMDS-class
#' @docType class
#' @exportClass WMDS
#'
setClass("WMDS",
  slots = c(plates = "list"),
  contains = "VIRTUAL",
  sealed = SEALED
)


#' @rdname WMD
#' @name WMDX
#' @aliases WMDX-class
#' @docType class
#' @exportClass WMDX
#'
NULL

setClassUnion("WMDX", c("WMD", "WMDS"))


#' @rdname WMD
#' @name FOE
#' @aliases FOE-class
#' @docType class
#' @exportClass FOE
#'
NULL

setClassUnion("FOE", c("formula", "expression"))


################################################################################


#' Real classes of the \pkg{opm} package
#'
#' Classes whose members can be generated and manipulated by an \pkg{opm} user:
#' \acronym{OPM}, \acronym{OPMA}, \acronym{OPMD}, \acronym{OPMS} and
#' \acronym{MOPMX}.
#'
#' @details
#' \acronym{OPM} is an acronym for
#' \sQuote{OmniLog\eqn{\textsuperscript{\textregistered}}{(R)} Phenotype
#' Microarray}. This is the class for holding single-plate
#' OmniLog\eqn{\textsuperscript{\textregistered}}{(R)} phenotype microarray data
#' without aggregated values, but with information read from the original input
#' \acronym{CSV} files as well as an additional arbitrary amount of arbitrarily
#' organised metadata. Objects of this class are usually created by inputting
#' files with \code{\link{read_single_opm}} or \code{\link{read_opm}}, not with
#' a call to \code{new} or \code{as}.
#'
#' \acronym{OPM} inherits from \code{\link{WMD}} and, hence, has all its
#' methods.
#'
#' Regarding the coercion of this class to other classes (see the \code{as}
#' method from the \pkg{methods} package), consider the following:
#' \itemize{
#'   \item Conversion of its child classes to this class is straightforward, see
#'   the examples below.
#'   \item The coercion of this class (and its child classes) to a list (and
#'   vice versa) is only for expert users and relies on a mapping between slot
#'   names and keys in the list, i.e. the list must be appropriately named. For
#'   instance, this is the mechanism when reading from and writing to
#'   \acronym{YAML}, see \code{\link{to_yaml}}.
#'   \item Coercions to other data frames and matrices first coerce the
#'   \code{\link{measurements}} and then add the other slots as attributes.
#'   \item Methods such as \code{\link{flatten}} and \code{\link{extract}} might
#'   be way more appropriate for converting \acronym{OPM} objects.
#' }
#'
#' \acronym{OPMA} is an acronym for \sQuote{\acronym{OPM}, aggregated}. This is
#' the class for holding single-plate
#' OmniLog\eqn{\textsuperscript{\textregistered}}{(R)} phenotype microarray data
#' together with aggregated values. Objects of this class are usually created by
#' calling \code{\link{do_aggr}} on an \acronym{OPM} object, or by inputting
#' files with \code{\link{read_single_opm}} or \code{\link{read_opm}} if these
#' files already contain aggregated data.
#'
#' \acronym{OPMA} inherits from \acronym{OPM} and, hence, has all its methods.
#'
#' \acronym{OPMD} is an acronym for \sQuote{\acronym{OPM}, discretised}. This is
#' the class for holding single-plate
#' OmniLog\eqn{\textsuperscript{\textregistered}}{(R)} phenotype microarray data
#' together with aggregated \strong{and} discretised values. Objects of this
#' class are usually created by calling \code{\link{do_disc}} on an
#' \acronym{OPMA} object, or by inputting files with
#' \code{\link{read_single_opm}} or \code{\link{read_opm}} if these files
#' already contain discretised data.
#'
#' \acronym{OPMD} inherits from \acronym{OPMA} and, hence, has all its methods.
#'
#' The discretised data are considered as \sQuote{consistent} with the curve
#' parameter from which they have been estimated if no \code{FALSE} value
#' corresponds to curve parameter larger than the curve parameter of any
#' \code{TRUE} value; \code{NA} values are not considered when checking
#' consistency. The \code{strict.OPMD} entry of \code{\link{opm_opt}} determines
#' whether an error or only a warning is issued in the case of inconsistency.
#'
#' \acronym{OPMS} is the class for holding multiple-plate
#' OmniLog\eqn{\textsuperscript{\textregistered}}{(R)} phenotype microarray data
#' with or without aggregated or discretised values. Regarding the name:
#' \acronym{OPMS} is just the plural of \acronym{OPM}. Objects of this class are
#' usually created by calling \code{\link{opms}} or other combination functions
#' on \acronym{OPM} or derived objects, or by inputting files with
#' \code{\link{read_opm}} if these files altogether contain more than a single
#' plate. \acronym{OPMS} objects are not normally created with a call to
#' \code{new} or \code{as}. The data may have been obtained from distinct
#' organisms and/or replicates, but \strong{must} correspond to the same plate
#' type and \strong{must} contain the same wells.
#'
#' \acronym{OPMS} inherits from \code{\link{WMDS}} and, hence, has all its
#' methods. As a rule, \acronym{OPMS} has the same methods as the \acronym{OPM}
#' class, but adapted to a collection of more than one \acronym{OPM} object.
#' Also, \acronym{OPMS} can hold \acronym{OPMD} and \acronym{OPMA} as well as
#' \acronym{OPM} objects, even though this is not indicated for all its methods
#' in this manual.
#'
#' \acronym{MOPMX} is an object for holding \acronym{OPMX} objects with
#' potentially multiple plate types. Regarding the name: the \acronym{M} stands
#' for \sQuote{multiple}. \acronym{MOPMX} objects are generated by
#' \code{\link{read_opm}(convert = "grp")} and \code{\link{opms}(group = TRUE)}.
#' \acronym{MOPMX} objects can also be created with \code{new} or \code{as} and
#' then further manipulated; see the examples below. \acronym{MOPMX} objects in
#' many aspects behave like lists.
#'
#' \acronym{OPM_MCP_OUT} is a data-frame based class useful as intermediate
#' result of \code{\link{opm_mcp}}. See there and its \code{\link{annotated}}
#' method for usages.
#'
#' @examples
#'
#' ## overview on the classes
#' showClass("OPM")
#' showClass("OPMA")
#' showClass("OPMD")
#' showClass("OPMS")
#' showClass("MOPMX")
#'
#' ## OPMX conversions with as()
#' showMethods("coerce", classes = c("OPM", "OPMA", "OPMD", "OPMS"))
#' data(vaas_1)
#' data(vaas_4)
#' (x <- as(vaas_1, "OPMA")) # drops the discretised data
#' stopifnot(has_disc(vaas_1), !has_disc(x))
#' (x <- as(vaas_1, "OPM")) # drops the aggregated data
#' stopifnot(has_aggr(vaas_1), !has_aggr(x))
#'
#' ## MOPMX creation and conversion
#' (x <- new("MOPMX")) # don't do this with the other classes
#' (x <- as(vaas_1, "MOPMX"))
#' (x <- as(vaas_4, "MOPMX"))
#' # conversion backwards is only possible as long as the MOPMX object contains
#' # only a single OPMX object
#' showMethods("coerce", classes = "MOPMX")
#'
#' @docType class
#' @export
#' @aliases OPM-class
#' @seealso methods::Methods methods::new
#' @family classes
#' @keywords methods classes
#'
setClass("OPM",
  slots = c(measurements = "matrix", csv_data = "character"),
  contains = "WMD",
  validity = function(object) {
    errs <- c(opm_problems(object@measurements), opm_problems(object@csv_data))
    if (length(errs))
      errs
    else
      TRUE
  },
  sealed = SEALED
)

#' @rdname OPM
#' @name OPMA
#' @aliases OPMA-class
#' @docType class
#' @export
#'
setClass("OPMA",
  slots = c(aggregated = "matrix", aggr_settings = "list"),
  contains = "OPM",
  validity = function(object) {
    settings <- object@aggr_settings
    if (length(errs <- opma_problems(settings)))
      settings <- NULL # => no settings-based checks of the matrix
    errs <- c(errs, opma_problems(object@aggregated, object@measurements,
      settings))
    if (length(errs))
      errs
    else
      TRUE
  },
  sealed = SEALED
)

#' @rdname OPM
#' @name OPMD
#' @aliases OPMD-class
#' @docType class
#' @export
#'
setClass("OPMD",
  slots = c(discretized = "logical", disc_settings = "list"),
  contains = "OPMA",
  validity = function(object) {
    errs <- opmd_problems(object@disc_settings)
    errs <- c(errs, opmd_problems(object@aggregated, object@discretized,
      object@disc_settings[[c(OPTIONS, "parameter")]]))
    if (length(errs))
      errs
    else
      TRUE
  },
  sealed = SEALED
)

#' @rdname OPM
#' @name OPMS
#' @aliases OPMS-class
#' @docType class
#' @export
#'
setClass("OPMS",
  contains = "WMDS",
  validity = function(object) {
    if (length(errs <- opms_problems(object@plates)))
      errs
    else
      TRUE
  },
  sealed = SEALED
)

#' @rdname OPM
#' @name MOPMX
#' @aliases MOPMX-class
#' @docType class
#' @export
#'
setClass("MOPMX",
  contains = "list",
  slots = c(names = "character"),
  prototype = prototype(names = character()),
  validity = function(object) {
    if (all(vapply(object@.Data, is, NA, "OPMX")))
      TRUE
    else
      "not ell elements inherit from 'OPMX'"
  }, sealed = SEALED
)


#' @rdname OPM
#' @name OPM_MCP_OUT
#' @aliases OPM_MCP_OUT-class
#' @docType class
#' @export
#'
setClass("OPM_MCP_OUT",
  contains = "data.frame",
  validity = function(object) {
    errs <- NULL
    for (name in RESERVED_NAMES[c("well", "value")])
      if (!name %in% colnames(object))
        errs <- c(errs, sprintf("missing column named '%s'", name))
    if (length(errs))
      errs
    else
      TRUE
  },
  sealed = SEALED
)


################################################################################


#' @rdname WMD
#' @name OPMX
#' @aliases OPMX-class
#' @docType class
#' @exportClass OPMX
#'
NULL

# Currently the child classes must provide plate_type() and minmax() for the
# methods to work
#
setClassUnion("OPMX", c("OPM", "OPMS"))


#' @rdname WMD
#' @name XOPMX
#' @aliases XOPMX-class
#' @docType class
#' @exportClass XOPMX
#'
NULL

setClassUnion("XOPMX", c("MOPMX", "OPMS", "OPM"))


#' @rdname WMD
#' @name YAML_VIA_LIST
#' @aliases YAML_VIA_LIST-class
#' @docType class
#' @exportClass YAML_VIA_LIST
#'
NULL

setOldClass("print_easy")

setClassUnion("YAML_VIA_LIST", c("OPM", "OPMS", "print_easy"))


################################################################################


# CMAT class: undocumented, as for internal use only.
#
setClass("CMAT",
  contains = "matrix",
  validity = function(object) {
    errs <- character()
    if (is.null(rownames(object)) || anyNA(rownames(object)))
      errs <- c(errs, "missing row names")
    mode <- typeof(object)
    if (mode == "list") {
      mode <- unique.default(vapply(object, typeof, ""))
      if (length(mode) > 1L)
        errs <- c(errs, "non-uniform list elements contained")
      if (any(lengths(object, FALSE) < 1L))
        errs <- c(errs, "empty list elements contained")
    }
    mode <- setdiff(mode, c("character", "integer", "double", "logical"))
    if (length(mode))
      errs <- c(errs, sprintf("unsupported storage mode: '%s'", mode))
    if (length(errs))
      errs
    else
      TRUE
  },
  sealed = SEALED
)


################################################################################
#
# Conversion functions: OPMX <=> other objects. For principle, see description
# of OPM class. Conversion of OPMA or OMDS to matrix/data frame is just repeated
# from OPM because otherwise some elements would be missing.
#


## OPM

setAs("OPM", "matrix", function(from) {
  attach_attr(from, from@measurements)
})

setAs("OPM", "data.frame", function(from) {
  attach_attr(from, as.data.frame(from@measurements))
})

setAs("OPM", "list", function(from) {
  list(metadata = metadata(from), csv_data = as.list(csv_data(from)),
    measurements = as.list(as.data.frame(measurements(from))))
})

setAs("list", "OPM", function(from) {
  convert_measurements <- function(mat) {
    mat <- must(do.call(cbind, lapply(mat, as.numeric)))
    if (length(hour.pos <- which(colnames(mat) == HOUR)) != 1L)
      stop("uninterpretable column names in list element 'measurements'")
    sorted.names <- c(colnames(mat)[hour.pos],
      sort.int(colnames(mat)[-hour.pos]))
    mat[, sorted.names, drop = FALSE]
  }
  md <- repair_na_strings.list(as.list(from$metadata), "character")
  new(Class = "OPM", csv_data = map_names(unlist(from$csv_data), rescue_dots),
    metadata = map_names(md, rescue_dots),
    measurements = convert_measurements(from$measurements))
})


## OPMA

setAs("OPMA", "matrix", function(from) {
  attach_attr(from, from@measurements)
})

setAs("OPMA", "data.frame", function(from) {
  attach_attr(from, as.data.frame(from@measurements))
})

setAs("OPMA", "list", function(from) {
  result <- as(as(from, "OPM"), "list")
  result$aggregated <- apply(aggregated(from), 2L, as.list)
  result$aggr_settings <- aggr_settings(from)
  result
})

setAs("list", "OPMA", function(from) {
  select_aggr <- function(x, wanted) {
    x <- repair_na_strings(lapply(x, `[`, unlist(map_param_names())))
    x <- do.call(cbind, x[wanted])
    must(mode(x) <- "numeric")
    x # should now be matrix, reduced to the known wells, parameters and CIs
  }
  x <- as(from, "OPM")
  new(Class = "OPMA", measurements = measurements(x),
    csv_data = csv_data(x), metadata = metadata(x),
    aggregated = select_aggr(from$aggregated, colnames(x@measurements)[-1L]),
    aggr_settings = update_settings_list(as.list(from$aggr_settings)))
})


## OPMD

setAs("OPMD", "matrix", function(from) {
  attach_attr(from, from@measurements)
})

setAs("OPMD", "data.frame", function(from) {
  attach_attr(from, as.data.frame(from@measurements))
})

setAs("OPMD", "list", function(from) {
  result <- as(as(from, "OPMA"), "list")
  result$discretized <- as.list(from@discretized)
  result$disc_settings <- from@disc_settings
  result
})

setAs("list", "OPMD", function(from) {
  # up to official release opm 0.10.0, the discretized curve parameter had
  # not been included in the discretization settings
  repair_missing_parameter <- function(x) {
    if (x[[SOFTWARE]] != opm_string())
      return(x)
    if (is.null(x[[c(OPTIONS, "parameter")]])) {
      warning("assuming discretized parameter is opm_opt('curve.param')")
      x[[c(OPTIONS, "parameter")]] <- opm_opt("curve.param")
    }
    x
  }
  x <- as(from, "OPMA")
  settings <- update_settings_list(as.list(from$disc_settings))
  settings <- repair_missing_parameter(settings)
  discretized <- from$discretized[colnames(x@aggregated)]
  discretized <- unlist(repair_na_strings(discretized, "logical"))
  new(Class = "OPMD", csv_data = csv_data(x), measurements = measurements(x),
    metadata = metadata(x), aggr_settings = aggr_settings(x),
    aggregated = aggregated(x), discretized = discretized,
    disc_settings = settings)
})


## OPMS

setAs("OPMS", "list", function(from) {
  lapply(from@plates, as, "list")
})

setAs("list", "OPMS", function(from) {
  opmd.slots <- setdiff(slotNames("OPMD"), opma.slots <- slotNames("OPMA"))
  opma.slots <- setdiff(opma.slots, slotNames("OPM"))
  new(Class = "OPMS", plates = lapply(from, function(x) {
    as(x, if (all(opma.slots %in% names(x)))
      if (all(opmd.slots %in% names(x)))
        "OPMD"
      else
        "OPMA"
      else
        "OPM")
  }))
})


## MOPMX

setAs("list", "MOPMX", function(from) {
  new("MOPMX", from) # overwritten to enforce consistency checks
})

setAs("OPMX", "MOPMX", function(from) {
  x <- new("MOPMX", list(from))
  names(x) <- plate_type(from)
  x
})

setAs("MOPMX", "OPMX", function(from) {
  if (length(from) != 1L)
    stop("conversion impossible: number of elements is not 1")
  from[[1L]]
})


## CMAT

setAs("matrix", "CMAT", function(from) {
  new("CMAT", from) # overwritten to enforce consistency checks
})


################################################################################


#' Classes for \pkg{opm} database I/O
#'
#' These child classes of \code{DBTABLES} from the \pkg{pkgutils} package hold
#' intermediary objects that can be used for database input and output of
#' \code{\link{OPMX}} objects. These classes are not normally directly dealt
#' with by an \pkg{opm} user but are documented here for completeness. See
#' \code{\link{opm_dbput}} for methods that internally use these classes for
#' database I/O.
#'
#' @details
#'   See their documentation for details on \code{\link{OPMX}} objects
#'   themselves.
#'   We here define the following additional classes: \describe{
#'   \item{OPM_DB}{Holds all data that occur in an \code{\link{OPM}} object, or
#'   in several such objects as contained in an \code{\link{OPMS}} object.}
#'   \item{OPMA_DB}{Holds all data that occur in an \code{\link{OPMA}} object,
#'   or in several such objects as contained in an \code{\link{OPMS}} object.}
#'   \item{OPMD_DB}{Holds all data that occur in an \code{\link{OPMD}} object,
#'   or in several such objects as contained in an \code{\link{OPMS}} object.}
#'   }
#'   The inheritance relationships thus mirror those of the \code{\link{OPMX}}
#'   objects (with the exception of \code{\link{OPMS}}). Conversion with
#'   \code{as} is implemented from all \code{\link{OPMX}} classes to all classes
#'   defined here. Lists can also be converted provided they only contain
#'   \code{\link{OPMX}} objects (or lists of such objects).
#'
#'   Conversion in the other direction, yielding one of the \code{\link{OPMX}}
#'   classes, is also implemented. Attempting to convert several plates to an
#'   \code{\link{OPMX}} class other than \code{\link{OPMS}} will yield an error,
#'   however, as well as trying to convert a single plate to \code{\link{OPMS}},
#'   or several plates with distinct plate types. In contrast, conversion to a
#'   list will work in all instances, and such a list could further be processed
#'   with the \code{\link{opms}} function, irrespective of the number of plates
#'   contained.
#'
#'   In contrast to the \code{\link{OPMX}} classes, the three ones defined here
#'   can be created using \code{new}, yielding empty objects. These can neither
#'   be converted to \code{\link{OPMX}} objects nor combined with them using
#'   \code{c}. Instead, they are useful in conjunction with \code{by} from the
#'   \pkg{pkgutils} package with \code{do_inline} set to \code{TRUE}. They
#'   contain all \code{fkeys} information and can be filled using a suitable
#'   \code{FUN} argument.
#'
#' @docType class
#' @export
#' @name OPM_DB
#' @aliases OPM_DB-class
#' @seealso methods::Methods methods::new opm::opms
#' @family classes
#' @keywords methods classes database
#' @examples
#'
#' library(pkgutils)
#'
#' ## overview on the classes
#' showClass("OPM_DB")
#' showClass("OPMA_DB")
#' showClass("OPMD_DB")
#'
#' ## show all conversions with as()
#' showMethods("coerce", classes = c("OPM_DB", "OPMA_DB", "OPMD_DB"))
#'
#' ## conversions back and forth, OPMD as starting point
#' (x <- as(vaas_1, "OPMD_DB"))
#' (y <- as(x, "OPMD"))
#' stopifnot(
#'   dim(y) == dim(vaas_1),
#'   # numeric data remain except for rounding errors:
#'   all.equal(measurements(y), measurements(vaas_1)),
#'   all.equal(aggregated(y), aggregated(vaas_1)),
#'   all.equal(discretized(y), discretized(vaas_1)),
#'   # file names get normalized, hence CSV dat may get unequal:
#'   !isTRUE(all.equal(csv_data(y), csv_data(vaas_1)))
#' )
#' (y <- try(as(x, "OPMS"), silent = TRUE))
#' stopifnot(inherits(y, "try-error")) # does not work because only 1 plate
#'
#' ## conversions back and forth, OPMS as starting point
#' (small <- vaas_4[, , 11:15])
#' (x <- as(small, "OPMD_DB"))
#' (y <- as(x, "OPMS"))
#' stopifnot(sapply(1:length(y), # same values
#'   function(i) dim(y[i]) == dim(small[i])))
#' (y <- try(as(x, "OPMD"), silent = TRUE)) # does not work because > 1 plate
#' stopifnot(inherits(y, "try-error"))
#' (y <- as(x, "list")) # one can always go through a list
#' stopifnot(sapply(y, is, "OPMD")) # opms() could now be called
#'
#' ## one can create new objects without data
#' (y <- new("OPMD_DB"))
#' stopifnot(fkeys_valid(y), fkeys(y) == fkeys(x), !length(y))
#' # such objects cannot be converted to OPMX but can be filled using by()
#'
setClass("OPM_DB",
  contains = "DBTABLES",
  slots = c(plates = "data.frame", wells = "data.frame",
    measurements = "data.frame"),
  prototype = list(plates = data.frame(id = integer(), machine_id = integer()),
    wells = data.frame(id = integer(), plate_id = integer()),
    measurements = data.frame(id = integer(), well_id = integer())),
  validity = fkeys_valid,
  sealed = SEALED
)

#' @rdname OPM_DB
#' @name OPMA_DB
#' @aliases OPMA_DB-class
#' @docType class
#' @export
#'
setClass("OPMA_DB",
  contains = "OPM_DB",
  # the superclass slots must be repeated here to enforce the ordering
  slots = c(plates = "data.frame", wells = "data.frame",
    measurements = "data.frame", aggr_settings = "data.frame",
    aggregated = "data.frame"),
  prototype = list(plates = data.frame(id = integer(), machine_id = integer()),
    wells = data.frame(id = integer(), plate_id = integer()),
    measurements = data.frame(id = integer(), well_id = integer()),
    aggr_settings = data.frame(id = integer(), plate_id = integer()),
    aggregated = data.frame(id = integer(), well_id = integer(),
      aggr_setting_id = integer())),
  validity = fkeys_valid,
  sealed = SEALED
)

#' @rdname OPM_DB
#' @name OPMD_DB
#' @aliases OPMD_DB-class
#' @docType class
#' @export
#'
setClass("OPMD_DB",
  contains = "OPMA_DB",
  # the superclass slots must be repeated here to enforce the ordering
  slots = c(plates = "data.frame", wells = "data.frame",
    measurements = "data.frame", aggr_settings = "data.frame",
    aggregated = "data.frame", disc_settings = "data.frame",
    discretized = "data.frame"),
  prototype = list(plates = data.frame(id = integer(), machine_id = integer()),
    wells = data.frame(id = integer(), plate_id = integer()),
    measurements = data.frame(id = integer(), well_id = integer()),
    aggr_settings = data.frame(id = integer(), plate_id = integer()),
    aggregated = data.frame(id = integer(), well_id = integer(),
      aggr_setting_id = integer()),
    disc_settings = data.frame(id = integer(), plate_id = integer()),
    discretized = data.frame(id = integer(), well_id = integer(),
      disc_setting_id = integer())),
  validity = fkeys_valid,
  sealed = SEALED
)


################################################################################
#
# Conversions with as()
#


setAs("OPM", "OPMA", function(from) {
  stop("do_aggr() is needed to aggregate OPM objects")
})

setAs("OPM", "OPMD", function(from) {
  stop("do_aggr() and do_disc() are needed to discretise OPM objects")
})

setAs("OPMA", "OPMD", function(from) {
  stop("do_disc() is needed to discretise OPMA objects")
})


################################################################################
#
# Conversion to and from database I/O objects
#


setAs("OPM", "OPM_DB", function(from) {
  x <- forward_OPM_to_list(from)
  new(Class = "OPM_DB", plates = x$plates, wells = x$wells,
    measurements = x$measurements)
})

setAs("OPM_DB", "OPM", function(from) {
  as(backward_OPM_to_list(from), "OPM")
})

setAs("OPMA", "OPMA_DB", function(from) {
  x <- forward_OPMA_to_list(from)
  new(Class = "OPMA_DB", plates = x$plates, wells = x$wells,
    measurements = x$measurements, aggr_settings = x$aggr_settings,
    aggregated = x$aggregated)
})

setAs("OPMA_DB", "OPMA", function(from) {
  as(backward_OPMA_to_list(from), "OPMA")
})

setAs("OPMD", "OPMD_DB", function(from) {
  x <- forward_OPMA_to_list(from)
  d.sets <- settings_forward(from@disc_settings, x$plates[, "id"])
  d.data <- from@discretized
  storage.mode(d.data) <- "integer" # RMySQL might otherwise set all to FALSE
  d.data <- data.frame(id = seq_along(d.data), stringsAsFactors = FALSE,
    well_id = match(names(d.data), x$wells[, "coordinate"]),
    disc_setting_id = 1L, value = unname(d.data), check.names = FALSE)
  new(Class = "OPMD_DB", plates = x$plates, wells = x$wells,
    measurements = x$measurements, aggr_settings = x$aggr_settings,
    aggregated = x$aggregated, disc_settings = d.sets, discretized = d.data)
})

setAs("OPMD_DB", "OPMD", function(from) {
  as(backward_OPMD_to_list(from), "OPMD")
})


################################################################################
#
# Conversion to and from lists
#


setAs("list", "OPM_DB", function(from) {
  do.call(c, rapply(object = from, f = as, Class = "OPM_DB"))
})

setAs("list", "OPMA_DB", function(from) {
  do.call(c, rapply(object = from, f = as, Class = "OPMA_DB"))
})

setAs("list", "OPMD_DB", function(from) {
  do.call(c, rapply(object = from, f = as, Class = "OPMD_DB"))
})

setAs("OPM_DB", "list", function(from) {
  lapply(split(from), as, "OPM")
})

setAs("OPMA_DB", "list", function(from) {
  lapply(split(from), as, "OPMA")
})

setAs("OPMD_DB", "list", function(from) {
  lapply(split(from), as, "OPMD")
})


################################################################################
#
# Conversion to and from OPMS objects
#


setAs("OPMS", "OPM_DB", function(from) {
  do.call(c, lapply(from@plates, as, "OPM_DB"))
})

setAs("OPMS", "OPMA_DB", function(from) {
  do.call(c, lapply(from@plates, as, "OPMA_DB"))
})

setAs("OPMS", "OPMD_DB", function(from) {
  do.call(c, lapply(from@plates, as, "OPMD_DB"))
})

setAs("OPM_DB", "OPMS", function(from) {
  as(lapply(split(from), backward_OPM_to_list), "OPMS")
})

setAs("OPMA_DB", "OPMS", function(from) {
  as(lapply(split(from), backward_OPMA_to_list), "OPMS")
})

setAs("OPMD_DB", "OPMS", function(from) {
  as(lapply(split(from), backward_OPMD_to_list), "OPMS")
})



################################################################################
#
# Conversion to and from MOPMX objects
#


setAs("MOPMX", "OPM_DB", function(from) {
  do.call(c, lapply(unname(from), as, "OPM_DB"))
})

setAs("MOPMX", "OPMA_DB", function(from) {
  do.call(c, lapply(unname(from), as, "OPMA_DB"))
})

setAs("MOPMX", "OPMD_DB", function(from) {
  do.call(c, lapply(unname(from), as, "OPMD_DB"))
})

setAs("OPM_DB", "MOPMX", function(from) {
  do.call(opms, c(as(from, "list"),
    list(precomputed = TRUE, skip = FALSE, group = TRUE)))
})

################################################################################


