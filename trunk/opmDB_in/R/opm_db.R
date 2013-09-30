

################################################################################


#' Classes for opm database I/O
#'
#' These child classes of \code{\link{DBTABLES}} hold intermediary objects that
#' can be used for database input and output of \acronym{OPMX} objects as
#' created by \pkg{opm}.
#'
#' @details
#'   See the \pkg{opm} documentation for details on \acronym{OPMX} objects
#'   themselves. \pkg{opmDB} defines the following additional classes:
#'   \describe{
#'   \item{OPM_DB}{Holds all data that occur in an \acronym{OPM} object, or in
#'   several such objects as contained in an \acronym{OPMS} object.}
#'   \item{OPMA_DB}{Holds all data that occur in an \acronym{OPMA} object, or in
#'   several such objects as contained in an \acronym{OPMS} object.}
#'   \item{OPMD_DB}{Holds all data that occur in an \acronym{OPMD} object, or in
#'   several such objects as contained in an \acronym{OPMS} object.}
#'   }
#'   The inheritance relationships thus mirror those of the \code{OPMX} objects
#'   (with the exception of \acronym{OPMS}). Conversion with \code{as} is
#'   implemented from all \acronym{OPMX} classes to all classes defined here.
#'   Lists can also be converted provided they only contain \acronym{OPMX}
#'   objects (or lists of such objects).
#'
#'   Conversion in the other direction, yielding one of the \acronym{OPMX}
#'   classes, is also implemented. Attempting to convert several plates to an
#'   \code{OPMX} class other than \code{OPMS} will yield an error, however, as
#'   well as trying to convert a single plate to \code{OPMS}, or several plates
#'   with distinct plate types. In contrast, conversion to a list will work in
#'   all instances, and such a list could further be processed with the
#'   \code{opms} function from the \pkg{opm} package, irrespective of the number
#'   of plates contained.
#'
#'   In contrast to the \acronym{OPMX} classes, the three ones defined here can
#'   be created using \code{new}, yielding empty objects. These cannot neither
#'   be converted to \acronym{OPMX} objects nor combined with them using
#'   \code{\link{c}}. Instead, they are useful in conjunction with
#'   \code{\link{collect}}. They contain all \code{\link{fkeys}} information and
#'   can be filled using a suitable \code{FUN} argument.
#'
#' @docType class
#' @export
#' @name OPM_DB-classes
#' @aliases OPM_DB
#' @aliases OPM_DB-class
#' @seealso methods::Methods methods::new opm::opms
#' @family opm_db-functions
#' @keywords methods classes
#' @examples
#'
#' ## conversions back and forth, OPMD as starting point
#' (x <- as(vaas_1, "OPMD_DB"))
#' (y <- as(x, "OPMD"))
#' stopifnot(all.equal(unclass(y), unclass(vaas_1))) # same values
#' (y <- try(as(x, "OPMS"), silent = TRUE))
#' stopifnot(inherits(y, "try-error")) # does not work because only 1 plate
#'
#' ## conversions back and forth, OPMS as starting point
#' (x <- as(vaas_4, "OPMD_DB"))
#' (y <- as(x, "OPMS"))
#' stopifnot(sapply(1:length(y), # same values
#'   function(i) all.equal(unclass(y[i]), unclass(vaas_4[i]))))
#' (y <- try(as(x, "OPMD"), silent = TRUE)) # does not work because > 1 plate
#' stopifnot(inherits(y, "try-error"))
#' (y <- as(x, "list")) # one can always go through a list
#' stopifnot(sapply(y, is, "OPMD")) # opms() could now be called
#'
#' ## one can create new objects without data
#' (y <- new("OPMD_DB"))
#' stopifnot(fkeys_valid(y), fkeys(y) == fkeys(x), !length(y))
#' # such objects cannot be converted to OPMX but can be filled using collect()
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

#' @rdname OPM_DB-classes
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

#' @rdname OPM_DB-classes
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


setAs("OPM", "OPM_DB", function(from) {
  x <- forward_OPM_to_list(from)
  new("OPM_DB", plates = x$plates, wells = x$wells,
    measurements = x$measurements)
})

setAs("OPM_DB", "OPM", function(from) {
  as(backward_OPM_to_list(from), "OPM")
})

setAs("OPMA", "OPMA_DB", function(from) {
  x <- forward_OPMA_to_list(from)
  new("OPMA_DB", plates = x$plates, wells = x$wells,
    measurements = x$measurements, aggr_settings = x$aggr_settings,
    aggregated = x$aggregated)
})

setAs("OPMA_DB", "OPMA", function(from) {
  as(backward_OPMA_to_list(from), "OPMA")
})

setAs("OPMD", "OPMD_DB", function(from) {
  x <- forward_OPMA_to_list(from)
  dsets <- settings_forward(from@disc_settings, x$plates[, "id"])
  ddata <- from@discretized
  ddata <- data.frame(id = seq_along(ddata), stringsAsFactors = FALSE,
    well_id = match(names(ddata), x$wells[, "coordinate"]),
    disc_setting_id = 1L, value = unname(ddata), check.names = FALSE)
  new("OPMD_DB", plates = x$plates, wells = x$wells,
    measurements = x$measurements, aggr_settings = x$aggr_settings,
    aggregated = x$aggregated, disc_settings = dsets, discretized = ddata)
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


