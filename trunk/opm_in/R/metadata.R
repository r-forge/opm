

################################################################################
################################################################################
#
# Setter functions for metadata
#


#' Replace metadata
#'
#' Set the meta-information stored together with the data. For most kinds of
#' arguments the \code{\link{WMDS}} and \code{\link{MOPMX}} methods set the
#' meta-information stored together with the measurements for all plates at
#' once. But they can address the plates individually if \code{value} is a data
#' frame, and they can address metadata keys individually if \code{value} is a
#' formula.
#'
#' @name metadata.set
#' @aliases metadata<-
#'
#' @param object \code{\link{WMD}}, \code{\link{WMDS}} or \code{\link{MOPMX}}
#'   object.
#' @param key Missing, numeric scalar, character vector, factor, or list.
#' \itemize{
#'   \item If missing, this mostly means replace all metadata by \code{value},
#'   but behaviour is special for some kinds of \code{value} arguments. See
#'   below for details.
#'   \item If a numeric scalar, then if positive, prepend \code{value} to old
#'   metadata. If negative, append \code{value} to old metadata. If zero,
#'   replace old metadata entirely by \code{value}.
#'   \item If a list, treated as list of keys; expect \code{value} to be a list
#'   of corresponding metadata values to be set. Names are replaced by the
#'   values of either list if they are missing.
#'   \item If a character vector, used as key for setting/replacing this
#'   metadata entry to/by \code{value}. It is an error if \code{key} has zero
#'   length. If it contains more than one entry, a nested query is done. See
#'   \code{[[} from the \pkg{base} package for details.
#'   \item The factor method calls the character method after converting
#'   \code{key} to mode \sQuote{character}.
#' }
#' @param value Character vector, list, data frame, formula, \code{\link{WMD}}
#'   or \code{\link{WMDS}} object. As the metadata are stored as a list, other
#'   kinds of objects used as \code{value} are special, particularly if
#'   \code{key} is missing. \itemize{
#'   \item If \code{key} is a character vector, this can be arbitrary value(s)
#'   to be included in the metadata (if \code{NULL}, this metadata entry is
#'   deleted).
#'   \item If \code{key} is missing and \code{value} is a list but not a data
#'   frame, all metadata are replaced by it. If \code{value} is of mode
#'   \sQuote{logical}, \code{TRUE} causes all \code{\link{csv_data}} entries
#'   that are \emph{not} included in \code{\link{opm_opt}("csv.selection")} to
#'   be included in the metadata; \code{FALSE} causes these entries, if any, to
#'   be removed. If \code{value} is a character vector and it contains the
#'   value given by \code{\link{opm_opt}("md.id.name")}, then by default a
#'   globally unique \acronym{ID} identifying each plate is included in the
#'   metadata. Uniqueness only holds per session and can be circumvented by
#'   modifying \code{\link{opm_opt}("md.id.start")}. Other elements of a
#'   character vector are not currently supported (they may get a special
#'   meaning later on).
#'   \item If \code{key} is otherwise, \code{value} must be list of values to be
#'   prepended, appended or set as metadata, either entirely or specifically,
#'   depending on \code{key}.
#'   \item Formulae can also be used as \code{value}. In that case, the formula
#'   can specify the key to be replaced. See the examples below and
#'   \code{\link{map_values}} for details.
#'   \item If \code{object} is of class \code{\link{WMDS}}, \code{value} can be
#'   a data frame whose number of rows must be equal to the number of plates.
#'   Metadata to be set will then be selected from each individual row in turn
#'   and in input order. This works analogously if \code{value} is an
#'   \code{\link{WMDS}} object. The lengths of both objects must match. If
#'   \code{value} is a \code{\link{WMD}} object, its metadata entries will be
#'   recycled. \item If \code{object} is of class \code{\link{WMD}},
#'   \code{value} cannot be of class \code{\link{WMD}}.
#'   }
#' @return \code{value}.
#' @details This method can easily be used to copy (selected parts of) the
#'   \code{\link{csv_data}} to the metadata; see there for details.
#'
#'   \code{\link{map_metadata}} can also be used to modify metadata but it will
#'   return a novel object. See \code{\link{edit}} for manually modifying
#'   metadata.
#' @export
#' @exportMethod "metadata<-"
#' @family metadata-functions
#' @keywords manip
#' @examples
#'
#' ## WMD methods
#'
#' # WMD/missing/list method
#' copy <- vaas_1
#' new.md <- list(Species = "Thermomicrobium roseum")
#' metadata(copy) <- new.md
#' stopifnot(identical(metadata(copy), new.md))
#'
#' # WMD/missing/formula method (operates on previous entries!)
#' copy <- vaas_1
#' metadata(copy) <- Organism ~ paste(Species, Strain)
#' (x <- metadata(copy, "Organism"))
#' stopifnot(is.null(metadata(vaas_1, "Organism")), !is.null(x))
#'
#' # WMD/numeric/list method
#' copy <- vaas_1
#' metadata(copy, 1) <- list(Authors = "Vaas et al.")
#' stopifnot(length(metadata(copy)) > length(metadata(vaas_1)))
#'
#' # WMD/list/list method
#' copy <- vaas_1
#' stopifnot(identical(metadata(copy, "Species"), "Escherichia coli"))
#'
#' # You can use this to translate the keys on-the-fly...
#' metadata(copy, list(Organism = "Species")) <- list(
#'   Organism = "Bacillus subtilis")
#' stopifnot(length(metadata(copy)) == length(metadata(vaas_1)))
#' stopifnot(identical(metadata(copy, "Species"), "Bacillus subtilis"))
#' stopifnot(is.null(metadata(copy, "Organism"))) # this was not set!
#'
#' # ...but you need not
#' metadata(copy, list("Species")) <- list(Species = "Yersinia pestis")
#' stopifnot(length(metadata(copy)) == length(metadata(vaas_1)))
#' stopifnot(identical(metadata(copy, "Species"), "Yersinia pestis"))
#'
#' # Names need not be duplicated
#' metadata(copy, list("Species")) <- list("Gen. sp.")
#' stopifnot(length(metadata(copy)) == length(metadata(vaas_1)))
#' stopifnot(identical(metadata(copy, "Species"), "Gen. sp."))
#'
#' # ...but this would delete the entry because nothing would be found in
#' # 'value'
#' metadata(copy, list("Species")) <- list(Organism = "E. coli")
#' stopifnot(length(metadata(copy)) < length(metadata(vaas_1)))
#' stopifnot(is.null(metadata(copy, "Species")))
#'
#' # ...this yields a general mechanism for metadata deletion by providing an
#' # empty list as 'value'.
#'
#' # WMD/character/any method
#' copy <- vaas_1
#' metadata(copy, "Strain") <- "08/15"
#' stopifnot(length(metadata(copy)) == length(metadata(vaas_1)))
#' stopifnot(metadata(copy, "Strain") != metadata(vaas_1, "Strain"))
#'
#' # WMD/factor/any method
#' metadata(copy, as.factor("Strain")) <- metadata(vaas_1, "Strain")
#' stopifnot(metadata(copy, "Strain") == metadata(vaas_1, "Strain"))
#'
#' ## WMDS methods
#'
#' # WMDS/missing/list method
#' copy <- vaas_4
#' (metadata(copy) <- list(x = -99)) # will replace all of them
#' stopifnot(identical(unique(metadata(copy)), list(list(x = -99))))
#' metadata(copy[2]) <- list(x = 1) # will replace those of 2nd plate
#' stopifnot(identical(unique(metadata(copy)),
#'   list(list(x = -99), list(x = 1))))
#'
#' # WMDS/missing/WMD method
#' (metadata(copy) <- vaas_1) # will also replace all of them
#' stopifnot(identical(unique(metadata(copy)), list(metadata(vaas_1))))
#'
#' # WMDS/missing/formula method
#' copy <- vaas_4
#' metadata(copy) <- Organism ~ paste(Species, Strain)
#' (x <- metadata(copy, "Organism"))
#' stopifnot(length(x) == length(metadata(vaas_4, "Organism")) + 4)
#'
#' # WMDS/ANY/ANY method
#' copy <- vaas_4
#' (metadata(copy, "Species") <- "Bacillus subtilis") # will set all of them
#' stopifnot(identical(unique(metadata(copy, "Species")), "Bacillus subtilis"))
#' stopifnot(!identical(metadata(copy), metadata(vaas_4)))
#' metadata(copy) <- vaas_4 # reset
#' metadata(copy)
#' stopifnot(identical(metadata(copy), metadata(vaas_4)))
#' (metadata(copy) <- vaas_1) # set everything to metadata of vaas_1
#' stopifnot(identical(unique(metadata(copy)), list(metadata(vaas_1))))
#'
#' # WMDS/character/data frame method
#' copy <- vaas_4
#' (x <- data.frame(Type = grepl("T$", metadata(vaas_4, "Strain"))))
#' metadata(copy, "Type") <- x
#' # one-column data frames are simplified
#' stopifnot(identical(metadata(copy, "Type"), x$Type))
#' # if keys match, a partial selection of the data frame is used
#' (x <- cbind(x, Notype = !x$Type))
#' metadata(copy, "Type") <- x
#' stopifnot(identical(metadata(copy, "Type"), x$Type))
#' # if keys do not match, the entire data-frame rows are included
#' metadata(copy, "Type2") <- x
#' stopifnot(!identical(metadata(copy, "Type2"), x$Type))
#'
#' # WMDS/missing/character method: setting unique IDs
#' metadata(copy) <- opm_opt("md.id.name") # set IDs
#' metadata(copy, opm_opt("md.id.name")) # get these IDs
#' stopifnot(is.integer(metadata(copy, opm_opt("md.id.name"))))
#' # to reset the start point to the number n, use opm_opt(md.id.start = n)
#'
#' # WMDS/missing/logical method: storing or deleting csv_data() entries
#' copy <- vaas_4
#' metadata(copy) <- TRUE # store them
#' stopifnot(ncol(to_metadata(copy)) > ncol(to_metadata(vaas_4)))
#' metadata(copy) <- FALSE # remove them again
#' stopifnot(identical(metadata(copy), metadata(vaas_4)))
#'
setGeneric("metadata<-",
  function(object, key, ..., value) standardGeneric("metadata<-"))

#-------------------------------------------------------------------------------
# 1:1 relationship with the WMDS methods

#' @name metadata.set
#'
setMethod("metadata<-", c("WMD", "missing", "FOE"), function(object, key,
    value) {
  object@metadata <- map_values(object@metadata, value)
  object
}, sealed = SEALED)
#' @name metadata.set
#'
setMethod("metadata<-", c("WMD", "missing", "list"), function(object, key,
    value) {
  object@metadata <- value
  object
}, sealed = SEALED)

#' @name metadata.set
#'
setMethod("metadata<-", c("WMD", "missing", "data.frame"), function(object,
    key, value) {
  if (nrow(value) != 1L)
    stop("need data frame with one row")
  object@metadata <- as.list.data.frame(value)
  object
}, sealed = SEALED)

#' @name metadata.set
#'
setMethod("metadata<-", c("WMD", "missing", "WMD"), function(object, key,
    value) {
  object@metadata <- value@metadata
  object
}, sealed = SEALED)

#' @name metadata.set
#'
setMethod("metadata<-", c("WMD", "missing", "WMDS"), function(object, key,
    value) {
  stop("lengths of 'object' and 'value' do not fit")
}, sealed = SEALED)

#' @name metadata.set
#'
setMethod("metadata<-", c("WMD", "missing", "character"), function(object, key,
    value) {
  if (found <- match(opm_opt("md.id.name"), value, 0L)) {
    object@metadata[[value[[found]]]] <- id <- opm_opt("md.id.start")
    OPM_OPTIONS$md.id.start <- id + 1L
    value <- value[!found]
  }
  if (length(value))
    stop("value '", value[[1L]], "' not understood")
  object
}, sealed = SEALED)

#' @name metadata.set
#'
setMethod("metadata<-", c("WMD", "missing", "logical"), function(object, key,
    value) {
  if (L(value))
    for (key in setdiff(names(object@csv_data), opm_opt("csv.selection")))
      object@metadata[[key]] <- object@csv_data[[key]]
  else
    for (key in setdiff(names(object@csv_data), opm_opt("csv.selection")))
      object@metadata[[key]] <- NULL
  object
}, sealed = SEALED)

#-------------------------------------------------------------------------------
# the data-frame behaviour deliberately deviates from other key values

#' @name metadata.set
#'
setMethod("metadata<-", c("WMD", "character", "ANY"), function(object, key,
    value) {
  object@metadata[[key]] <- value
  object
}, sealed = SEALED)

#' @name metadata.set
#'
setMethod("metadata<-", c("WMD", "character", "data.frame"), function(object,
    key, value) {
  if (nrow(value) != 1L)
    stop("need data frame with one row")
  if (any(found <- key %in% colnames(value))) {
    j <- key[found <- which(found)[1L]]
    key <- key[seq_len(found)] # keys before the 1st => nested indexing
  } else
    j <- TRUE
  object@metadata[[key]] <- value[1L, j, drop = TRUE]
  object
}, sealed = SEALED)

#' @name metadata.set
#'
setMethod("metadata<-", c("WMD", "character", "WMD"), function(object, key,
    value) {
  object@metadata[[key]] <- value@metadata
  object
}, sealed = SEALED)

#' @name metadata.set
#'
setMethod("metadata<-", c("WMD", "character", "WMDS"), function(object, key,
    value) {
  stop("lengths of 'object' and 'value' do not fit")
}, sealed = SEALED)

#-------------------------------------------------------------------------------
# must repeat method for data frames to avoid ambiguity in dispatch

#' @name metadata.set
#'
setMethod("metadata<-", c("WMD", "numeric", "list"), function(object, key,
    value) {
  object@metadata <- if (L(key) > 0)
    c(value, object@metadata)
  else if (key < 0)
    c(object@metadata, value)
  else
    value
  object
}, sealed = SEALED)

#' @name metadata.set
#'
setMethod("metadata<-", c("WMD", "numeric", "data.frame"), function(object, key,
    value) {
  if (nrow(value) != 1L)
    stop("need data frame with one row")
  metadata(object, key) <- as.list.data.frame(value)
  object
}, sealed = SEALED)

#' @name metadata.set
#'
setMethod("metadata<-", c("WMD", "numeric", "WMD"), function(object, key,
    value) {
  metadata(object, key) <- value@metadata
  object
}, sealed = SEALED)

#' @name metadata.set
#'
setMethod("metadata<-", c("WMD", "numeric", "WMDS"), function(object, key,
    value) {
  stop("lengths of 'object' and 'value' do not fit")
}, sealed = SEALED)

#-------------------------------------------------------------------------------
# must repeat method for data frames to avoid ambiguity in dispatch

#' @name metadata.set
#'
setMethod("metadata<-", c("WMD", "list", "list"), function(object, key, value) {
  if (is.null(names(key)))
    names(key) <- unlist(key, TRUE, FALSE)
  if (is.null(names(value)))
    names(value) <- names(key)
  for (k in names(key))
    object@metadata[[key[[k]]]] <- value[[k]]
  object
}, sealed = SEALED)

#' @name metadata.set
#'
setMethod("metadata<-", c("WMD", "list", "data.frame"), function(object, key,
    value) {
  if (nrow(value) != 1L)
    stop("need data frame with one row")
  metadata(object, key) <- as.list.data.frame(value)
  object
}, sealed = SEALED)

#' @name metadata.set
#'
setMethod("metadata<-", c("WMD", "list", "WMD"), function(object, key,
    value) {
  metadata(object, key) <- value@metadata
  object
}, sealed = SEALED)

#' @name metadata.set
#'
setMethod("metadata<-", c("WMD", "list", "WMDS"), function(object, key, value) {
  stop("lengths of 'object' and 'value' do not fit")
}, sealed = SEALED)

#-------------------------------------------------------------------------------

#' @name metadata.set
#'
setMethod("metadata<-", c("WMD", "ANY", "ANY"), function(object, key,
    value) {
  metadata(object, as.character(key)) <- value
  object
}, sealed = SEALED)

#-------------------------------------------------------------------------------
# 1:1 relationship with the WMD methods

#' @name metadata.set
#'
setMethod("metadata<-", c("WMDS", "missing", "FOE"), function(object, key,
    value) {
  for (i in seq_along(object@plates))
    object@plates[[i]]@metadata <- map_values(object@plates[[i]]@metadata,
      value)
  object
}, sealed = SEALED)

#' @name metadata.set
#'
setMethod("metadata<-", c("WMDS", "missing", "list"), function(object, key,
    value) {
  for (i in seq_along(object@plates))
    object@plates[[i]]@metadata <- value
  object
}, sealed = SEALED)

#' @name metadata.set
#'
setMethod("metadata<-", c("WMDS", "missing", "data.frame"), function(object,
    key, value) {
  LL(object, .wanted = nrow(value))
  for (i in seq_along(object@plates))
    object@plates[[i]]@metadata <- as.list.data.frame(value[i, , drop = FALSE])
  object
}, sealed = SEALED)

#' @name metadata.set
#'
setMethod("metadata<-", c("WMDS", "missing", "WMD"), function(object, key,
    value) {
  for (i in seq_along(object@plates))
    metadata(object@plates[[i]]) <- value@metadata
  object
}, sealed = SEALED)

#' @name metadata.set
#'
setMethod("metadata<-", c("WMDS", "missing", "WMDS"), function(object, key,
    value) {
  LL(object, .wanted = length(value))
  for (i in seq_along(object@plates))
    object@plates[[i]]@metadata <- value@plates[[i]]@metadata
  object
}, sealed = SEALED)

#' @name metadata.set
#'
setMethod("metadata<-", c("WMDS", "missing", "character"), function(object, key,
    value) {
  if (found <- match(opm_opt("md.id.name"), value, 0L)) {
    key <- value[[found]]
    this <- opm_opt("md.id.start")
    for (i in seq_along(object@plates)) {
      object@plates[[i]]@metadata[[key]] <- this
      this <- this + 1L
    }
    OPM_OPTIONS$md.id.start <- this
    value <- value[!found]
  }
  if (length(value))
    stop("value '", value[[1L]], "' not understood")
  object
}, sealed = SEALED)

#' @name metadata.set
#'
setMethod("metadata<-", c("WMDS", "missing", "logical"), function(object, key,
    value) {
  for (i in seq_along(object@plates))
    metadata(object@plates[[i]]) <- value
  object
}, sealed = SEALED)

#-------------------------------------------------------------------------------

#' @name metadata.set
#'
setMethod("metadata<-", c("WMDS", "character", "WMDS"), function(object, key,
    value) {
  LL(object, .wanted = length(value))
  for (i in seq_along(object@plates))
    object@plates[[i]]@metadata[[key]] <- value@plates[[i]]@metadata
  object
}, sealed = SEALED)

#' @name metadata.set
#'
setMethod("metadata<-", c("WMDS", "character", "data.frame"), function(object,
    key, value) {
  LL(object, .wanted = nrow(value))
  if (any(found <- key %in% colnames(value))) {
    j <- key[found <- which(found)[1L]]
    key <- key[seq_len(found)]
  } else
    j <- TRUE
  for (i in seq_along(object@plates))
    object@plates[[i]]@metadata[[key]] <- value[i, j, drop = TRUE]
  object
}, sealed = SEALED)

#-------------------------------------------------------------------------------

#' @name metadata.set
#'
setMethod("metadata<-", c("WMDS", "ANY", "data.frame"), function(object, key,
    value) {
  LL(object, .wanted = nrow(value))
  for (i in seq_along(object@plates))
    metadata(object@plates[[i]], key) <- value[i, , drop = FALSE]
  object
}, sealed = SEALED)

#' @name metadata.set
#'
setMethod("metadata<-", c("WMDS", "ANY", "WMD"), function(object, key, value) {
  for (i in seq_along(object@plates))
    metadata(object@plates[[i]], key) <- value@metadata
  object
}, sealed = SEALED)

#' @name metadata.set
#'
setMethod("metadata<-", c("WMDS", "ANY", "WMDS"), function(object, key, value) {
  LL(object, .wanted = length(value))
  for (i in seq_along(object@plates))
    metadata(object@plates[[i]], key) <- value@plates[[i]]@metadata
  object
}, sealed = SEALED)

#' @name metadata.set
#'
setMethod("metadata<-", c("WMDS", "ANY", "ANY"), function(object, key, value) {
  for (i in seq_along(object@plates))
    metadata(object@plates[[i]], key) <- value
  object
}, sealed = SEALED)

#-------------------------------------------------------------------------------

#' @name metadata.set
#'
setMethod("metadata<-", c("MOPMX", "missing", "ANY"), function(object, key,
    value) {
  for (i in seq_along(object@.Data))
    metadata(object@.Data[[i]]) <- value
  object
}, sealed = SEALED)

#' @name metadata.set
#'
setMethod("metadata<-", c("MOPMX", "ANY", "ANY"), function(object, key,
    value) {
  for (i in seq_along(object@.Data))
    metadata(object@.Data[[i]], key) <- value
  object
}, sealed = SEALED)

#' @name metadata.set
#'
setMethod("metadata<-", c("MOPMX", "missing", "data.frame"), function(object,
    key, value) {
  indexes <- sub_indexes(object)
  if (nrow(value) != attr(indexes, "total"))
    stop("number of rows in 'value' unequal to number of plates in 'object'")
  for (i in seq_along(object@.Data))
    metadata(object@.Data[[i]]) <- value[indexes[[i]], , drop = FALSE]
  object
}, sealed = SEALED)

#' @name metadata.set
#'
setMethod("metadata<-", c("MOPMX", "missing", "character"), function(object,
    key, value) {
  for (i in seq_along(object@.Data))
    metadata(object@.Data[[i]]) <- value
  object
}, sealed = SEALED)

#' @name metadata.set
#'
setMethod("metadata<-", c("MOPMX", "missing", "logical"), function(object, key,
    value) {
  for (i in seq_along(object@.Data))
    metadata(object@.Data[[i]]) <- value
  object
}, sealed = SEALED)

#' @name metadata.set
#'
setMethod("metadata<-", c("MOPMX", "ANY", "data.frame"), function(object, key,
    value) {
  indexes <- sub_indexes(object)
  if (nrow(value) != attr(indexes, "total"))
    stop("number of rows in 'value' unequal to number of plates in 'object'")
  for (i in seq_along(object@.Data))
    metadata(object@.Data[[i]], key) <- value[indexes[[i]], , drop = FALSE]
  object
}, sealed = SEALED)


################################################################################


#' Add or map metadata or edit them by hand
#'
#' Either include metadata by mapping \acronym{CSV} data and column names in a
#' data frame (optionally read from file), or modify meta-information stored
#' together with the measurements by using a function or other kinds of mappings
#' and return the objects otherwise unchanged, or invoke \code{edit} from the
#' \pkg{utils} package for editing the metadata by hand.
#'
#' @param object \code{\link{OPM}} (\code{\link{WMD}}), \code{\link{OPMS}}
#'   (\code{\link{WMDS}}) or \code{\link{MOPMX}} object. For \code{map_values},
#'   a list.
#' @param name Like \code{object}, but for the \code{edit} method.
#' @param md Data frame containing keys as column names, or name of file from
#'   which to read the data frame. Handled by \code{\link{to_metadata}}. In the
#'   case of many plates, it is computationally more efficient to provide a data
#'   frame instead of a file name.
#' @param keys Character vector. Corresponds to the \code{selection} argument
#'   of \code{\link{collect_template}}.
#' @param replace Logical scalar indicating whether the previous metadata, if
#'   any, shall be replaced by the novel ones, or whether these shall be
#'   appended.
#' @param skip.failure Logical scalar. Do not stop with an error message if
#'   (unambiguous) selection is impossible but raise a warning only?
#' @param remove.keys Logical scalar. When including \code{md} in the metadata,
#'   discard the \code{keys} columns?
#' @param normalize Passed to \code{\link{csv_data}}. The same value must be
#'   chosen for subsequent calls of \code{\link{collect_template}} and
#'   \code{include_metadata}.
#' @param mapping In most cases passed to \code{map_values}. \itemize{
#'   \item If a function, this is just a wrapper for \code{rapply}, with
#'   \code{how} set to \sQuote{replace}, if \code{values} is \code{TRUE}. It is
#'   applied to all non-list elements of \code{\link{metadata}}, which is
#'   traversed recursively.
#'   \item Alternatively, a character vector. \code{\link{metadata_chars}} can
#'   be used to create a template for such a vector.
#'   \item \code{mapping} can also be a formula; in that case,
#'   \code{\link{metadata}} is replaced by the according  method of
#'   \code{map_values}. If the left side of the formula is missing, the
#'   entire metadata are replaced by the result, which is an error if the result
#'   is not a list.
#'   \item If \code{mapping} is missing, the behaviour is special; see the next
#'   two arguments.
#' }
#' The \pkg{opm} package augments \code{map_values} with a method for lists and
#' formulae. For all other methods, see the \pkg{pkgutils} package.
#'
#' @param values Mostly a logical scalar. \itemize{
#'   \item For the function and character-vector methods, if \code{FALSE},
#'   metadata names, not values, are mapped, and \code{classes} is ignored
#'   (names are always of class \sQuote{character}).
#'   \item For the formula method, \code{values} is the enclosing environment
#'   used.
#'   \item If \code{mapping} is missing, setting \code{values} to \code{TRUE}
#'   causes all non-list entries that only comprise \code{NA} values to be
#'   removed.
#'   }
#' @param classes Character vector or (for the character vector-based mapping)
#'   \code{TRUE}. For the mapping with a function or vector, this specifies the
#'   classes in addition to \sQuote{character} that are mapped (after converting
#'   to \sQuote{character} mode).
#'
#'   If \code{classes} is \code{TRUE}, \code{mapping} is treated as a mapping
#'   between class names, and the according conversions are applied. See the
#'   \code{coerce} argument of \code{map_values} for details.
#'
#'   If \code{mapping} is missing, \code{classes} specifies classes that are
#'   converted to character vectors.
#'
#' @param coerce Character vector or \code{TRUE}. See the description of
#'   \code{map_values} in the \pkg{pkgutils} package for details.
#' @param ... Optional arguments passed to \code{mapping} if it is a function,
#'   and from the \code{\link{WMDS}} method to the \code{\link{WMD}} method, or
#'   from \code{include_metadata} to \code{\link{to_metadata}}, or as additional
#'   arguments to \code{edit} from the \pkg{utils} package.
#' @export
#' @return Novel \code{\link{WMD}} or \code{\link{WMDS}} object with modified
#'   metadata.
#' @details
#' The \code{\link{WMDS}} method applies the inclusion and mapping routines to
#' all plates in turn and returns an \code{\link{WMDS}} object with accordingly
#' modified metadata.
#'
#' Three kinds of errors can occur when attempting to identify a data-frame row
#' using the given combination of keys and values. \itemize{
#'   \item{The combination results in more than a single row.}
#'   \item{The combination results in now rows at all.}
#'   \item{The keys are not found at all.}
#' } In the first two cases study the error message in detail. It contains the
#' failed values enclosed in single quotes, doubling all contained single
#' quotes, if any. This eases recognising leading and trailing spaces, which
#' used to be a frequent cause of mismatches between data-frame fields and
#' \code{\link{csv_data}} within \code{\link{WMD}} objects.
#'
#' The third kind of error is usually caused by a wrong column separator
#' (\code{sep}) argument. This happens particularly if a spreadsheet software
#' saves the file with a separator distinct from the one used in the input. The
#' \code{to_metadata} method for file names by default tries several \code{sep}
#' values in turn as a remedy.
#'
#' Leading and trailing spaces can be lost when writing \acronym{CSV} files and
#' inputting them again. By default both \code{\link{collect_template}} and
#' \code{include_metadata} attempt to avoid this by replacing all spaces with
#' underscores. Make sure you use the same \code{normalize} argument with both
#' functions. If you use a non-default \code{normalize} argument, consider the
#' \code{strip.white} argument.
#'
#' A further potential cause of mismatches is the reformatting of setup time
#' entries by spreadsheet software. By default both \code{include_metadata} and
#' \code{\link{collect_template}} attempt to avoid this by replacing all spaces
#' with underscores. If this does not help, prevent the reformatting by forcing
#' that software to treat the setup time as character strings.
#'
#' If \code{md} is a file name, the default settings for \code{sep} and
#' \code{strip.white} try to avoid these errors by trying several values in
#' turn.
#'
#' Calling \code{edit} will only work if \code{\link{to_metadata}} yields a data
#' frame suitable for the \code{edit} method from the \pkg{utils} package. This
#' usually means that the \code{\link{metadata}} must be rectangular, even
#' though this is not enforced by the implementation of the \code{\link{OPMX}}
#' classes. Entries missing in some elements of \code{name} should not present a
#' problem, however. Values that remained \code{NA} would  be removed before
#' returning the result. Rows additionally included in the temporary data frame
#' during editing yielded an error. The \code{\link{MOPMX}} method works by
#' calling each element in turn (allowing for independent editing).
#'
#' @family metadata-functions
#' @seealso utils::edit
#' @keywords manip
#' @examples
#'
#' ## include_metadata()
#'
#' (x <- collect_template(vaas_1, add.cols = "Location")) # generate data frame
#' x[1, "Location"] <- "Braunschweig" # insert additional information
#' copy <- include_metadata(vaas_1, x) # include the data in new OPM object
#' stopifnot(is.null(metadata(vaas_1, "Location")))
#' stopifnot(identical(metadata(copy, "Location"), "Braunschweig"))
#'
#' ## map_metadata()
#'
#' # WMD methods
#'
#' # WMD+function method
#' copy <- map_metadata(vaas_1, identity)
#' stopifnot(identical(copy, vaas_1))
#' copy <- map_metadata(vaas_1, identity, values = FALSE)
#' stopifnot(identical(copy, vaas_1))
#' copy <- map_metadata(vaas_1, function(x) paste(x, "!"), values = FALSE)
#' (x <- metadata_chars(vaas_1, values = FALSE))
#' (y <- metadata_chars(copy, values = FALSE))
#' stopifnot(identical(as.character(y), paste(x, "!")))
#'
#' # WMD+character method: mapping a value
#' map <- metadata_chars(vaas_1)
#' map["First replicate"] <- "Rep. 1"
#' copy <- map_metadata(vaas_1, map)
#' stopifnot(identical(names(metadata(copy)), names(metadata(vaas_1))))
#' stopifnot(!identical(metadata(copy, "Experiment"),
#'   metadata(vaas_1, "Experiment")))
#'
#' # WMD+character method: mapping a name
#' map <- metadata_chars(vaas_1, values = FALSE)
#' map["Plate number"] <- "Plate no."
#' copy <- map_metadata(vaas_1, map, values = FALSE)
#' stopifnot(!identical(names(metadata(copy)), names(metadata(vaas_1))))
#'
#' # WMD+formula method
#' copy <- map_metadata(vaas_1, Organism ~ paste(Species, Strain))
#' (x <- setdiff(metadata_chars(copy), metadata_chars(vaas_1)))
#' stopifnot(length(x) == 1, x == "Escherichia coli DSM30083T")
#' stopifnot(identical(copy, # same result with expression
#'   map_metadata(vaas_1, expression(Organism <- paste(Species, Strain)))))
#'
#' # WMD+missing method
#' (x <- metadata(map_metadata(vaas_1)))
#' stopifnot(identical(x, metadata(vaas_1))) # nothing to modify in that case
#'
#' # WMDS method
#'
#' # using a function
#' copy <- map_metadata(vaas_4, identity)
#' stopifnot(identical(copy, vaas_4))
#' copy <- map_metadata(vaas_4, identity, values = FALSE)
#' stopifnot(identical(copy, vaas_4))
#' copy <- map_metadata(vaas_4, function(x) paste(x, "!"), values = FALSE)
#' (x <- metadata_chars(vaas_4, values = FALSE))
#' (y <- metadata_chars(copy, values = FALSE))
#' stopifnot(identical(as.character(y), paste(x, "!")))
#'
#' # using a character vector
#' map <- metadata_chars(vaas_4)
#' map["First replicate"] <- "Rep. 1"
#' copy <- map_metadata(vaas_4, map)
#' x <- metadata(vaas_4, "Experiment")
#' stopifnot(x == "First replicate")
#' y <- metadata(copy, "Experiment")
#' stopifnot(y == "Rep. 1")
#'
#' # using a formula
#' copy <- map_metadata(vaas_4, Organism ~ paste(Species, Strain))
#' (x <- setdiff(metadata_chars(copy), metadata_chars(vaas_4)))
#' stopifnot(length(x) == 4) # one entry per plate
#'
#' # 'mapping' missing
#' (x <- metadata(map_metadata(vaas_4)))
#' stopifnot(identical(x, metadata(vaas_4))) # nothing to modify in that case
#'
#' \dontrun{ ## edit metadata by hand
#'   x <- edit(vaas_4) # this would create a new object
#'   x <- edit(x) # overwrite x in 2nd editing step
#'   ## This will not necessarily work as intended if the metadata are nested!
#'   ## Moreover, additionally inserted rows would cause an error.
#' }
#'
#' ## List/formula method of map_values()
#' x <- list(a = 1:8, c = 9, d = 'x')
#' (y <- map_values(x, ~ a + c))
#' stopifnot(is.numeric(y), y == c(10:17))
#' (y <- map_values(x, b ~ a + c))
#' stopifnot(is.list(y), y$b == c(10:17))
#'
#' # ...applied to a data frame
#' x <- data.frame(a = 1:5, b = 6:10)
#' (y <- map_values(x, c ~ a + b))
#' stopifnot(is.data.frame(y), dim(y) == c(5, 3))
#' (z <- map_values(x, ~ a + b))
#' stopifnot(identical(z, y$c))
#' # same effect with an expression
#' (z <- map_values(x, expression(c <- a + b)))
#' stopifnot(identical(z, y))
#'
setGeneric("include_metadata",
  function(object, ...) standardGeneric("include_metadata"))

setMethod("include_metadata", "WMD", function(object, md, keys, replace = FALSE,
    skip.failure = FALSE, remove.keys = TRUE, normalize = -1L, ...) {

  pick_from <- function(object, selection) {
    matches <- lapply(names(selection), FUN = function(name) {
      m <- lapply(selection[[name]], `==`, y = object[, name])
      apply(do.call(cbind, m), 1L, any)
    })
    matches <- apply(do.call(cbind, matches), 1L, all)
    matches[is.na(matches)] <- FALSE # we get NA from all-NA rows
    object[matches, , drop = FALSE]
  }

  LL(replace, skip.failure, remove.keys)

  selection <- csv_data(object = object, keys = keys, normalize = normalize)
  selection <- as.list(selection)

  # Get and check metadata.
  md <- to_metadata(md, ...)
  if (length(absent.keys <- setdiff(keys, colnames(md))))
    stop("key missing in 'metadata': ", absent.keys[1L])

  # Try to select the necessary information from the metadata.
  found <- pick_from(md, selection)
  msg <- case(nrow(found), listing(lapply(selection, safe_labels, "nexus"),
      header = "could not find this key/value combination in 'metadata':"),
    NULL, listing(lapply(selection, safe_labels, "nexus"),
      header = "the selection resulted in more than one row for:"))

  # Failures.
  if (length(msg))
    if (skip.failure) {
      warning(msg)
      return(object)
    } else {
      stop(msg)
    }

  # Success.
  wanted <- colnames(found)
  if (remove.keys)
    wanted <- setdiff(wanted, keys)
  found <- as.list(found[, wanted, drop = FALSE])
  result <- object
  result@metadata <- if (replace)
      found
    else
      c(metadata(result), found)

  result

}, sealed = SEALED)

setMethod("include_metadata", "OPM", function(object, md,
    keys = opm_opt("csv.keys"), ...) {
  callNextMethod(object = object, md = md, keys = keys, ...)
}, sealed = SEALED)

setMethod("include_metadata", "WMDS", function(object, ...) {
  object@plates <- lapply(X = object@plates, FUN = include_metadata, ...)
  object
}, sealed = SEALED)

setMethod("include_metadata", "MOPMX", function(object, ...) {
  object@.Data <- lapply(X = object@.Data, FUN = include_metadata, ...)
  object
}, sealed = SEALED)

#= map_metadata include_metadata

#' @rdname include_metadata
#' @export
#'
setGeneric("map_metadata",
  function(object, mapping, ...) standardGeneric("map_metadata"))

setMethod("map_metadata", c("WMD", "function"), function(object, mapping,
    values = TRUE, classes = "ANY", ...) {
  object@metadata <- if (L(values))
      map_values(object = object@metadata, mapping = mapping,
        coerce = classes, ...)
    else
      map_names(object = object@metadata, mapping = mapping, ...)
  object
}, sealed = SEALED)

setMethod("map_metadata", c("WMD", "character"), function(object, mapping,
    values = TRUE, classes = "factor") {
  object@metadata <- if (L(values))
      map_values(object@metadata, mapping, coerce = classes)
    else
      map_names(object@metadata, mapping)
  object
}, sealed = SEALED)

setMethod("map_metadata", c("WMD", "FOE"), function(object, mapping,
    values = parent.frame(), classes = NULL) {
  object@metadata <- map_values(object@metadata, mapping, values)
  object
}, sealed = SEALED)

setMethod("map_metadata", c("WMD", "missing"), function(object, mapping,
    values = TRUE, classes = "factor") {
  if (L(values))
    object@metadata <- rapply(object@metadata, function(x) if (all(is.na(x)))
        NULL
      else
        x, "ANY", NULL, "replace")
  object@metadata <- map_values(object@metadata, NULL, classes)
  object
}, sealed = SEALED)

setMethod("map_metadata", c("WMDS", "missing"), function(object, mapping,
    values = TRUE, classes = "factor") {
  object@plates <- lapply(X = object@plates, FUN = map_metadata,
    values = values, classes = classes)
  object
}, sealed = SEALED)

setMethod("map_metadata", c("WMDS", "ANY"), function(object, mapping, ...) {
  object@plates <- lapply(X = object@plates, FUN = map_metadata,
    mapping = mapping, ...)
  object
}, sealed = SEALED)

setMethod("map_metadata", c("MOPMX", "missing"), function(object, mapping,
    values = TRUE, classes = "factor") {
  object@.Data <- lapply(X = object@.Data, FUN = map_metadata,
    values = values, classes = classes)
  object
}, sealed = SEALED)

setMethod("map_metadata", c("MOPMX", "ANY"), function(object, mapping, ...) {
  object@.Data <- lapply(X = object@.Data, FUN = map_metadata,
    mapping = mapping, ...)
  object
}, sealed = SEALED)

#= map_values include_metadata

#' @rdname include_metadata
#' @export
#'
setGeneric("map_values")

setMethod("map_values", c("list", "formula"), function(object, mapping,
    coerce = parent.frame()) {
  if (length(mapping) < 3L)
    return(eval(mapping[[2L]], object, coerce))
  right <- eval(mapping[[3L]], object, coerce)
  left <- metadata_key.formula(mapping[-3L], FALSE, envir = coerce)
  if (is.list(left)) {
    right <- rep(right, length.out = length(left))
    for (i in seq_along(left))
      object[[left[[i]]]] <- right[[i]]
  } else {
    object[[left]] <- right
  }
  object
}, sealed = SEALED)

#= edit include_metadata

#' @rdname include_metadata
#' @export
#'
setGeneric("edit")

setMethod("edit", "WMDX", function(name, ...) {
  metadata(name) <- edit(to_metadata(name), ...)
  map_metadata(name)
}, sealed = SEALED)

setMethod("edit", "MOPMX", function(name, ...) {
  for (i in seq_along(name))
    name[[i]] <- edit(name[[i]], ...)
  name
}, sealed = SEALED)


################################################################################
################################################################################
#
# Getter functions for metadata
#


#' Get metadata
#'
#' Get meta-information stored together with the data or collect all
#' \sQuote{character} entries from the meta-information stored together with the
#' measurements. Optionally coerce data of other types.
#'
#' @param object \code{\link{WMD}}, \code{\link{WMDS}} or \code{\link{MOPMX}}
#'   object.
#' @param key \code{NULL}, vector, factor or formula. \itemize{
#'   \item If \code{NULL} or otherwise empty, return all metadata.
#'   \item If a non-empty list, treated as list of keys. Return value would be
#'   the list of corresponding metadata values. Here, character vectors of
#'   length > 1 can be used to query nested metadata lists.
#'   \item If neither empty nor a list nor a formula (i.e. usually a character
#'   or numeric vector), \code{key} is treated as a single list key. Factors are
#'   converted to \sQuote{character} mode.
#'   \item Formulae can also be used and are converted to a list or character or
#'   numeric vector using the rules described under \sQuote{Details}.
#'   \item It is in general not recommended to use numeric vectors as \code{key}
#'   arguments, either directly or within a list or formula.
#' }
#' @param exact Logical scalar. Use exact or partial matching of keys? Has no
#'   effect if \code{key} is empty.
#' @param strict Logical scalar. Is it an error if a \code{NULL} value results
#'   from fetching a metadata key?
#' @param values Logical scalar. If \code{FALSE}, metadata names, not values,
#'   are collected, and \code{classes} is ignored (names are always of class
#'   \sQuote{character} and need not be coerced).
#' @param classes Character vector containing the names of classes that should
#'   also be collected (and coerced to \sQuote{character}), or \code{TRUE}. In
#'   that case, a mapping template for the classes themselves is returned. See
#'   the \code{coerce} argument of \code{map_values} for details.
#' @param max.dist Numeric scalar. If non-negative, causes the construction of a
#'   mapping from potential misspellings to the putative correct spelling, based
#'   on string similarity. \code{max.dist} then gives the maximum string
#'   distance allowed for regarding two strings as synonyms. This boundary must
#'   not be set too high; otherwise strings with distinct meanings might be
#'   regarded as misspellings (see the example below). The resulting vector
#'   should always be inspected before passing it to \code{\link{map_values}}.
#'   See the \code{map_values} method for character vectors as \code{object} and
#'   numeric vectors as \code{mapping} argument in the \pkg{pkgutils} package
#'   for further details on such string matching.
#' @param ... Optional arguments passed between the methods or to
#'   \code{\link{map_values}}.
#'
#' @return \code{metadata} generates a list (empty if metadata were not set or
#'   if partial selection using \code{key} did not result).
#'
#'   Under default settings \code{metadata_chars} yields a character vector,
#'   sorted and made unique. Original \code{names} attributes, if any, are
#'   dropped and replaced by the character vector itself. (This might be
#'   convenient regarding its use with \code{\link{map_metadata}}.) If
#'   \code{max.distance} is non-negative, the result is distinct; see above for
#'   details.
#' @export
#' @family metadata-functions
#' @keywords attribute
#' @details If a named list is used as \code{key} argument, its names will be
#'   used within the first level of the resulting nested or non-nested list.
#'   That is, \code{key} can be used to translate names on the fly, and this can
#'   be used by all functions that call \code{metadata} indirectly, usually via
#'   an \code{as.labels} or \code{as.groups} argument.
#'
#'   Even though it is not technically impossible, it is usually a bad idea to
#'   select metadata entries using numeric (positional) or logical keys. The
#'   problem is that, in contrast to, e.g., data frames, their is no guarantee
#'   that metadata entries with the same name occur in the same position, even
#'   if they belong to \code{\link{WMD}} objects within a single
#'   \code{\link{WMDS}} object.
#'
#'   Note that \code{key = c("a", "b")} would search for an element named
#'   \code{b} \emph{within} the element named \code{a}. To extract two elements
#'   at the same (highest) level, \code{key = list("a", "b")} should be used.
#'   This prevents many \sQuote{subscript out of bounds} errors.
#'
#'   Formulae passed as \code{key} argument are treated by ignoring the left
#'   side (if any) and converting the right side to a list or other vector. Code
#'   enclosed in \code{I} is evaluated with a call to \code{eval}. It is up to
#'   the user to ensure that this call succeeds and yields a character vector or
#'   a list. Operators in all other code within the formula are used just as
#'   separators, and all names are converted to character scalars. The \code{$}
#'   operator binds tightly, i.e. it separates elements of a character vector
#'   (for nested querying) in the output. The same effect have other operators
#'   of high precedence such as \code{::} but their use is not recommended. All
#'   operators with a lower precedence than \code{$} separate list elements.
#'
#'   Additional options when using formulae are described under
#'   \code{\link{extract}}.
#'
#'   The result of \code{metadata_chars} can be used to create a mapping for
#'   \code{\link{map_metadata}}. The \code{\link{WMDS}} method just applies the
#'   \code{\link{WMD}} method to all contained plates in turn.
#'
#' @examples
#'
#' # 'WMD' methods
#'
#' (x <- metadata(vaas_1, "Strain"))
#' stopifnot(x == "DSM30083T")
#' (y <- metadata(vaas_1, ~ Strain)) # using a formula => same result
#' stopifnot(identical(x, y))
#'
#' (x <- metadata_chars(vaas_1, values = FALSE))
#' stopifnot(names(x) == x) # mapping metadata keys to themselves
#' (x <- metadata_chars(vaas_1, values = TRUE))
#' stopifnot(names(x) == x) # mapping metadata values to themselves
#' # See map_metadata() for a potential usage of the metadata_chars() result
#'
#' # 'WMDS' methods
#'
#' (x <- metadata(vaas_4, "Strain"))
#' stopifnot(x == c("DSM18039", "DSM30083T", "DSM1707", "429SC1"))
#' (y <- metadata(vaas_4, ~ Strain)) # using a formula => same result
#' stopifnot(identical(x, y))
#'
#' (x <- metadata_chars(vaas_4, values = TRUE)) # the values
#' (y <- metadata_chars(vaas_4, values = FALSE)) # the keys
#' stopifnot(length(x) > length(y))
#'
#' # detecting misspellings
#' (x <- metadata_chars(vaas_4, max.dist = 0.1))
#' stopifnot(length(x) == 0) # no misspellings
#' (x <- metadata_chars(vaas_4, max.dist = 0.5)) # wrong result!
#' # distance too high => non-synonyms thought to be misspellings
#' stopifnot(length(x) == 2, !is.null(names(x)))
#' (x <- metadata_chars(vaas_4, max.dist = 0.5, exclude = "\\d"))
#' stopifnot(length(x) == 0) # strings with numbers excluded
#'
setGeneric("metadata", function(object, ...) standardGeneric("metadata"))

setMethod("metadata", "WMD", function(object, key = NULL, exact = TRUE,
    strict = FALSE) {
  LL(exact, strict)
  if (!length(key))
    return(object@metadata)
  key <- metadata_key(key, FALSE)
  fetch_fun <- if (strict)
    function(key) {
      if (is.null(result <- object@metadata[[key, exact = exact]]))
        stop(sprintf("got NULL value when using key '%s'",
          paste0(key, collapse = " -> ")))
      result
    }
  else
    function(key) object@metadata[[key, exact = exact]]
  if (is.list(key))
    sapply(key, fetch_fun, simplify = FALSE)
  else # should be a (character) vector
    fetch_fun(key)
}, sealed = SEALED)

setMethod("metadata", "WMDS", function(object, ...) {
  simplify_conditionally(lapply(X = object@plates, FUN = metadata, ...))
}, sealed = SEALED)

#= metadata_chars metadata

#' @rdname metadata
#' @export
#'
setGeneric("metadata_chars",
  function(object, ...) standardGeneric("metadata_chars"))

setMethod("metadata_chars", "WMD", function(object, values = TRUE,
    classes = "factor", max.dist = -1, ...) {
  result <- if (L(values))
      map_values(object = object@metadata, coerce = classes)
    else
      map_names(object@metadata)
  if (is.na(L(max.dist)) || max.dist < 0)
    return(result)
  map_values(object = result, mapping = max.dist, ...)
}, sealed = SEALED)

setMethod("metadata_chars", "WMDS", function(object, values = TRUE,
    classes = "factor", max.dist = -1, ...) {
  result <- unlist(lapply(object@plates, FUN = metadata_chars,
    values = values, classes = classes, max.dist = NA_real_, ...))
  if (is.na(L(max.dist)))
    return(result)
  else if (max.dist < 0) # 2nd call of map_values unifies the
    return(map_values(result)) # vector but keeps the names
  map_values(object = result, mapping = max.dist, ...)
}, sealed = SEALED)

setMethod("metadata_chars", "MOPMX", function(object, values = TRUE,
    classes = "factor", max.dist = -1, ...) {
  result <- unlist(lapply(object@.Data, FUN = metadata_chars,
    values = values, classes = classes, max.dist = NA_real_, ...))
  if (is.na(L(max.dist)))
    return(result)
  else if (max.dist < 0) # 2nd call of map_values unifies the
    return(map_values(result)) # vector but keeps the names
  map_values(object = result, mapping = max.dist, ...)
}, sealed = SEALED)


################################################################################

