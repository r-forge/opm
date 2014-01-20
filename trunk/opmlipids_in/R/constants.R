

# Used in all S4 method definitions. The idea is to keep it FALSE during
# programming but set it to TRUE when building the package. The line contains a
# special comment used by pkgutils and should not be changed.
#
SEALED <- FALSE #|| SEALED <- TRUE


################################################################################
#
# Class names
#

FAME <- "FAME"

FAMES <- "FAMES"


################################################################################


DEFAULT_PLATE_TYPE_SETTINGS <- list(
  na.yields = 0, # replacement of NA values resulting when creating matrix
  file.entry = "OLIF", # name of input-file entry in the metadata
  char.group = "Fatty acids",
  tolerance = 0.1, # used when checking whether MIDI results add up to 100%
  value.col = "VALUE", # special column within the measurements
  row.names = "_ROWNAMES" # for YAML input/output
)


PLATE_TYPE_SETTINGS <- new.env(parent = emptyenv())
PLATE_TYPE_SETTINGS$MIDI <- DEFAULT_PLATE_TYPE_SETTINGS


# For duplicate row names.
#
APPENDIX <- " #"


################################################################################




