

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


# Special column within the measurements.
#
VALUE_COL <- "VALUE"

# For YAML input/output.
#
ROWNAMES <- "_ROWNAMES"

# For duplicate row names.
#
APPENDIX <- " #"

# Name of input-file entry in the metadata.
#
OLIF <- "OLIF"


################################################################################


# Used when checking whether MID results add up to 100%.
#
TOLERANCE <- 0.1

