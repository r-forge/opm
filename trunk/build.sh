#!/bin/sh


################################################################################
#
# Documentation-generating script for the 'opm' package
#
# This script was tested using Bash and Dash. Prerequisites are the 'docu.R'
# script from the pkgutils R package and the 'Rscript' executable present in
# the $PATH.
#
# This script is distributed under the terms of the Gnu Public License V2.
# For further information, see http://www.gnu.org/licenses/gpl.html
#
# (C) 2012 by Markus Goeker (markus [DOT] goeker [AT] dsmz [DOT] de)
#
################################################################################


# Checked locations of the R installation directory. Names must not contain
# whitespace. The 'docu.R' script is either found in the $PATH or searched
# within these directories, in order. If an environment variable $R_LIBRARY is
# defined, it is honoured.
#
if [ "${R_LIBRARY:-}" ]; then
  R_LIBRARIES=$R_LIBRARY
else
  R_LIBRARIES="
    /usr/local/lib/R/library
    /usr/local/lib64/R/library
    /usr/share/R/library
    $HOME/R/x86_64-pc-linux-gnu-library/2.15
  "
fi


# 'docu.R' creates a logfile itself, which contains, e.g., information on style
# checks and according modifications of R source files.
#
LOGFILE=misc/docu_opm.log


# If source packages are built, they will be moved into that directory.
#
BUILT_PACKAGES=misc/built_packages


################################################################################
#
# It should not be necessary to change anything below this line
#


set -eu


# Find the script docu.R (from the pkgutils R package) either in the $PATH or
# within the pkgutils subdirectory of the R installation directory.
#
find_docu_script()
{
  local result=`which docu.R`
  if [ "$result" ]; then
    echo "$result"
    return 0
  fi
  local r_library
  for r_library in $R_LIBRARIES; do
    result=$r_library/pkgutils/scripts/docu.R
    if [ -s "$result" ]; then
      echo "$result"
      return 0
    fi
  done
  echo
  return 1
}


################################################################################


# Check whether or not each *.Rnw vignette file has a more recent *.pdf file
# in the doc directory of the package.
#
check_vignettes()
{
  local indir sweave_file pdf_file
  local errs=0
  for indir; do
    for sweave_file in "$indir"/vignettes/*.Rnw; do
      [ -s "$sweave_file" ] || break
      pdf_file=$indir/inst/doc/${sweave_file##*/}
      pdf_file=${pdf_file%.*}.pdf
      if [ "$sweave_file" -nt "$pdf_file" ]; then
        echo "WARNING: '$pdf_file' missing or older than '$sweave_file'" >&2
        errs=$(($errs + 1))
      fi
    done
  done
  return $errs
}


################################################################################


# Make sure all functions and constants (package-wide variables) in the R code
# either have a test or are accordingly annotated. See the files in the folder
# inst/tests for how this annotation works.
#
check_R_tests()
{
  local infile testfile
  local errs=0
  for infile; do
    testfile=${infile%/R/*}/inst/tests/test_${infile##*/R/}
    if ! [ -s "$testfile" ]; then
      echo "test file for '$infile' does not exist" >&2
      errs=$((errs + 1))
    elif ! awk -v rfile="$infile" \
      '
        BEGIN {
          fcnt = 0
          while((getline < rfile) > 0) {
            if ($0 ~ /^setMethod[ \t]*\(/) {
              # this should collect S4 methods
              sub(/[^"]+"/, "")
              sub(/".*/, "")
              if (!($0 in items)) {
                item_names[++fcnt] = $0
                items[$0] = -1
              }
            } else if ($0 ~ /^setAs[ \t]*\(/) {
              # this just requests a test for "as" if setAs() has been called
              item = "as"
              if (!(item in items)) {
                item_names[++fcnt] = item
                items[item] = -1
              }
            } else if ($0 ~ /^[^ \t<]+[ \t]*<-[ \t]*function/) {
              # this should collect functions and S3 methods
              sub(/<.*/, "", $1)
              sub(/\..*/, "", $1)
              if (!($1 in items)) {
                item_names[++fcnt] = $1
                items[$1] = -1
              }
            } else if ($0 ~ /^[^ \t<]+[ \t]*<-/) {
              # this is intended to collect package-wide variables (constants
              # at the user-level)
              sub(/<.*/, "", $1)
              if ($1 !~ /[($]/ && !($1 in items)) {
                item_names[++fcnt] = $1
                items[$1] = -1
              }
            }
          }
          close(rfile)
        }
        $1 == "##" {
          item = $2
          getline
          if (!(item in items)) {
            printf "WARNING: unknown name \"%s\" in file %s, line %i\n", item,
              FILENAME, FNR - 1 > "/dev/stderr"
            next
          }
          if ($1 == "##" && $2 == "UNTESTED") {
            items[item] = 0
          } else if ($0 ~ /^test_that\(/) {
            if (items[item] < 0)
              items[item] = 0
            items[item]++
          }
          next
        }
        /^test_that\(/ {
          printf "WARNING: unannotated test in file %s, line %i\n", FILENAME,
            FNR > "/dev/stderr"
        }
        END {
          for (i = 1; i <= fcnt; i++) {
            item = item_names[i]
            count = items[item]
            if (count < 0)
              printf "WARNING: item %s (%s) not found\n", item,
                rfile > "/dev/stderr"
          }
        }
      ' "$testfile"
    then
      echo "error in call to awk -- is a modern awk installed?" >&2
      errs=$(($errs + 1))
    fi
  done
  return $errs
}


################################################################################


# Check "@family" tag entries in Roxygen comments (they must be present in the
# documentation of exported functions with normal names, and must refer to the
# filename).
#
check_roxygen_tags()
{
  awk '
    BEGIN {
      problems = 0
      no_family["datasets"]++
      no_family["internal"]++
      no_family["package"]++
    }
    FILENAME != oldfilename {
      basename = oldfilename = FILENAME
      sub(/^.*\//, "", basename)
      sub(/\.[^.]+$/, "", basename)
    }
    /^#'"'"'/ {
      if (!comment) {
        if ($2 ~ /^@/)
          title = ""
        else
          title = substr($0, index($0, $2))
        position = FNR
        comment = 1
      }
      if ($2 ~ /^@/)
        value[$2] = $3
      next
    }
    /^#~/ {
      next
    }
    comment {
      if (title) {
        if ("@family" in value) {
          sub(/-functions$/, "", value["@family"])
          if (value["@family"] != basename) {
            printf "wrong @family entry in \"%s\" (line %i in file \"%s\")\n",
              title, position, FILENAME
            problems++
          }
        } else if (!(value["@keywords"] in no_family) &&
            !("@exportMethod" in value)) {
          printf "missing @family tag in \"%s\" (line %i in file \"%s\")\n",
            title, position, FILENAME
          problems++
        }
      }
      title = ""
      delete value
      comment = 0
    }
    END {
      exit(problems)
    }
  ' "$@"
}


################################################################################


PKG_DIR=opm_in

DOCU=`find_docu_script || :`

if [ "$DOCU" ]; then
  echo "using script '$DOCU'" >&2
  echo >&2
else
  echo "script 'docu.R' not found, exiting now" >&2
  exit 1
fi

[ "${LOGFILE##*/}" = "$LOGFILE" ] || mkdir -p "${LOGFILE%/*}"

check_vignettes "$PKG_DIR" || true

if ! check_R_tests "$PKG_DIR"/R/*; then
  echo "something wrong with the tests in '$PKG_DIR', exiting now" >&2
  exit 1
fi

if ! check_roxygen_tags "$PKG_DIR"/R/*; then
  echo "something wrong with the Roxygen tags in '$PKG_DIR', exiting now" >&2
  exit 1
fi

Rscript --vanilla "$DOCU" "$@" --logfile "$LOGFILE" \
  --modify --preprocess --S4methods \
  --good well-map.R,substrate-info.R,plate-map.R "$PKG_DIR"

OUT_DIR=${PKG_DIR%_in}
if [ -d "$OUT_DIR" ]; then
  target=../pkg/$OUT_DIR
  mkdir -p "$target" && cp -r -u "$OUT_DIR"/* "$target" &&
    rm -r "$OUT_DIR"
fi

for file in "$OUT_DIR"_*.tar.gz; do
  [ -s "$file" ] || break
  mkdir -p "$BUILT_PACKAGES" && mv -v "$file" "$BUILT_PACKAGES"
done


################################################################################



