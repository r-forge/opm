#!/bin/sh


################################################################################
#
# Documentation-generating script for the 'opm' package
#
# This script was tested using Bash and Dash. Prerequisites are the 'docu.R'
# script from the pkgutils R package and the 'Rscript' executable present in
# the $PATH.
#
# Call this script with the -h option to get an overview of the options.
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
    /usr/lib64/R/library
    /usr/lib/R/library
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


# Check whether or not each file in the source directory (1st argument) has a 
# more recent *.pdf file in the target directory (2nd argument). Used below for 
# comparing the original graphics files with the resulting PDFs in the vignette
# directory.
#
check_graphics_files()
{
  local source_dir=$1 target_dir=$2 errs=0
  local source_file target_file
  for source_file in "$source_dir"/*; do
    [ -e "$source_file" ] || break
    target_file=$target_dir/${source_file##*/}
    target_file=${target_file%.*}.pdf
    if [ "$target_file" -ot "$source_file" ]; then
      echo "WARNING: '$target_file' missing or older than '$source_file'" >&2
      echo >&2
      errs=$(($errs + 1))
    fi
  done
  return $errs
}


################################################################################


# Check whether or not each *.Rnw vignette file has a more recent *.pdf file
# in the doc directory of the package. Remove problematic lines inserted by 
# some software from Rnw files.
#
check_vignettes()
{
  local indir sweave_file pdf_file built_pdf_file
  local errs=0
  for indir; do
    for sweave_file in "$indir"/vignettes/*.Rnw; do
      [ -e "$sweave_file" ] || break
      pdf_file=$indir/inst/doc/${sweave_file##*/}
      pdf_file=${pdf_file%.*}.pdf
      if [ "$sweave_file" -nt "$pdf_file" ]; then
        built_pdf_file=${sweave_file%.*}.pdf
        if [ "$built_pdf_file" -nt "$pdf_file" ]; then
          cp -v "$built_pdf_file" "$pdf_file" 1>&2
          echo >&2
        else
          echo "WARNING: '$pdf_file' missing or older than '$sweave_file'" >&2
          echo "create or update '$built_pdf_file' to fix this" >&2
          echo >&2
          errs=$(($errs + 1))
        fi
      fi
      if grep -q '^\\SweaveOpts{concordance=TRUE}' "$sweave_file"; then
        # changes are only done if necessary to avoid changes in file
        # modification time, and after comparison with PDF
        sed -i 'v; /^\\SweaveOpts{concordance=TRUE}/ d' "$sweave_file"
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
      echo "test file for '$infile' does not exist or is empty" >&2
      errs=$((errs + 1))
    elif ! awk -v rfile="$infile" \
      '
        BEGIN {
          fcnt = 0
          while((getline < rfile) > 0) {
            sub(/\r$/, "") # repair Windows/DOS line breaks
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
            } else if ($0 ~ /^`[^`]+`[ \t]*<-[ \t]*function/) {
              # this should collect backtick-enclosed functions and S3 methods
              # they require special care because "<-" might be part of the name
              sub(/^`/, "", $1)
              sub(/`.*/, "", $1)
              sub(/\.[^<]*/, "", $1)
              if (!($1 in items)) {
                item_names[++fcnt] = $1
                items[$1] = -1
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
        {
          sub(/\r$/, "") # repair Windows/DOS line breaks
        }
        $1 == "##" {
          item = $2
          getline
          sub(/\r$/, "") # repair Windows/DOS line breaks
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
    {
      sub(/\r$/, "") # repair Windows/DOS line breaks
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


# A 'full build' means we copy the code from trunk/opm to pkg/opm, which, after
# committing to SVN, would cause R-Forge to (attempt to) build the next package.
#
if [ $# -gt 0 ] && [ "$1" = full ]; then
  shift
  full_build=yes
else
  full_build=
fi


# Set up package input directory, name of logfile, and location of 'docu.R'.
#
PKG_DIR=opm_in
GRAPHICS_DIR=graphics
[ "${LOGFILE##*/}" = "$LOGFILE" ] || mkdir -p "${LOGFILE%/*}"
DOCU=`find_docu_script || :`
if [ "$DOCU" ]; then
  echo "using script '$DOCU'" >&2
  echo >&2
else
  echo "script 'docu.R' not found, exiting now" >&2
  exit 1
fi


# Conduct checks defined in this file (via shell functions).
#
check_graphics_files "$GRAPHICS_DIR" "$PKG_DIR/vignettes" || true
check_vignettes "$PKG_DIR" || true
if ! check_R_tests "$PKG_DIR"/R/*; then
  echo "something wrong with the tests in '$PKG_DIR', exiting now" >&2
  exit 1
fi
if ! check_roxygen_tags "$PKG_DIR"/R/*; then
  echo "something wrong with the Roxygen tags in '$PKG_DIR', exiting now" >&2
  exit 1
fi


# Conduct checks defined in 'docu.R', as far as requested, and build/check of
# the package directory or archive, if requested.
#
delete_pat="(.*[.](css|bbl|blg|html|aux|yml|epf|R|gz|log|out|tex)|"
delete_pat="vignettes/$delete_pat(Rplots|opm|.*-[0-9]{3,3})[.]pdf)\$"
Rscript --vanilla "$DOCU" "$@" --logfile "$LOGFILE" \
  --modify --preprocess --S4methods --junk "$delete_pat" \
  --good well-map.R,substrate-info.R,plate-map.R "$PKG_DIR"


# Copy the package files to pkg/opm, if requested ('full' build).
#
OUT_DIR=${PKG_DIR%_in}
if [ "$full_build" ]; then
  if [ -d "$OUT_DIR" ]; then
    target=../pkg/$OUT_DIR
    mkdir -p "$target" && cp -r -u "$OUT_DIR"/* "$target" &&
      rm -r "$OUT_DIR"
  fi
else
  echo "NOTE: no full build, '$OUT_DIR' not copied" >&2
  echo >&2
fi


# Move the generated archive files, if any, into their directory.
#
for file in "$OUT_DIR"_*.tar.gz; do
  [ -e "$file" ] || break
  mkdir -p "$BUILT_PACKAGES" && mv -v "$file" "$BUILT_PACKAGES"
done


################################################################################


