#!/bin/sh


################################################################################
#
# Build, analysis and test script for the 'opm' package.
#
# This script was tested using Bash and Dash. For details on portability, see
# https://wiki.ubuntu.com/DashAsBinSh . Prerequisites for using this script in
# main running modes are the 'docu.R' script from the 'pkgutils' R package and
# the 'Rscript' executable present in the $PATH. The working directory in which
# to call this script is the parent directory of the R package directories one
# is dealing with.
#
# Call this script with 'help' as first argument to get an overview of its
# usage.
#
# This script is distributed under the terms of the Gnu Public License V2.
# For further information, see http://www.gnu.org/licenses/gpl.html
#
# (C) 2013 by Markus Goeker (markus [DOT] goeker [AT] dsmz [DOT] de)
#
################################################################################


# The folder in which the files for the external tests are placed (i.e. those
# without an interactive R session). Must contain a subfolder "tests" with the
# files used.
#
EXTERNAL_TEST_DIR=external_tests


# 'docu.R' creates a logfile itself, which contains, e.g., information on style
# checks and according modifications of R source files. Should only be changed
# after discussion with all project members.
#
LOGFILE=misc/docu_opm.log


# If source packages are built, they will be moved into that directory. Should
# only be changed after discussion with all project members.
#
BUILT_PACKAGES=misc/built_packages


################################################################################
#
# It should not be necessary to change anything below this line. Negotiate with
# MG in case of doubt.
#


set -eu


# Find the script 'docu.R' (from the 'pkgutils' R package) either in the
# environment variable $DOCU_R_SCRIPT or in the $PATH or within the 'pkgutils'
# subdirectory of the default R installation directory.
#
find_docu_script()
{
  local result=${DOCU_R_SCRIPT:-`which docu.R`}
  if [ "$result" ] && [ -s "$result" ]; then
    echo "$result"
    return 0
  fi
  local r_dir=`R RHOME`
  local subdir
  for subdir in library site-library; do
    result=$r_dir/$subdir/pkgutils/scripts/docu.R
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
  local source_dir=$1
  local target_dir=$2
  local errs=0
  local source_file
  local target_file
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
# some software from *.Rnw files.
#
check_vignettes()
{
  local indir
  local sweave_file
  local pdf_file
  local built_pdf_file
  local errs=0
  for indir; do
    for sweave_file in "$indir"/vignettes/*.Rnw; do
      [ -e "$sweave_file" ] || break
      pdf_file=$indir/inst/doc/${sweave_file##*/}
      pdf_file=${pdf_file%.*}.pdf
      if [ "$sweave_file" -nt "$pdf_file" ] || ! [ -e "$pdf_file" ]; then
        built_pdf_file=${sweave_file%.*}.pdf
        if [ "$built_pdf_file" -nt "$pdf_file" ] ||
            [ -e "$built_pdf_file" -a ! -e "$pdf_file" ]; then
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
# either have a test or are annotated as being untested. (See the files in the
# folder inst/tests for how this annotation works.)
#
check_R_tests()
{
  local infile
  local testfile
  local errs=0
  for infile; do
    testfile=${infile%/R/*}/inst/tests/test_${infile##*/R/}
    if ! [ -s "$testfile" ]; then
      echo "test file for '$infile' does not exist or is empty" >&2
      errs=$((errs + 1))
    elif ! awk -v rfile="$infile" '
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


# Check "@family" tag entries in Roxygen comments. They must be present in the
# documentation of exported functions with normal names, and must refer to the
# filename.
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
              title, position, FILENAME > "/dev/stderr"
            problems++
          }
        } else if (!(value["@keywords"] in no_family) &&
            !("@exportMethod" in value)) {
          printf "missing @family tag in \"%s\" (line %i in file \"%s\")\n",
            title, position, FILENAME > "/dev/stderr"
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


# Show the warnings, if any, within the results from running examples in an R
# package documentation. Assume default output names used by 'R CMD check'.
#
show_example_warnings()
{
  local folder
  local outfile
  for folder; do
    outfile=$folder.Rcheck/${folder##*/}-Ex.Rout
    [ -s "$outfile" ] || continue
    awk '
      BEGIN {
        cnt = 0
        printf "WARNINGS found in \"%s\" (might be OK):\n",
          ARGV[1] > "/dev/stderr"
      }
      {
        sub(/\r$/, "") # repair Windows/DOS line breaks
      }
      $1 == "Warning" {
        cnt++
        text = $0
        if (text !~ / :$/) {
          print text > "/dev/stderr"
          next
        }
        while (1) {
          getline
          if ($0 !~ /^  /)
            break
          text = (text $0)
        }
        print text > "/dev/stderr"
      }
      END {
        if (!cnt)
          print "None found." > "/dev/stderr"
      }
    ' "$outfile"
    echo >&2
  done
}


################################################################################


# Show the timings, if any, in the results from running examples contained in
# an R package. Assume default output names used by 'R CMD check'.
#
show_example_timings()
{
  local folder
  local outfile
  for folder; do
    timings=$folder.Rcheck/${folder##*/}-Ex.timings
    if [ -s "$timings" ]; then
      echo "# $folder"
      sort -nk4 "$timings"
    else
      echo "No timings for directory '$folder'." >&2
    fi
    echo
  done
}


################################################################################


# Show the warnings, if any, in the results from running tests contained in an
# R package. Assume default output names used by 'R CMD check'.
#
show_test_warnings()
{
  local folder
  local outfile
  for folder; do
    find -type f -wholename "*/$folder.Rcheck/tests/*.Rout" -exec awk '
      FILENAME != oldfilename {
        if (!cnt && oldfilename)
          print "None found." > "/dev/stderr"
        printf "WARNINGS found in \"%s\" (should be fixed):\n",
          FILENAME > "/dev/stderr"
        oldfilename = FILENAME
        cnt = 0
      }
      {
        sub(/\r$/, "") # repair Windows/DOS line breaks
      }
      /There were [0-9]+ (or more )?warnings/ {
        cnt++
        print > "/dev/stderr"
        next
      }
      /^Warning messages:/, /^>/ {
        cnt++
        print > "/dev/stderr"
        next
      }
      /Warning message:$/ { # explicit warnings outside of tests
        cnt++
        print "Warning message:" > "/dev/stderr"
        getline
        print > "/dev/stderr"
        next
      }
      /^Warning message:/, /^>/ { # from tests not based on testthat
        cnt++
        if ($0 !~ /^>/)
          print > "/dev/stderr"
      }
      END {
        if (!cnt)
          print "None found." > "/dev/stderr"
      }
    ' \{\} +
    echo >&2
  done
}


################################################################################


# Remove trailing whitespace characters from R files.
#
remove_trailing_whitespace()
{
  # Note that GNU sed does not understand \b (the backspace escape). We also
  # omit \r to avoid any problems with Mac or Windows line breaks.
  find . -type f -name '*.R' -exec sed -i 'v; s/[ \t\v\a\f]\+$//' \{\} +
}


################################################################################


# Count tags used in Roxygen2 documentation.
#
count_roxygen_tags()
{
  find . -type f -wholename '*/R/*.R' \
    -exec awk '$1 == "#'"'"'" && $2 ~ /^@/ {print $2}' \{\} + |
      sort - | uniq -c - | sort -nr -
}


################################################################################


# Clean up directories left over by R CMD check.
#
remove_R_CMD_check_dirs()
{
  find . -type d -name '*.Rcheck' -prune -exec rm -fr \{\} +
}


################################################################################


# Clean up directories if they have a *_in counterpart.
#
remove_dirs_carefully()
{
  local indir
  local errs=0
  for indir; do
    # missing directories are OK because they may already have been removed
    [ -d "$indir" ] || continue
    if [ -d "${indir}_in" ]; then
      rm -rf "$indir"
    else
      echo "directory '$indir' given, but '${indir}_in' is missing" >&2
      errs=$(($errs + 1))
    fi
  done
  return $errs
}


################################################################################


# Run R CMD Stangle on all vignette code files.
#
run_Stangle()
{
  local indir
  for indir; do
    echo "$indir" >&2
    echo find "$indir" -type f -name '*.Rnw' -exec R CMD Stangle \{\} \;
  done
}


################################################################################


# Extract R code within the examples of the given files.
#
extract_examples()
{
  local infile
  local package
  for infile; do
    package=${infile%/man/*}
    package=${package##*/}
    [ "${package#*.}" = Rd ] && package=
    R CMD Rdconv --type=txt "$infile" |
      awk -v examples=0 -v package="$package" '
        $0 == "_\bE_\bx_\ba_\bm_\bp_\bl_\be_\bs:" {
          examples = 1
          if (package) {
            printf "## guessed from the filename\n"
            printf "library(%s)\n", package
          }
          next
        }
        examples {
          sub(/^     /, "")
          print
        }
      ' -
  done
}


################################################################################


# Search for all *.Rout files, then search within these for the results from
# running the examples of one to several given functions.
#
show_example_results()
{
  [ $# -eq 0 ] && return
  find . -name '*-Ex.Rout' -exec awk -v query="$*" '
      BEGIN {
        nq = split(query, field)
        for (i = 1; i <= nq; i++)
          wanted[field[i]]++
        must_print = 0
      }
      /^> +#+ +[*] +/ {
        must_print = ($NF in wanted)
        if (must_print) {
          sub(/^> +/, "")
          printf "%s [%s]\n", $0, FILENAME
        }
      }
      /^> +#+ +[*][*] +Examples/, /^> +base:+/ {
        if (must_print && $0 !~ /^> +(base:+|#+ +[*][*] +Examples)/) {
          sub(/^> +/, "")
          print
        }
      }
    ' \{\} +
}


################################################################################


# Show 'todo' entries in R files. This will find all uppercase, separate-word
# entries, irrespective of whether or not they are outcommented.
#
show_todos()
{
  find . -type f -name '*.R' -exec grep -Fnw TODO \{\} +
}


################################################################################


# Remove files left over from interactive or non-interactive R sessions.
#
remove_R_session_files()
{
  local pattern
  for pattern in .RData .Rhistory "*.Rout"; do
    find . -name "$pattern" -delete
  done
}


################################################################################


# Run the R code in the files delivered with opm as examples.
#
test_external_examples()
{
  local wdir=`pwd`
  local tmpdir=`mktemp -d --tmpdir`
  cd "$tmpdir"
  local csv_file
  for csv_file in "$wdir"/external_tests/tests/*.csv; do
    case $csv_file in
      *Multiline* ) continue;;
    esac
    ln -s "$csv_file" "${csv_file##*/}"
  done
  local rscript
  local errs=0
  for rscript in "$wdir"/"$1"/inst/examples/*.R; do
    [ -s "$rscript" ] || continue
    echo "TESTING ${rscript##*/}..."
    if R CMD BATCH "$rscript"; then
      echo "	<<<SUCCESS>>>"
    else
      echo "	<<<FAILURE>>>"
      errs=$((errs + 1))
      cp "${rscript##*/}out" "$wdir"
    fi
    echo
  done
  cd "$wdir"
  rm -rf "$tmpdir"
  return $errs
}

################################################################################


# Show lines that contain non-printable ASCII (except for space, carriage
# return and line feed) or non-ASCII.
#
show_lines_with_forbidden_characters()
{
  [ $# -eq 0 ] && return
  awk '/[^\n -~]/ {
    printf "%s:%i\t%s\n", FILENAME, FNR, $0
  }' "$@"
}


################################################################################


# Command-line argument parsing. The issue here is that all arguments for
# 'docu.R' should remain untouched. We thus only allow for a single running
# mode indicator as (optional) first argument.
#
if [ $# -gt 0 ] && [ "${1%%-*}" ]; then
  RUNNING_MODE=$1
  shift
else
  RUNNING_MODE=norm
fi

remind_of_external_tests=

case $RUNNING_MODE in
  ascii )
    show_lines_with_forbidden_characters "$@"
    exit $?
  ;;
  bnorm )
    PKG_DIR=opmDB_in
    RUNNING_MODE=${RUNNING_MODE#b}
    CHECK_R_TESTS=
  ;;
  codex )
    test_external_examples opm_in
    exit $?
  ;;
  dfull|dnorm )
    PKG_DIR=opmdata_in
    RUNNING_MODE=${RUNNING_MODE#d}
    CHECK_R_TESTS=
  ;;
  docu )
    :
  ;;
  erase )
    remove_R_CMD_check_dirs &&
      remove_dirs_carefully pkgutils opm opmdata opmDB
    exit $?
  ;;
  example )
    extract_examples "$@"
    exit $?
  ;;
  forget )
    remove_R_session_files
    exit $?
  ;;
  full|norm )
    PKG_DIR=opm_in
    CHECK_R_TESTS=yes
    remind_of_external_tests=yes
  ;;
  help )
    cat >&2 <<-____EOF
	$0 -- build the opm package using the 'docu.R' script.

	This script must be executed in the parent directory of the project's R
	package source directories.

	Usage: $0 [mode] [options]

	Possible values for 'mode':
	  ascii   Show lines that contain forbidden characters (such as non-ASCII).
	  bnorm   Normal build of the opmDB package.
	  codex   Test the external code examples that come with opm.
	  dfull   Full build of the opmdata package.
	  dnorm   Normal build of the opmdata package.
	  docu    Check whether the 'docu.R' script can be found, then exit.
	  erase   Remove directories left over by R CMD check and docu.R, if any.
	  example Extract R code from the examples within specified Rd files.
	  forget  Remove .RData, .Rhistory and *.Rout files.
	  full    Full build of the opm package.
	  help    Print this message.
	  norm    [DEFAULT] Normal build of the opm package.
	  pfull   Full build of the pkgutils package.
	  pnorm   Normal build of the pkgutils package.
	  rnw     Run R CMD Stangle on the *.Rnw files.
	  rout    Show results of the examples, if any, for given function names.
	  space   Remove trailing whitespace from R code files.
	  tags    Get list of Roxygen2 tags used, with counts of occurrences.
	  time    Show the timings of the last examples, if any, in order.
	  todo    Show TODO entries (literally!) in R source files.

	A 'full' build includes copying to the local copy of the pkg directory.
	Other details of the build process depend on the options.

	All options go to 'docu.R'. Use '-h' to display all of them. Missing options
	in normal or full running mode means generating the test copy of the package
	directory and the documentation and running the checks implemented in this
	script, but not those via 'docu.R'.

	One frequently needs the following options:
	  -c	Check the copy of the package directory.
	  -i	Check and install the copy of the package directory.
	  -o no-vignettes,no-build-vignettes	Skip the time-consuming parts.
	  -o no-codoc,no-examples,no-tests,no-manual	Check vignettes only.
	  -u	Turn off checking altogether (used together with -i or -y).
	  -y	Build a package tar archive.

	Search for 'docu.R' is first done in the environment variable \$DOCU_R_SCRIPT,
	then in the \$PATH, then in the R installation directory. For an initial setup
	of the build process it is usually necessary to install the pkgutils package
	obtained from R-Forge manually via, e.g., R CMD INSTALL, and then to assure
	that the installed 'docu.R' script is found.

____EOF
    exit 1
  ;;
  pfull|pnorm )
    PKG_DIR=pkgutils_in
    RUNNING_MODE=${RUNNING_MODE#p}
    CHECK_R_TESTS=
  ;;
  rnw )
    run_Stangle opm_in opmdata_in pkgutils_in opmDB_in
    exit $?
  ;;
  rout )
    show_example_results "$@"
    exit $?
  ;;
  space )
    remove_trailing_whitespace
    exit $?
  ;;
  tags )
    count_roxygen_tags
    exit $?
  ;;
  time )
    show_example_timings opm opmdata pkgutils opmDB
    exit $?
  ;;
  todo )
    show_todos
    exit $?
  ;;
  * )
    echo "unknown running mode '$RUNNING_MODE', exiting now" >&2
    exit 1
  ;;
esac


################################################################################


# Get the 'docu.R' script.
#
DOCU=`find_docu_script || :`
if [ "$DOCU" ]; then
  echo "Using script '$DOCU' (`stat -c %y "$DOCU"`)..." >&2
  echo >&2
  [ "$RUNNING_MODE" = docu ] && exit 0
else
  echo "script 'docu.R' not found, exiting now" >&2
  echo "call '$0 help' for help" >&2
  exit 1
fi


################################################################################
#
# From here on, either a 'normal' or a 'full' build will be conducted. A
# 'full build' means we copy the code from trunk/opm to pkg/opm, which, after
# committing to SVN, would cause R-Forge to (attempt to) build the next
# package.
#


# Conduct checks defined in this file (via shell functions).
#
check_graphics_files graphics "$PKG_DIR/vignettes" || :
check_vignettes "$PKG_DIR" || :
if [ "$CHECK_R_TESTS" ]; then
  if ! check_R_tests "$PKG_DIR"/R/*; then
    echo "something wrong with the tests in '$PKG_DIR', exiting now" >&2
    exit 1
  fi
else
  echo "NOTE: omitting check for the presence of R tests." >&2
  echo >&2
fi
if ! check_roxygen_tags "$PKG_DIR"/R/*; then
  echo "something wrong with the Roxygen tags in '$PKG_DIR', exiting now" >&2
  exit 1
fi


# Conduct checks defined in 'docu.R', as far as requested, and build/check of
# the package directory or archive, if requested.
#
delete_pat="[.](aux|bbl|blg|bst|cls|css|epf|gz|html|log|out|png|R|tex|yml|xml)"
# 'jss.cls' and 'jss.bst' are in R and cause a NOTE during checking
delete_pat="vignettes/.*($delete_pat|(?<!opm_fig_[0-9])[.]pdf)\$"
[ "${LOGFILE##*/}" = "$LOGFILE" ] || mkdir -p "${LOGFILE%/*}"
Rscript --vanilla "$DOCU" "$@" --logfile "$LOGFILE" \
  --modify --preprocess --S4methods --junk "$delete_pat" \
  --good well-map.R,substrate-info.R,plate-map.R "$PKG_DIR"


OUT_DIR=${PKG_DIR%_in}


# Visualize R warnings in the examples and tests, if any.
#
echo >&2
show_example_warnings "$OUT_DIR"
show_test_warnings "$OUT_DIR"


# Copy the package files to pkg/, if requested ('full' build).
#
if [ "$RUNNING_MODE" = full ]; then
  if [ -d "$OUT_DIR" ]; then
    target=../pkg/$OUT_DIR
    mkdir -p "$target" && cp -ru "$OUT_DIR"/* "$target" && rm -r "$OUT_DIR"
  fi
else
  echo "NOTE: no full build, '$OUT_DIR' not copied." >&2
  echo >&2
fi


# Move the generated archive files, if any, into their directory.
#
for file in "$OUT_DIR"_*.tar.gz; do
  [ -e "$file" ] || break
  mkdir -p "$BUILT_PACKAGES" && mv -v "$file" "$BUILT_PACKAGES"
done


if [ "$remind_of_external_tests" ]; then
  echo "NOTE: please do not forget to run the external tests, too." >&2
  echo "Any problems within them should be fixed before submission." >&2
  echo "(Small deviations in plotting mode might be OK.)" >&2
  echo >&2
fi


################################################################################


