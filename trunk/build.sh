#!/bin/bash


################################################################################
#
# Build, analysis and test script for the 'opm' package and its accompanying
# packages.
#
# This script was tested using Bash and Dash. For details on portability, see
# https://wiki.ubuntu.com/DashAsBinSh . Prerequisites for using this script in
# main running modes are the 'docu.R' script from the 'pkgutils' R package and
# the 'Rscript' executable present in the $PATH. The working directory in which
# to call this script is the parent directory of the R package directories one
# is dealing with. The 'test' running mode needs an installed opm package, with
# an available 'run_opm.R' script.
#
# Call this script with 'help' as first argument to get an overview of its
# usage.
#
# This script is distributed under the terms of the Gnu Public License V2.
# For further information, see http://www.gnu.org/licenses/gpl.html
#
# (C) 2012, 2013 by Markus Goeker (markus [DOT] goeker [AT] dsmz [DOT] de)
#
################################################################################


# The folder in which the files for the external tests are placed (i.e. those
# without an interactive R session). Must contain a subfolder "tests" with the
# files used. These files are mostly under version control, so this should be
# changed only if great care is taken.
#
EXTERNAL_TEST_DIR=external_tests


# The folder in which a variety of 'miscellaneous' files are placed. Some of
# them are under version control, so this should only be changed with great
# care.
#
MISC_DIR=misc


################################################################################
#
# It should not be necessary to change anything below this line. Negotiate with
# MG in case of doubt.
#


set -eu

# 'docu.R' creates a logfile itself, which contains, e.g., information on style
# checks and according modifications of R source files.
#
LOGFILE=$MISC_DIR/docu_opm.log

# If source packages are built, they will be moved into that directory.
#
BUILT_PACKAGES=$MISC_DIR/built_packages

# Manuals (PDF files) rescued from the check directories.
#
RESCUED_MANUALS=$MISC_DIR/manuals

# The whitelists used for spell checking.
#
WHITELIST_MANUAL=$MISC_DIR/whitelist-manual.txt
WHITELIST_VIGNETTE=$MISC_DIR/whitelist-vignette.txt


################################################################################


# Find the script with the name given as first argument either in the variable
# $PATH or within the subdirectory (given as second argument) of the R
# installation directory.
#
find_R_script()
{
  local result=`which "$1"`
  if [ "$result" ] && [ -s "$result" ]; then
    echo "$result"
    return 0
  fi
  local r_dir=`R RHOME`
  local subdir
  for subdir in library site-library; do
    result=$r_dir/$subdir/$2/scripts/$1
    if [ -s "$result" ]; then
      echo "$result"
      return 0
    fi
  done
  echo
  return 1
}


################################################################################


# Find the script 'docu.R' (from the 'pkgutils' R package) either in the
# environment variable $DOCU_R_SCRIPT or via find_R_script().
#
find_docu_script()
{
  if [ "${DOCU_R_SCRIPT:-}" ]; then
    echo "$DOCU_R_SCRIPT"
  else
    find_R_script docu.R pkgutils
  fi
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
  [ -d "$target_dir" ] || return
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


# If a given number of CPUs is larger than the number of available ones, reduce
# it accordingly.
#
correct_num_cpus()
{
  local wanted=$(($1 + 0))
  local got=`grep -wc processor /proc/cpuinfo 2> /dev/null || echo 0`
  if [ $got -lt $wanted ]; then
    echo "WARNING: number of used CPUs reduced from $wanted to $got" >&2
    echo >&2
    echo "$got"
  else
    echo "$wanted"
  fi
}


################################################################################


# Used by do_test().
#
format_basename()
{
  local filename=${1##*/}
  printf "$2" ${filename%.*}
}


################################################################################


# Conduct a certain kind of test based on file comparison.
#
do_test()
{
  local qdir=
  local outfmt=
  local wantedfmt=
  local indir=
  local inext=
  local logfile=/dev/stderr

  local opt
  OPTIND=1
  while getopts d:f:i:l:q:w: opt; do
    case $opt in
      d ) indir=$OPTARG;;
      f ) outfmt=$OPTARG;;
      i ) inext=$OPTARG;;
      l ) logfile=$OPTARG;;
      q ) qdir=$OPTARG;;
      w ) wantedfmt=$OPTARG;;
      * ) return 1;;
    esac
  done
  shift $(($OPTIND - 1))

  if [ $# -eq 0 ] || [ -z "$outfmt" ] || [ -z "$wantedfmt" ] ||
    [ -z "$indir" ] || [ -z "$inext" ] || [ -z "$qdir" ]
  then
    echo "arguments -d, -f, -i, -q and -w must be provided" >&2
    return 1
  fi

  "$@" "$indir"/*."$inext" 2> "$logfile" || true

  local infile
  local wantfile
  local gotfile
  local lastfile=

  for infile in "$indir"/*."$inext"; do
    wantfile=`format_basename "$infile" "$wantedfmt"`
    gotfile=`format_basename "$infile" "$outfmt"`
    [ "$lastfile" ] && [ "$lastfile" = "$gotfile" ] && continue
    lastfile=$gotfile
    echo
    echo "TESTING $infile => $wantfile..."
    if [ -s "$gotfile" ]; then
      if diff -q "$wantfile" "$gotfile"; then
        echo "	<<<SUCCESS>>>"
        echo
        rm -f "$gotfile"
      else
        echo "	<<<FAILURE>>>"
        echo
        mv "$gotfile" "$qdir"
      fi
    else
      echo "	<<<ERROR>>>"
      echo
      rm -f "$gotfile"
    fi
  done
}


################################################################################


# Modify the version entry within opm-generated JSON files.
#
change_json_version()
{
  local version=$1
  shift
  sed -i "v; s/\(\"version\"\):\"[^\"]\+\"/\1:\"$version\"/g" "$@"
}


################################################################################


# Modify the version entry within opm-generated YAML files.
#
change_yaml_version()
{
  local version=$1
  local tmpfile=`mktemp --tmpdir`
  shift
  local infile
  for infile; do
    if awk -v version="$version" '
      $1 == "version:" {sub($2, version)}
      {print}
      ' "$infile" > "$tmpfile"
    then
      mv "$tmpfile" "$infile"
    else
      rm -f "$tmpfile"
      return 1
    fi
  done
}


################################################################################


# Modify the version entry within opm-generated CSV files.
#
change_csv_version()
{
  local version=$1
  shift
  sed -i "v; s/\(\"opm\";\"\)[^\"]\+\(\";\)/\1$version\2/g" "$@"
}


################################################################################


# Used for showing the failed files.
#
show_files_of()
{
  local files
  mkdir -p "$1"
  files=`ls "$1"`
  if [ "$files" ]; then
    echo "File(s) within '$1':"
    local f
    for f in $files; do
      echo "$f"
    done
  else
    echo "No files found within '$1'."
  fi
}


################################################################################


# Helper function for comparing JSON files (which is easier after conversion to
# YAML).
#
reyaml()
{
  local cmd='for (file in commandArgs(TRUE))'
  cmd="$cmd  write(yaml::as.yaml(yaml::yaml.load_file(file)), \"\")"
  Rscript --vanilla -e "$cmd" "$@"
}


################################################################################


# Call diff on failed files, giving their directory, the test directory, and
# the file extension.
#
compare_files_of()
{
  local failed
  failed=`find "$1" -type f -name "*.$3"`
  if [ -z "$failed" ]; then
    echo "No files with extension '$3' found." >&2
    return 1
  fi
  local failed_file
  local other_file
  local tmpfile=`mktemp --tmpdir`
  for failed_file in $failed; do
    other_file=$2/${failed_file##*/}
    echo "$failed_file"
    case $3 in
      json )
        reyaml "$failed_file" > "$tmpfile"
        reyaml "$other_file" | diff "$tmpfile" - || :
      ;;
      * )
        diff "$failed_file" "$other_file" || :
      ;;
    esac
    echo
  done
  rm -f "$tmpfile"
}


################################################################################


# For testing the 'run_opm.R' script that comes with the opm package.
#
run_external_tests()
{

  local output_mode=run_tests
  local help_msg=
  local np=4 # using more cores yielded only little speedup
  local run_opm=
  local testdir=$EXTERNAL_TEST_DIR
  local version=opm_in/DESCRIPTION
  local extension

  local opt
  OPTIND=1
  while getopts c:d:fhp:s:v: opt; do
    case $opt in
      c ) output_mode=compare_failed; extension=$OPTARG;;
      d ) testdir=$OPTARG;;
      f ) output_mode=show_failed;;
      h ) help_msg=yes;;
      p ) np=$(($OPTARG + 0));;
      s ) run_opm=$OPTARG;;
      v ) version=$OPTARG;;
      * ) return 1;;
    esac
  done
  shift $(($OPTIND - 1))

  if [ "$help_msg" ] || [ $# -gt 0 ]; then
    cat >&2 <<-____EOF
	Test the opm package via its 'run_opm.R' script. For testing the current opm
	version, it must be installed beforehand.

	As usual, this running mode must be executed in the parent directory of the
	project's R package source directories.

	Options:
	  -c x  Ignore tests; show differences for failed files with extension x.
	  -d x  Use x as test directory (must contain subdirectory 'tests').
	  -f    Do not run tests; list all failed files (if any).
	  -h    Print this message.
	  -p x  Use x processors (cores) for running the tests.
	  -s x  Use x as 'run_opm.R' script for running the tests.
	  -v x  Insert opm version x (x can also be an R package DESCRIPTION file).

	The default is to read the version to use during the tests from the opm
	DESCRIPTION file from the opm code directory within the working directory.
	So if tests fail but the resulting files show no differences, this usually
	means an old opm version was used for testing.

____EOF
    return 1
  fi

  testfile_dir=$testdir/tests # must not be local variable for use with trap()
  if ! [ -d "$testfile_dir" ]; then
    echo "directory '$testfile_dir' does not exist, exiting now" >&2
    exit 1
  fi
  # created if necessary; must not be local variable for use with trap()
  failedfile_dir=$testdir/failed_files
  mkdir -p "$failedfile_dir"

  local errfile=$testdir/tests.err
  local outfile=$testdir/tests.out

  case $output_mode in
    compare_failed )
      compare_files_of "$failedfile_dir" "$testfile_dir" "$extension"
      exit $?
    ;;
    show_failed )
      show_files_of "$failedfile_dir"
      exit $?
    ;;
    run_tests )
      :
    ;;
    * )
      echo "unknown \$output_mode '$output_mode'" >&2
      exit 1
    ;;
  esac

  [ "$run_opm" ] || run_opm=`find_R_script run_opm.R opm || :`
  if [ -s "$run_opm" ]; then
    echo "Using script '$run_opm' (`stat -c %y "$run_opm"`)..." >&2
    echo "NOTE: Make sure this is the opm version you want to test!" >&2
    echo >&2
  else
    echo "script 'run_opm.R' not found and not provided, exiting now" >&2
    return 1
  fi

  if [ "$version" ]; then
    [ -s "$version" ] &&
      version=`awk '$1 == "Version:" {print $2; exit}' "$version"`
  else
    echo "opm version to insert not found and not provided, exiting now" >&2
    return 1
  fi

  np=`correct_num_cpus "$np"`

  rm -rf "$failedfile_dir"/*
  rm -f "$errfile" "$outfile"
  local tmpdir=`mktemp --tmpdir -d`
  local tmpfile=`mktemp --tmpdir`

  # Update the version to let the YAML, JSON and CSV tests pass the test
  # irrespective of the actual version. This must later on be reversed, see
  # below.
  #
  change_yaml_version "$version" "$testfile_dir"/*.yml
  change_json_version "$version" "$testfile_dir"/*.json
  change_csv_version "$version" "$testfile_dir"/*.tab

  # Fix the version in the YAML, JSON and CSV files to avoid SVN updates. Do
  # this in the quarantined files, too, if any, to avoid annoying reports when
  # manually calling diff.
  #
  trap '
    change_yaml_version 0.0.0 "$testfile_dir"/*.yml
    change_yaml_version 0.0.0 "$failedfile_dir"/*.yml 2> /dev/null || true
    change_json_version 0.0.0 "$testfile_dir"/*.json
    change_json_version 0.0.0 "$failedfile_dir"/*.json 2> /dev/null || true
    change_csv_version 0.0.0 "$testfile_dir"/*.tab
    change_csv_version 0.0.0 "$failedfile_dir"/*.tab 2> /dev/null || true
  ' 0

  echo "Testing plot mode..."
  do_test -i csv -d "$testfile_dir" \
    -w "$testfile_dir/%s.ps" -l "$tmpfile" \
    -f "$tmpdir/%s.ps" -q "$failedfile_dir" \
    Rscript --vanilla "$run_opm" -p "$np" -r xyplot -d "$tmpdir" -i '*.csv' \
    -k 'TIME:Setup Time,ID' >> "$outfile" &&
      cat "$tmpfile" >> "$errfile"

  echo "Testing split mode..."
  # This test only guarantees that if there is nothing to split the original
  # file results.
  do_test -i csv -d "$testfile_dir" \
    -w "$testfile_dir"/%s.csv -l "$tmpfile" \
    -f "$tmpdir/%s-00001.csv" -q "$failedfile_dir" \
    Rscript --vanilla "$run_opm" -p "$np" -s , -r split -d "$tmpdir" \
    -i '*.csv' -k 'TIME:Setup Time,ID' >> "$outfile" &&
      cat "$tmpfile" >> "$errfile"

  echo "Testing template-collection mode with machine ID and normalization..."
  do_test -i csv -d "$testfile_dir" \
    -w "$testfile_dir/md.template" -l "$tmpfile" \
    -f "$tmpdir/md.template" -q "$failedfile_dir" \
    Rscript --vanilla "$run_opm" -p "$np" -r template \
    -m "$tmpdir/md.template" -i '*.csv' -y 5 -v >> "$outfile" &&
      cat "$tmpfile" >> "$errfile"

  echo "Testing template-collection mode with other field separator..."
  do_test -i csv -d "$testfile_dir" \
    -w "$testfile_dir/md.template2" -l "$tmpfile" \
    -f "$tmpdir/md.template2" -q "$failedfile_dir" \
    Rscript --vanilla "$run_opm" -p "$np" -m "$tmpdir/md.template2" \
    -r template -s , -i '*.csv' -k 'TIME:Setup Time,ID' >> "$outfile" &&
      cat "$tmpfile" >> "$errfile"

  echo "Testing YAML mode..."
  do_test -i csv -d "$testfile_dir" \
    -w "$testfile_dir/%s.yml" -l "$tmpfile" \
    -f "$tmpdir/%s.yml" -q "$failedfile_dir" \
    Rscript --vanilla "$run_opm" -z -p "$np" -a fast -b 0 -r yaml \
    -d "$tmpdir" -i '*.csv' -k 'TIME:Setup Time,ID' >> "$outfile" &&
      cat "$tmpfile" >> "$errfile"

  echo "Testing JSON mode..."
  do_test -i csv -d "$testfile_dir" \
    -w "$testfile_dir/%s.json" -l "$tmpfile" \
    -f "$tmpdir/%s.json" -q "$failedfile_dir" \
    Rscript --vanilla "$run_opm" -z -p "$np" -a smooth -b 0 -r json \
    -d "$tmpdir" -i '*.csv' -k 'TIME:Setup Time,ID' >> "$outfile" &&
      cat "$tmpfile" >> "$errfile"

  echo "Testing CSV mode..."
  do_test -i csv -d "$testfile_dir" \
    -w "$testfile_dir/%s.tab" -l "$tmpfile" \
    -f "$tmpdir/%s.tab" -q "$failedfile_dir" \
    Rscript --vanilla "$run_opm" -z -p "$np" -a fast -b 0 -r csv -d "$tmpdir" \
    -u ';' -i '*.csv' -k 'TIME:Setup Time,ID' >> "$outfile" &&
      cat "$tmpfile" >> "$errfile"

  rm -rf "$tmpdir" "$tmpfile"

  echo
  printf "RESULT: "
  printf "`grep -F -c '<<<SUCCESS>>>' "$outfile"` successes, "
  printf "`grep -F -c '<<<FAILURE>>>' "$outfile"` failures, "
  printf "`grep -F -c '<<<ERROR>>>' "$outfile"` errors, "
  echo "`ls "$failedfile_dir" | wc -l` quarantined files."
  echo
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
      awk -v FS="\t" '$4 > 1' "$timings" | sort -nk4 -
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
  local pat='v; s/[ \t\v\a\f]\+$//'
  find . -type f -name '*.R' -exec sed -i "$pat" \{\} +
  local tmpfile=`mktemp --tmpdir`
  local rnw_file
  for rnw_file in `find . -type f -name '*.Rnw'`; do
    # this avoids unnecessary updates, which would trigger warnings once these
    # Rnw files were compared with the according PDF files
    sed "$pat" "$rnw_file" > "$tmpfile" &&
      ! diff -q "$tmpfile" "$rnw_file" > /dev/null && mv "$tmpfile" "$rnw_file"
  done
  rm -f "$tmpfile"
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


# Clean up directories left over by R CMD check. Rescue the manuals, if any.
#
remove_R_CMD_check_dirs()
{
  mkdir -p "$RESCUED_MANUALS" &&
    find . -type f -wholename '*.Rcheck/*-manual.pdf' \
      -exec mv -vt "$RESCUED_MANUALS" \{\} + || return 1
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


# Remove graphics files dynamically generated by vignette code.
#
remove_generated_graphics()
{
  local rnw_file
  for rnw_file in `find . -type f -iname '*.Rnw'`; do
    rm -f "${rnw_file%.*}"-*
  done
}


################################################################################


# Run R CMD Stangle on all vignette code files.
#
run_Stangle()
{
  local indir
  for indir; do
    find "$indir" -type f -name '*.Rnw' -exec R CMD Stangle \{\} \;
  done
}


################################################################################


# Extract R code within the examples of the given files.
#
extract_examples()
{
  if [ $# -eq 0 ]; then
    echo "No file names given, returning now." >&2
    return 1
  fi
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
  if [ $# -eq 0 ]; then
    echo "No function names given, returning now." >&2
    return 1
  fi
  find . -name '*-Ex.Rout' -type f -exec awk -v query="$*" '
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
  for pattern in .RData .Rhistory "*.Rout" Rplots.pdf; do
    find . -type f -name "$pattern" -delete
  done
}


################################################################################


# Run the R code in the files delivered with opm as demos.
#
test_demos()
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
  for rscript in "$wdir"/"$1"/demo/*.R; do
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
  if [ $# -eq 0 ]; then
    echo "No file names given, returning now." >&2
    return 1
  fi
  awk '/[^\r -~]/ {
    printf "%s:%i\t%s\n", FILENAME, FNR, $0
  }' "$@"
}


################################################################################


# Simple output utility used by test_sql().
#
print_test_result()
{
  local result
  [ "$1" -gt 0 ] && result=FAILURE || result=SUCCESS
  [ $# -gt 1 ] && echo "* $2 TEST: $result ($3)" ||
    echo "*** TESTS: $result ***"
  echo
}


################################################################################


# Test the database I/O functions of opmDB. Fails unless the test databases are
# accessible.
#
test_sql()
{
  local default_dbname=pmdata
  local sqlite3_dbname=$MISC_DIR/$default_dbname.db
  local mysql_dbname=$default_dbname
  local postgresql_dbname=$default_dbname
  local help_msg=

  local opt
  OPTIND=1
  while getopts hm:p:s: opt; do
    case $opt in
      h ) help_msg=yes;;
      m ) mysql_dbname=$OPTARG;;
      p ) postgresql_dbname=$OPTARG;;
      s ) sqlite3_dbname=$OPTARG;;
      * ) return 1;;
    esac
  done
  shift $(($OPTIND - 1))

  if [ "$help_msg" ]; then
    cat >&2 <<-____EOF
	Test the SQL included in the opmDB package. Requires access to SQLite, MySQL
	and/or PostgreSQL databases with read/write access for the current user.

	Options:
	  -h    Print this message.
	  -m x  Use MySQL database x.
	  -p x  Use PostgreSQL database x.
	  -s x  Use SQLite database x.

	The default database name is 'pmdata'. An empty database name turns off the
	according test. Unless explicitly provided, SQL files to test are search in
	the 'opmDB_in' directory.

____EOF
    return 1
  fi

  local errs=0
  local outcome
  local infile
  if [ "$sqlite3_dbname" ]; then
    local dirpart=${sqlite3_dbname%/*}
    [ "$dirpart" != "$sqlite3_dbname" ] && mkdir -p "$dirpart"
    for infile; do
      sqlite3 -bail -batch "$sqlite3_dbname" < "$infile" > /dev/null &&
        outcome=0 || outcome=1
      print_test_result $outcome SQLITE3 "$infile"
      errs=$((errs + outcome))
    done
  fi
  if [ "$mysql_dbname" ]; then
    for infile; do
      mysql --show-warnings -B -s "$mysql_dbname" < "$infile" > /dev/null &&
        outcome=0 || outcome=1
      print_test_result $outcome MYSQL "$infile"
      errs=$((errs + outcome))
    done
  fi
  if [ "$postgresql_dbname" ]; then
    for infile; do
      psql -q -1 -o /dev/null -d "$postgresql_dbname" -f "$infile" &&
        outcome=0 || outcome=1
      print_test_result $outcome POSTGRESQL "$infile"
      errs=$((errs + outcome))
    done
  fi

  return $errs
}


################################################################################


# Crop PDF given files and reduce their size with qpdf. Input files are
# modified. When using Ubuntu, pdfcrop is available in the texlive-extra-utils
# package; qpdf is directly available as package.
#
reduce_pdf_size()
{
  if [ $# -eq 0 ]; then
    echo "No file names given, returning now." >&2
    return 1
  fi
  local infile
  local tmpfile=`mktemp --tmpdir`
  for infile; do
    if pdfcrop "$infile" "$tmpfile" > /dev/null && qpdf "$tmpfile" "$infile"
    then
      rm -f "$tmpfile"
    else
      rm -f "$tmpfile"
      return 1
    fi
  done
}


################################################################################


# If any PDF files with examples have been produced, open them in a PDF viewer
# in the background.
#
show_example_pdf_files()
{
  local pdf_viewer
  local name
  for name in evince acroread; do
    pdf_viewer=`which "$name"`
    [ -x "$pdf_viewer" ] && break
  done
  if [ ! -x "$pdf_viewer" ]; then
    echo "no PDF viewer found, returning now" >&2
    return 1
  fi
  local pdf_file
  for pdf_file in `find . -type f -name '*-Ex.pdf'`; do
    "$pdf_viewer" "$pdf_file" &
  done
}


################################################################################


# Remove duplicates from whitelists, keep only lines with one word, and sort the
# result.
#
clean_whitelists()
{
  local tmpfile=`mktemp --tmpdir`
  local infile
  for infile in "$WHITELIST_MANUAL" "$WHITELIST_VIGNETTE"; do
    awk 'NF == 1 {print $1}' "$infile" | sort -u - > "$tmpfile" &&
      mv "$tmpfile" "$infile"
  done
  rm -f "$tmpfile"
}


################################################################################


# Check the spelling of the vignette files. We assume aspell is available.
# Specific LaTeX commands are excluded from spell checking.
#
spellcheck_vignettes()
{
  local pkg
  for pkg; do
    R --vanilla <<-____EOF
	words <- readLines("$WHITELIST_VIGNETTE")
	saveRDS(words[nzchar(words)], tmpfile <- tempfile(fileext = ".rds"))
	ctrl <- c("ac", "acf", "acs", "acl")
	ctrl <- c(ctrl, paste0(ctrl, "p"))
	ctrl <- c(ctrl, "bibliography", "code", "citep", "pkg", "proglang", "German",
	  "Plainauthor", "Plainkeywords", "Plaintitle", "Sexpr", "Surname")
	ctrl <- c("DefineVerbatimEnvironment oppp", "acrodef op", "subsection o",
	  "subsubsection o", "pdfbookmark opp", "item o", sprintf("%s op", ctrl))
	ctrl <- c("-t", "-d en_GB", sprintf("--add-tex-command=\"%s\"", ctrl))
	x <- aspell(Sys.glob(file.path("$pkg", "vignettes", "*.Rnw")),
	  filter = "Sweave", dictionaries = tmpfile, control = ctrl)
	if (nrow(x)) {
	  x <- x[order(x[, "Original"]), c("Original", "File", "Line", "Column")]
	  names(x) <- sprintf(".%s.", names(x))
	  write.table(x, "", sep = "\t", quote = FALSE, row.names = FALSE)
	}
____EOF
  done | awk -v FS="\t" 'NF > 1' -
}


################################################################################


# Reduce the Rnw files to stubs. Needed if the original Rnw files cannot be
# copied in the pkg directory (because they should only be checked locally).
#
reduce_vignette_Rnw_files()
{
  local indir
  local vignette
  local pdf_file
  local tmpdir=`mktemp -d --tmpdir`
  for indir; do
    for vignette in "$indir"/vignettes/*.Rnw; do
      [ -f "$vignette" ] || continue 2
      awk -v found=0 '
          /^%\\VignetteIndexEntry/ {
            print
            print "%% need no \\usepackage{Sweave.sty}"
            print "\\documentclass{article}"
            print "\\begin{document}"
            print "%% Rnw document stub because checking is disabled on CRAN"
            print "..."
            print "\\end{document}"
            found = 1
          }
          END {
            if (!found)
              exit 1
          }
        ' "$vignette" > "$tmpdir/${vignette##*/}"
      # to be safe in cases where PDF files have not yet been moved
      pdf_file=${vignette%.*}.pdf
      if [ -s "$pdf_file" ]; then
        mkdir -p "$indir/inst/doc" &&
          mv -v "$pdf_file" "$indir/inst/doc/${pdf_file##*/}"
      else
        pdf_file=$indir/inst/doc/${pdf_file##*/}
        if [ ! -s "$pdf_file" ]; then
          echo "file '$pdf_file' does not exist or is empty" >&2
          return 1
        fi
      fi
    done
    rm -fv "$indir"/vignettes/*
    mv "$tmpdir"/* "$indir"/vignettes/
  done
  rmdir "$tmpdir"
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
  cran )
    for mode in test demo sql time plex spell; do
      "$0" "$mode" || :
    done
    exit $?
  ;;
  demo )
    test_demos opm_in
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
    remove_generated_graphics && remove_R_CMD_check_dirs &&
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
	$0 -- build or test the opm package or one of its auxiliary packages.

	This script must be executed in the parent directory of the project's R
	package source directories.

	Usage: $0 [mode] [options]

	Possible values for 'mode':
	  ascii   Show lines that contain forbidden characters (such as non-ASCII).
	  bnorm   Normal build of the opmDB package.
	  cran    Run in all modes that should be run before a CRAN submission.
	  demo    Test the demo code that comes with opm.
	  dfull   Full build of the opmdata package.
	  dnorm   Normal build of the opmdata package.
	  docu    Show the 'docu.R' script to use if it can be found, then exit.
	  erase   Remove directories left over by R CMD check and 'docu.R', if any.
	  example Extract R code from the examples within specified Rd files.
	  forget  Remove all .RData, .Rhistory and *.Rout files found.
	  full    Full build of the opm package.
	  help    Print this message.
	  norm    [DEFAULT] Normal build of the opm package.
	  plex    Open the PDF files with plots produced from the examples, if any.
	  pdf     Reduce size of PDF files either in ./graphics or given as arguments.
	  pfull   Full build of the pkgutils package.
	  pnorm   Normal build of the pkgutils package.
	  rnw     Run R CMD Stangle on all *.Rnw files found.
	  rout    Show results of the examples, if any, for given function names.
	  space   Remove trailing whitespace from all R and Rnw code files found.
	  spell   Check spelling in the vignette files (see below for the Rd files).
	  sql     SQL-based tests. Call '$0 sql -h' for a description.
	  tags    Get list of Roxygen2 tags used, with counts of occurrences.
	  test    Test the 'run_opm.R' script. Call '$0 test -h' for details.
	  time    Show the timings of the last examples, if any, in order.
	  todo    Show TODO entries (literally!) in all R source files found.

	In contrast to a normal build, a full build includes copying to the local
	copy of the pkg directory. Other details of the build process depend on the
	options.

	In the normal and full running modes, all options go to 'docu.R'. Use '-h'
	after the mode name to display all of them. Missing options in these running
	modes imply generating the test copy of the package directory and the
	documentation and running the checks implemented in this script, but not
	those via 'docu.R'.

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
	that the installed 'docu.R' script is found. The same holds for updates in
	this build script that refer to novel command-line options of the 'docu.R'
	script.

____EOF
    exit 1
  ;;
  pdf )
    [ $# -eq 0 ] && set -- `find graphics -type f -iname '*.pdf'`
    reduce_pdf_size "$@"
    exit $?
  ;;
  pfull|pnorm )
    PKG_DIR=pkgutils_in
    RUNNING_MODE=${RUNNING_MODE#p}
    CHECK_R_TESTS=
  ;;
  plex )
    show_example_pdf_files
    exit $?
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
  spell )
    clean_whitelists
    spellcheck_vignettes opm_in
    exit $?
  ;;
  sql )
    set -- "$@" -- `find opmDB_in -type f -iname '*.sql' -exec ls \{\} +`
    test_sql "$@" && test_demos opmDB_in
    exit $?
  ;;
  tags )
    count_roxygen_tags
    exit $?
  ;;
  test )
    run_external_tests "$@"
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
Rscript --vanilla "$DOCU" "$@" --logfile "$LOGFILE" --lines-reduce \
  --no-internal --modify --preprocess --S4methods --junk "$delete_pat" \
  --mark-duplicates --good 00Index,well-map.R,substrate-info.R,plate-map.R \
  --whitelist "$WHITELIST_MANUAL" "$PKG_DIR"


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
    if mkdir -p "$target"; then
      reduce_vignette_Rnw_files "$OUT_DIR"
      cp -ru "$OUT_DIR"/* "$target" && rm -r "$OUT_DIR"
    fi
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


