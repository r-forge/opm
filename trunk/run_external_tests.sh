#!/bin/sh


################################################################################
#
# Script for testing the 'run_opm.R' script that comes with the opm package.
#
# One needs (of course) the 'run_opm.R' script from the opm R package to run
# this, as well as the 'Rscript' executable in the $PATH. The script itself was
# tested using Bash and Dash.
#
# This script is distributed under the terms of the Gnu Public License V2.
# For further information, see http://www.gnu.org/licenses/gpl.html
#
# (C) 2013 by Markus Goeker (markus [DOT] goeker [AT] dsmz [DOT] de)
#
################################################################################


# Checked locations of the R installation directory if it is not found in the
# result from calling R RHOME. Directory names must not contain whitespace.
# The 'run_opm.R' script is either found in the $PATH or searched within these 
# directories, in order.
#
FALLBACK_R_LIBRARY_DIRS="
  /usr/local/lib/R/library
  /usr/local/lib64/R/library
  /usr/lib64/R/library
  /usr/lib/R/library
  /usr/share/R/library
"


# This directory must exist, be readable and writeable and must contain a
# subdirectory called 'tests'
#
TESTDIR=external_tests


################################################################################
#
# It should not be necessary to change anything below this line
#


set -eu


# Find the script docu.R (from the pkgutils R package) either in the $PATH or
# within the pkgutils subdirectory of the R installation directory.
#
find_run_opm_script()
{
  local result=`which run_opm.R`
  if [ "$result" ]; then
    echo "$result"
    return 0
  fi
  local r_library_dirs=`R RHOME || :`
  if [ "$r_library_dirs" ]; then
    r_library_dirs=$r_library_dirs/library
  else
    r_library_dirs=$FALLBACK_R_LIBRARY_DIRS
  fi
  local r_library_dir
  for r_library_dir in $r_library_dirs; do
    result=$r_library_dir/opm/scripts/run_opm.R
    if [ -s "$result" ]; then
      echo "$result"
      return 0
    fi
  done
  echo
  return 1
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


# Run tests based on file comparison.
#
do_test()
{
  local qdir= outfmt= wantedfmt= indir= inext= logfile=/dev/stderr

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
    echo "arguments -d, -f, -im -q and -w must be provided" >&2
    return 1
  fi

  local infile wantfile gotfile
  local lastfile=

  mkdir -p "$qdir" || return 1

  "$@" "$indir"/*."$inext" 2> "$logfile" || true

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


num_items()
{
  echo $#
}


################################################################################


read_opm_version()
{
  awk '$1 == "Version:" {print $2; exit}' "$@"
}


################################################################################


change_json_version()
{
  local version=$1
  shift
  sed -i "v; s/\(\"version\"\):\"[^\"]\+\"/\1:\"$version\"/g" "$@"
}


################################################################################


change_yaml_version()
{
  local version=$1 tmpfile=`mktemp --tmpdir`
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


run_opm=`find_run_opm_script`
if [ "$run_opm" ]; then
  echo "using script '$run_opm'" >&2
  echo >&2
  echo "WARNING: ensure this is the opm version you want to test" >&2
  echo >&2
else
  echo "script 'run_opm.R' not found, exiting now" >&2
  exit 1
fi


if [ $# -eq 0 ]; then
  version=`read_opm_version opm_in/DESCRIPTION`
else
  version=$1
fi


tmpdir=`mktemp --tmpdir -d`
tmpfile=`mktemp --tmpdir`


testfile_dir=$TESTDIR/tests
failedfile_dir=$TESTDIR/failed_files # within $TESTDIR, created if necessary
[ -d "$failedfile_dir" ] && rm -rf "$failedfile_dir"/* ||
  mkdir "$failedfile_dir"
errfile=$TESTDIR/tests.err
outfile=$TESTDIR/tests.out
rm -f "$errfile" "$outfile"


# Update the version to let the YAML and JSON tests pass the test irrespective
# of the actual version. This must later on be set back, see below.
#
change_yaml_version "$version" "$testfile_dir"/*.yml
change_json_version "$version" "$testfile_dir"/*.json


################################################################################


echo "Testing plot mode..."
do_test -i csv -d "$testfile_dir" \
  -w "$testfile_dir/%s.ps" -l "$tmpfile" \
  -f "$tmpdir/%s.ps" -q "$failedfile_dir" \
  Rscript --vanilla "$run_opm" -p 2 -r xyplot -d "$tmpdir" -i '*.csv' \
  -k 'TIME:Setup Time,ID' >> "$outfile" &&
    cat "$tmpfile" >> "$errfile"

echo "Testing split mode..."
# This test only guarantees that if there is nothing to split the original
# file results
do_test -i csv -d "$testfile_dir" \
  -w "$testfile_dir"/%s.csv -l "$tmpfile" \
  -f "$tmpdir/%s-00001.csv" -q "$failedfile_dir" \
  Rscript --vanilla "$run_opm" -p 2 -s , -r split -d "$tmpdir" -i '*.csv' \
  -k 'TIME:Setup Time,ID' >> "$outfile" &&
    cat "$tmpfile" >> "$errfile" 

echo "Testing template-collection mode..."
do_test -i csv -d "$testfile_dir" \
  -w "$testfile_dir/md.template" -l "$tmpfile" \
  -f "$tmpdir/md.template" -q "$failedfile_dir" \
  Rscript --vanilla "$run_opm" -p 2 -r template -m "$tmpdir/md.template" \
  -i '*.csv' >> "$outfile" &&
    cat "$tmpfile" >> "$errfile"

echo "Testing template-collection mode with other field separator..."
do_test -i csv -d "$testfile_dir" \
  -w "$testfile_dir/md.template2" -l "$tmpfile" \
  -f "$tmpdir/md.template2" -q "$failedfile_dir" \
  Rscript --vanilla "$run_opm" -p 2 -r template -m "$tmpdir/md.template2" \
  -s , -i '*.csv' -k 'TIME:Setup Time,ID' >> "$outfile" &&
    cat "$tmpfile" >> "$errfile"

echo "Testing YAML mode..."
do_test -i csv -d "$testfile_dir" \
  -w "$testfile_dir/%s.yml" -l "$tmpfile" \
  -f "$tmpdir/%s.yml" -q "$failedfile_dir" \
  Rscript --vanilla "$run_opm" -z -p 2 -a fast -b 0 -r yaml -d "$tmpdir" \
  -i '*.csv' -k 'TIME:Setup Time,ID' >> "$outfile" &&
    cat "$tmpfile" >> "$errfile"

echo "Testing JSON mode..."
do_test -i csv -d "$testfile_dir" \
  -w "$testfile_dir/%s.json" -l "$tmpfile" \
  -f "$tmpdir/%s.json" -q "$failedfile_dir" \
  Rscript --vanilla "$run_opm" -z -p 2 -a smooth -b 0 -r json -d "$tmpdir" \
  -i '*.csv' -k 'TIME:Setup Time,ID' >> "$outfile" &&
    cat "$tmpfile" >> "$errfile"


################################################################################


rm -rf "$tmpdir" "$tmpfile" 

echo "RESULT:\
  `grep -F -c '<<<SUCCESS>>>' "$outfile"` successes,\
  `grep -F -c '<<<FAILURE>>>' "$outfile"` failures,\
  `grep -F -c '<<<ERROR>>>' "$outfile"` errors"
echo

num_failed=`num_items \`ls "$failedfile_dir"\``
echo "OVERALL RESULT (number of failed files): $num_failed"
echo


# Fix the version in the YAML and JSON files to avoid SVN updates. Do this in
# the failed files, too, if any, to avoid annoying reports when manually
# calling diff.
#
change_yaml_version 0.0.0 "$testfile_dir"/*.yml
change_yaml_version 0.0.0 "$failedfile_dir"/*.yml 2> /dev/null || true
change_json_version 0.0.0 "$testfile_dir"/*.json
change_json_version 0.0.0 "$failedfile_dir"/*.json 2> /dev/null || true


################################################################################



