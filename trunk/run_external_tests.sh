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


# It should not be necessary to change anything below this line. See the help
# message for the possible command-line options.
#
set -eu


################################################################################


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
  local r_dir=`R RHOME`
  local subdir
  for subdir in library site-library; do
    result=$r_dir/$subdir/opm/scripts/run_opm.R
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


# Conduct a certain kind of test based on file comparison.
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
    echo "arguments -d, -f, -i, -q and -w must be provided" >&2
    return 1
  fi

  "$@" "$indir"/*."$inext" 2> "$logfile" || true

  local infile wantfile gotfile
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


help_msg=
np=4 # using more cores yielded only little speedup
run_opm=
testdir=external_tests
version=opm_in/DESCRIPTION

OPTIND=1
while getopts d:hp:s:v: opt; do
  case $opt in
    d ) testdir=$OPTARG;;
    h ) help_msg=yes;;
    p ) np=$(($OPTARG + 0));;
    s ) run_opm=$OPTARG;;
    v ) version=$OPTARG;;
    * ) exit 1;;
  esac
done
shift $(($OPTIND - 1))


if [ "$help_msg" ] || [ $# -gt 0 ]; then
  cat >&2 <<-__EOF
	$0 -- test the opm package via its 'run_opm.R' script

	Options:
	  -d x  Use x as test directory (must contain subdirectory 'tests').
	  -h    Print this message.
	  -p x  Use x processors (cores).
	  -s x  Use x as 'run_opm.R' script.
	  -v x  Insert opm version x (can also be an R package DESCRIPTION file).

__EOF
  exit 1
fi


################################################################################


[ "$run_opm" ] || run_opm=`find_run_opm_script || :`
if [ -s "$run_opm" ]; then
  echo "Using script '$run_opm'..." >&2
  echo "WARNING: ensure this is the opm version you want to test" >&2
  echo >&2
else
  echo "script 'run_opm.R' not found and not provided, exiting now" >&2
  exit 1
fi

if [ "$version" ]; then
  [ -s "$version" ] &&
    version=`awk '$1 == "Version:" {print $2; exit}' "$version"`
else
  echo "opm version to insert not found and not provided, exiting now" >&2
  exit 1
fi

testfile_dir=$testdir/tests
if ! [ -d "$testfile_dir" ]; then
  echo "directory '$testfile_dir' does not exist, exiting now" >&2
  exit 1
fi


failedfile_dir=$testdir/failed_files # within $testdir, created if necessary
[ -d "$failedfile_dir" ] && rm -rf "$failedfile_dir"/* ||
  mkdir "$failedfile_dir"
errfile=$testdir/tests.err
outfile=$testdir/tests.out
rm -f "$errfile" "$outfile"


tmpdir=`mktemp --tmpdir -d`
tmpfile=`mktemp --tmpdir`


# Update the version to let the YAML and JSON tests pass the test irrespective
# of the actual version. This must later on be reversed, see below.
#
change_yaml_version "$version" "$testfile_dir"/*.yml
change_json_version "$version" "$testfile_dir"/*.json


################################################################################


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
  Rscript --vanilla "$run_opm" -p "$np" -s , -r split -d "$tmpdir" -i '*.csv' \
  -k 'TIME:Setup Time,ID' >> "$outfile" &&
    cat "$tmpfile" >> "$errfile" 

echo "Testing template-collection mode..."
do_test -i csv -d "$testfile_dir" \
  -w "$testfile_dir/md.template" -l "$tmpfile" \
  -f "$tmpdir/md.template" -q "$failedfile_dir" \
  Rscript --vanilla "$run_opm" -p "$np" -r template -m "$tmpdir/md.template" \
  -i '*.csv' >> "$outfile" &&
    cat "$tmpfile" >> "$errfile"

echo "Testing template-collection mode with other field separator..."
do_test -i csv -d "$testfile_dir" \
  -w "$testfile_dir/md.template2" -l "$tmpfile" \
  -f "$tmpdir/md.template2" -q "$failedfile_dir" \
  Rscript --vanilla "$run_opm" -p "$np" -r template -m "$tmpdir/md.template2" \
  -s , -i '*.csv' -k 'TIME:Setup Time,ID' >> "$outfile" &&
    cat "$tmpfile" >> "$errfile"

echo "Testing YAML mode..."
do_test -i csv -d "$testfile_dir" \
  -w "$testfile_dir/%s.yml" -l "$tmpfile" \
  -f "$tmpdir/%s.yml" -q "$failedfile_dir" \
  Rscript --vanilla "$run_opm" -z -p "$np" -a fast -b 0 -r yaml -d "$tmpdir" \
  -i '*.csv' -k 'TIME:Setup Time,ID' >> "$outfile" &&
    cat "$tmpfile" >> "$errfile"

echo "Testing JSON mode..."
do_test -i csv -d "$testfile_dir" \
  -w "$testfile_dir/%s.json" -l "$tmpfile" \
  -f "$tmpdir/%s.json" -q "$failedfile_dir" \
  Rscript --vanilla "$run_opm" -z -p "$np" -a smooth -b 0 -r json -d "$tmpdir" \
  -i '*.csv' -k 'TIME:Setup Time,ID' >> "$outfile" &&
    cat "$tmpfile" >> "$errfile"


################################################################################


rm -rf "$tmpdir" "$tmpfile"


echo
echo -n "RESULT: "
echo -n "`grep -F -c '<<<SUCCESS>>>' "$outfile"` successes, "
echo -n "`grep -F -c '<<<FAILURE>>>' "$outfile"` failures, "
echo -n "`grep -F -c '<<<ERROR>>>' "$outfile"` errors, "
echo "`ls "$failedfile_dir" | wc -l` quarantined files."
echo


# Fix the version in the YAML and JSON files to avoid SVN updates. Do this in
# the quarantined files, too, if any, to avoid annoying reports when manually
# calling diff.
#
change_yaml_version 0.0.0 "$testfile_dir"/*.yml
change_yaml_version 0.0.0 "$failedfile_dir"/*.yml 2> /dev/null || true
change_json_version 0.0.0 "$testfile_dir"/*.json
change_json_version 0.0.0 "$failedfile_dir"/*.json 2> /dev/null || true


################################################################################


