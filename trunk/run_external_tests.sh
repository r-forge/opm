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
# (C) 2012 by Markus Goeker (markus [DOT] goeker [AT] dsmz [DOT] de)
#
################################################################################


# Checked locations of the R installation directory. Names must not contain
# whitespace. The 'run_opm.R' script is either found in the $PATH or searched
# within these directories, in order.
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


# This directory must exist, be readable and writeable and must contain a
# subdirectory called 'tests'
#
TESTDIR=external_tests


################################################################################
#
# It should not be necessary to change anything below this line
#


set -eu


find_run_opm_script()
{
  local result=`which run_opm.R`
  if [ "$result" ]; then
    echo "$result"
    return 0
  fi
  local r_library
  for r_library in $R_LIBRARIES; do
    result=$r_library/opm/scripts/run_opm.R
    if [ -s "$result" ]; then
      echo "$result"
      return 0
    fi
  done
  echo
  return 1
}


# Run tests based on file comparison. Ths function originates from a larger
# Shell library and thus contains a usage message, even though it is not
# relevant here.
#
do_test()
{
  local help_msg= test_dir=tests in_ext=txt out_ext=out q_dir=
  local err_file=tests.err out_file=tests.out outfmt=

  local opt
  OPTIND=1
  while getopts d:f:hi:o:q: opt; do
    case $opt in
      d ) test_dir=$OPTARG;;
      f ) outfmt=$OPTARG;;
      h ) help_msg=yes;;
      i ) in_ext=$OPTARG;;
      o ) out_ext=$OPTARG;;
      q ) q_dir=$OPTARG;;
      * ) return 1;;
    esac
  done
  shift $(($OPTIND - 1))

  if [ $# -eq 0 ] || [ "$help_msg" ]; then
    cat >&2 <<-____EOF
	$FUNCNAME runs tests based on the comparison of specified input and output
	files.

	Usage: $FUNCNAME [options] arguments

	Options:
	  -d x  Assume input and output files in this directory.
	  -f x  Use x as template for the output filenames. Should use '%s' to
	        refer to the respective input filename.
	  -h    Print this message.
	  -i x  Use x as input file extension.
	  -o x  Use x as output file extension.
	  -q x  Copy unexpected output files in quarantine directory x.

	Arguments should start with the name of a script file or command, followed
	by its command-line arguments. The respective input file will be appended
	as the last argument.

____EOF
    return 1
  fi

  if ! [ -d "$test_dir" ]; then
    echo "Directory $test_dir does not exist, exiting..." >&2
    return 1
  fi

  local infile wantfile
  local stdout_output=`mktemp`
  local gotfile=$stdout_output

  for infile in "$test_dir"/*."$in_ext"; do
    echo
    wantfile=${infile%.$in_ext}.$out_ext
    echo "TESTING $infile => $wantfile ..."
    if "$@" "$infile" > "$stdout_output"; then
      if [ "$outfmt" ]; then
        gotfile=${infile%.$in_ext}
        gotfile=${gotfile##*/}
        gotfile=`printf "$outfmt" "$gotfile"`
      fi
      if diff -q "$wantfile" "$gotfile"; then
        echo "	<<<SUCCESS>>>"
        echo
      else
        echo "	<<<FAILURE>>>"
        echo
        if [ "$q_dir" ]; then
          mkdir -p "$q_dir"
          mv "$gotfile" "$q_dir"
        else
          diff "$wantfile" "$gotfile" || true
        fi
      fi
    else
      echo "	<<<ERROR>>>"
      echo
    fi
    [ "$outfmt" ] && rm -f "$gotfile"
  done 2> "$err_file" | tee "$out_file"

  rm -f "$stdout_output"

  echo "RESULT:\
    `grep -F -c '<<<SUCCESS>>>' "$out_file"` successes,\
    `grep -F -c '<<<FAILURE>>>' "$out_file"` failures,\
    `grep -F -c '<<<ERROR>>>' "$out_file"` errors"
  echo
  echo
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


tmpdir=`mktemp -d`


cd "$TESTDIR"


FAILED_FILES=failed_files # within $TESTDIR, created if necessary
[ -d "$FAILED_FILES" ] && rm -rf "$FAILED_FILES"/* || mkdir "$FAILED_FILES"


# plot mode
#
do_test -i csv -o ps -f "$tmpdir/%s.ps" -q "$FAILED_FILES" \
  Rscript --vanilla "$run_opm" -p 2 -r plot -d "$tmpdir" -i '*.csv'

# split mode -- these tests only guarantee that if there is nothing to split
# the original file results
#
do_test -i csv -o csv -f "$tmpdir/%s-00001.csv" -q "$FAILED_FILES" \
  Rscript --vanilla "$run_opm" -p 2 -s , -r split -d "$tmpdir" -i '*.csv'

# template collection mode
#
do_test -i csv -o template -f "$tmpdir/md.csv" -q "$FAILED_FILES" \
  Rscript --vanilla "$run_opm" -p 2 -r template -m "$tmpdir/md.csv" -i '*.csv'

# template collection mode with other field separator
#
do_test -i csv -o template2 -f "$tmpdir/md.csv" -q "$FAILED_FILES" \
  Rscript --vanilla "$run_opm" -p 2  -s , -r template -m "$tmpdir/md.csv" \
  -i '*.csv'

# yaml mode
#
do_test -i csv -o yml -f "$tmpdir/%s.yml" -q "$FAILED_FILES" \
  Rscript --vanilla "$run_opm" -z -p 2 -a -b 0 -f -r yaml -d "$tmpdir" \
  -i '*.csv'


rm -rf "$tmpdir"


################################################################################


