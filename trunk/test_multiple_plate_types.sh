#!/bin/sh


set -eu


test_multiple_plate_types()
{
  local indir=`pwd`/
  indir=${indir%/trunk/*}/trunk
  local rscript=$indir/opm_in/inst/examples/multiple_plate_types.R
  local wdir=`mktemp -d`
  mkdir -p "$wdir" && cd "$wdir"
  local csv_file
  for csv_file in "$indir"/external_tests/tests/*.csv; do
    case $csv_file in
      *Multiline* ) continue;;
    esac
    ln -sf "$csv_file" "${csv_file##*/}"
  done
  if R CMD BATCH "$rscript"; then
    echo "*** TEST SUCCEEDED ***" >&2
    cd ..
    rm -rf "$wdir"
  else
    echo "*** TEST FAILED ***" >&2
    tail "${rscript##*/}out"
    cp "${rscript##*/}out" ..
    cd ..
    rm -rf "$wdir"
    return 1
  fi
}




test_multiple_plate_types



