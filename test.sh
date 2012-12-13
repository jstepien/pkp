#!/bin/bash

pkp=build/pkp

if [[ ! -f $pkp ]]; then
  echo "$pkp is not installed, run ./waf install" 1>&2
  exit 1
fi

for img in images/*.png; do
  width=$(identify $img | sed -e 's/.*PNG //;s/x.*//')
  convert $img tmp.rgbo
  cat tmp.rgbo \
    | $pkp $width \
    | $pkp -d \
    2> width.tmp \
    | diff - tmp.rgbo
  if [[ x$? != x0 || x$(cat width.tmp) != x$width ]]; then
    echo "$img FAILED" 1>&2
  fi
  rm -f tmp.rgbo width.tmp
done
