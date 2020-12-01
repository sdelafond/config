#!/bin/bash

INPUT="$1"
TMPDIR="$HOME/tmp/mutt_print"
 
# check to make sure that enscript and ps2pdf are both installed
if ! command -v paps >/dev/null || ! command -v ps2pdf >/dev/null; then
  echo "ERROR: both paps and ps2pdf must be installed" 1>&2
  exit 1
fi
 
# create temp dir

if ! mkdir -p "$TMPDIR" 2>/dev/null ; then
  echo "Unable to make directory '$TMPDIR'" 1>&2
  exit 2
fi
 
tmpfile=${TMPDIR}/mutt_$(date -Iseconds).pdf

cat $INPUT | paps --header --font="DejaVu Sans Mono" | ps2pdf - $tmpfile
