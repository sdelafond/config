#!/usr/bin/env sh
INPUT="$1"
PDIR="$HOME/tmp/mutt_print"
OPEN_PDF=evince
 
# check to make sure that enscript and ps2pdf are both installed
if ! command -v enscript >/dev/null || ! command -v ps2pdf >/dev/null; then
    echo "ERROR: both enscript and ps2pdf must be installed" 1>&2
    exit 1
fi
 
# create temp dir if it does not exist
if [ ! -d "$PDIR" ]; then
    mkdir -p "$PDIR" 2>/dev/null
    if [ $? -ne 0 ]; then
        echo "Unable to make directory '$PDIR'" 1>&2
        exit 2
    fi
fi
 
#tmpfile="`mktemp $PDIR/mutt_XXXXXXXX.pdf`"
tmpfile="/tmp/mutt-msg.pdf"
iconv -c -f utf-8 -t ISO-8859-1 $INPUT | enscript -G -p - 2>/dev/null | ps2pdf - $tmpfile
