#! /bin/bash

git rev-list ${1:-master} | while read rev; do git ls-tree -lr $rev | cut -c54- | grep -v '^ '; done | sort -u | perl -e '
  while (<>) {
    chomp;
    @stuff=split("\t");
    $sums{$stuff[1]} += $stuff[0];
  }
  print "$sums{$_} $_\n" for (keys %sums);
' | sort -rn >> large_files.txt
