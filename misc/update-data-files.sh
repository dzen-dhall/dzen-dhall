#!/usr/bin/env bash
# Find dhall files and put them to 'data-files' section of dzen-dhall.cabal

flag=0
files=`find ./dhall -type f | sed 's#./##' | sort`
echo "$files"

# pretty-print
result=`echo "$files" | while read line; do
  if [[ "$flag" == 0 ]]; then
     echo "        $line";
     flag=1
  else
     echo "                     $line";
  fi;
done`

# replace
perl -0777 -i.original -pe "s#data-files:.*library#data-files:  $result\nlibrary#igs" dzen-dhall.cabal
