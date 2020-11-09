#!/bin/bash
loopdirs=$*
a=0
rm -r mega.rhythmr/loops
mkdir -p mega.rhythmr/loops
for d in $(ls -d $loopdirs); do
  echo $a $d
  ln -s ../../$d mega.rhythmr/loops/$a
  a=$((a+1))
done
