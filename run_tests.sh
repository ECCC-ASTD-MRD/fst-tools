#!/bin/bash
[[ ! -x $1 ]] && echo "test program '$1' not found or not executable" && exit 1
[[ ! -r $2 ]] && echo "test file '$2' not found or no readable"
echo "running tests for '$1' using input file '$2'"
ulimit -s 16384000
for dirfile in test*.dir
do
  [[ -r $dirfile ]] || break
  echo "================== $dirfile =================="
  cat $dirfile
  echo "executing: ./$1 -s $2 -i $dirfile -dryrun -nobox -d rien.fst"
  ./$1 -s $2 -i $dirfile -dryrun -nobox -d rien.fst
done
echo "================== TESTS END =================="
