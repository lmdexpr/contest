#!/bin/sh

num=$1
dir="yaruzo$num"

path=`cd ../template && pwd`

mkdir $dir
pushd $dir

for ((i = 1; i <= 5; i++)); do
  mkdir $i
  pushd $i
  cp $path/dune ./
  cp $path/main.ml ./
  popd
done

dune build

popd
