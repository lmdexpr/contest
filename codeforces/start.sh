#!/bin/sh

name=$1; shift
dir="$name"

template_path=`cd template && pwd`

mkdir -p $dir
pushd $dir

base_url=$2; shift

for p; do
  mkdir $p
  pushd $p
  cp $template_path/dune ./
  cp $template_path/main.ml ./
  popd
done

dune build

popd
