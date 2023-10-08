#!/bin/sh

dir="$1"; shift

template_path=`cd template && pwd`

mkdir -p $dir
pushd $dir

base_url="https://codeforces.com/contest/$1"; shift

for p; do
  mkdir $p
  pushd $p
  oj d $base_url/problem/$p
  cp $template_path/dune ./
  cp $template_path/main.ml ./
  popd
done

dune build

popd
