#!/bin/sh

dir=$1
url=$2

path=`cd ../../template && pwd`

mkdir $dir
pushd $dir

cp $path/dune ./
cp $path/main.ml ./

dune build

oj download $url
