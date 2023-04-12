(* https://atcoder.jp/contests/sumitrust2019/tasks/sumitb2019_d *)

open Core
open Scanf

let _n = scanf "%d" ident
let s = scanf " %s" ident

let (let*) x f = Option.bind ~f x
let greedy t =
  let* i = String.index_from s 0     t.[0] in
  let* j = String.index_from s (i+1) t.[1] in
  let* _ = String.index_from s (j+1) t.[2] in
  Some ()
let is_password = Fn.compose Option.is_some greedy

let ans = Iter.(0 -- 999) |> Iter.map (sprintf "%03d") |> Iter.filter_count is_password

let () = printf "%d\n%!" ans
