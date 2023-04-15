(* https://judge.u-aizu.ac.jp/onlinejudge/description.jsp?id=ALDS1_4_B&lang=ja# *)
open Scanf
open Printf

let ident x = x

let n = scanf "%d" ident
let s = Array.init n (fun _ -> scanf " %d" ident)

let q = scanf " %d" ident
let t = Array.init q (fun _ -> scanf " %d" ident)

let rec binsearch ~ok left right =
  if abs (right - left) <= 1 then right
  else
    let mid = (right + left) / 2 in
    let left, right = if ok mid then left, mid else mid, right in
    binsearch ~ok left right

let ans =
  let f acc t =
    let i = binsearch ~ok:(fun i -> t < s.(i)) 0 n in
    acc + if s.(i - 1) = t then 1 else 0
  in
  Array.fold_left f 0 t

let () = printf "%d\n%!" ans
