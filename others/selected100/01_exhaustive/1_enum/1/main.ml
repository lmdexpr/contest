(* https://judge.u-aizu.ac.jp/onlinejudge/description.jsp?id=ITP1_7_B&lang=ja *)

open Scanf
open Printf

let solve n x =
  let count = ref 0 in
  for a = 1 to n - 2 do
    for b = a + 1 to n - 1 do
      let  c = x - a - b in
      if b < c && c <= n then
        incr count
    done
  done;
  printf "%d\n" !count

let () =
  let rec loop n x =
    if n = 0 && x = 0 then ()
    else
      (solve n x; scanf " %d %d" loop)
  in
  scanf "%d %d" loop
