(* https://judge.u-aizu.ac.jp/onlinejudge/description.jsp?id=ALDS1_10_A&lang=ja *)
open Scanf
open Printf

let n = scanf "%d" @@ fun n -> n

let dp = Array.make (n + 1) 0
let () =
  dp.(0) <- 1;
  dp.(1) <- 1;
  for i = 2 to n do
    dp.(i) <- dp.(i - 1) + dp.(i - 2)
  done

let ans = dp.(n)

let () = printf "%d\n%!" ans
