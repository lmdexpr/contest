(* https://atcoder.jp/contests/joi2009ho/tasks/joi2009ho_b *)

open Core
open Scanf

let d, n, m = scanf "%d %d %d" Tuple3.create

let s = Array.init (n+1) ~f:(const 0)
let () =
  for i = 1 to n - 1 do
    scanf " %d" (Array.set s i)
  done;
  s.(n) <- d;
  Array.sort s ~compare

let k = Array.init m ~f:(fun _ -> scanf " %d" ident)

let ans = Array.fold k ~init:0 ~f:(fun acc k ->
    match Array.binary_search ~compare s `Last_less_than_or_equal_to k with
    | None   -> acc
    | Some i -> acc + min (k - s.(i)) (s.(i+1) - k)
  )

let () = printf "%d\n%!" ans
