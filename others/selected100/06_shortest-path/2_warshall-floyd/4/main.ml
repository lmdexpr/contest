(* https://atcoder.jp/contests/abc074/tasks/arc083_b *)
open Core
open Scanf

let n = scanf "%d" ident

let a = Array.init n ~f:(fun _ -> Array.init n ~f:(fun _ -> scanf " %d" ident))

let b = Array.init n ~f:(fun i -> Array.copy a.(i))
let _warshall_floyd =
  for k = 1 to n do
    for i = 1 to n do
      for j = 1 to n do
        b.(i).(j) <- min b.(i).(j) (b.(i).(k) + b.(k).(j))
      done
    done
  done
  
let ans = 0

let () = printf "%d\n%!" ans
