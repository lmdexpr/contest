open Core
open Scanf

let n, q = scanf "%d %d" Tuple2.create

let yellow = Array.create ~len:n 0
let red = Array.create ~len:n false

let () =
  for _ = 1 to q do
    match scanf " %d %d" Tuple2.create with
    | 1, x -> yellow.(x - 1) <- yellow.(x - 1) + 1
    | 2, x -> red.(x - 1) <- true
    | _, x -> 
      let x = x - 1 in
      let yes = yellow.(x) >= 2 || red.(x) in
      printf "%s\n" (if yes then "Yes" else "No")
  done
