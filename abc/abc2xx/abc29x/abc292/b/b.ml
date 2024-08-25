open Core
open Scanf

let n, q = scanf "%d %d" Tuple2.create

let yellow = Array.init (n+1) ~f:(const 0)
let red = Array.init (n+1) ~f:(const 0)

let () =
  for _ = 1 to q do
    let command, x = scanf " %d %d" Tuple2.create in
    match command with
    | 1 -> yellow.(x) <- yellow.(x) + 1
    | 2 -> red.(x) <- red.(x) + 1
    | _ -> printf "%s\n%!" @@ if yellow.(x) < 2 && red.(x) = 0 then "No" else "Yes"
  done
