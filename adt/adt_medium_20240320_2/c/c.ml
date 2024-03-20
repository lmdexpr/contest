open Core
open Scanf

let n, m, t = scanf "%d %d %d" Tuple3.create

let a = Array.create ~len:(n+1) 0
let b = Array.create ~len:(n+1) 0
let () =
  for i = 1 to n - 1 do
    scanf " %d" (fun x -> a.(i) <- x)
  done;
  for _ = 1 to m do
    let x, y = scanf " %d %d" Tuple2.create in
    b.(x) <- y
  done

let yes =
  let rec go t i =
    if n <= i then true
    else if t <= a.(i) then false
    else 
      go (t - a.(i) + b.(i + 1)) (i + 1)
  in
  go t 1

let ans = if yes then "Yes" else "No"

let () = printf "%s\n%!" ans
