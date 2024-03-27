open Core
open Scanf

let n, m, t = scanf "%d %d %d" Tuple3.create
let a = Array.init (n-1) ~f:(fun _ -> scanf " %d" Fn.id)
let b = Array.create ~len:n 0
let () =
  for _ = 0 to m - 1 do
    let x, y = scanf " %d %d" Tuple2.create in
    b.(x-1) <- y
  done

let rec solve ?(t = t) i =
  if i >= n - 1 then true
  else 
    let t = t - a.(i) in
    let i = i + 1 in 
    if t <= 0 then false
    else 
      let t = t + b.(i) in
      solve ~t i

let ans = if solve 0 then "Yes" else "No"
let () = printf "%s\n%!" ans
