(* https://atcoder.jp/contests/arc054/tasks/arc054_b *)
open Core
open Scanf

let p = scanf "%f" ident

let five_point_diff ?(h=1e-9) f x =
  let y1 = f(x +. h)
  and y2 = f(x -. h)
  and y3 = f(x +. 2. *. h)
  and y4 = f(x -. 2. *. h)
  in
  (y4 -. 8. *. y2 +. 8. *. y1 -. y3) /. (12. *. h)

let  f x = x +. (2. ** (-. 2. *. x /. 3.)) *. p
let df x = five_point_diff f x

let rec binsearch ?(count=50) ~ok left right =
  let open Float in
  if Int.(count < 0) || abs (right - left) <= 1e-9 then right
  else
    let mid = (right + left) / 2. in
    let left, right = if ok mid then left, mid else mid, right in
    binsearch ~count:(Int.pred count) ~ok left right

let ok x = Float.(df x > 0.)
let ans =
  if Float.(df 0. * df 100. > 0.) then p
  else
    f @@ binsearch ~ok 0. 100.

let () = printf "%.10f\n%!" ans
