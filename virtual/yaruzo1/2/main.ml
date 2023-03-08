open Core
open Scanf

let a, b, k = scanf "%d %d %d" Tuple3.create

let op x y =
  let x = x - Bool.to_int (x % 2 = 1) in
  y + x / 2, x / 2

let a, b = Iter.(1 -- k) |> Iter.fold (fun (a, b) _ -> op a b) (a, b)
let a, b = if k % 2 = 0 then a, b else b, a

let () = printf "%d %d\n%!" a b
