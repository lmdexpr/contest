open Core
open Scanf

let b = scanf "%Ld" Fn.id

let a =
  Iter.(1 -- 15)
  |> Iter.map Int64.of_int
  |> Iter.map Int64.(fun i -> i, pow i i)
  |> Iter.to_array

let ans = Array.find a ~f:Int64.(fun (_, x) -> x = b)
let ans =
  match ans with
  | Some (i, _) -> i
  | None        -> -1L

let () = printf "%Ld\n%!" ans
