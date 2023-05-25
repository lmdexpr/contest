open Core
open Scanf

let n = scanf "%d" ident

let is_prime n =
  let sqrt = float n |> sqrt |> Float.round_up |> int_of_float in
  Iter.(2 -- sqrt) |> Iter.for_all (fun x -> n % x <> 0)

let yes = is_prime n
let ans = if yes then "YES" else "NO"

let () = printf "%s\n%!" ans
