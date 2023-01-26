open Core
open Scanf

let x, k = scanf "%Ld %d" Tuple2.create

let ans =
  Iter.(1 -- k)
  |> Iter.fold
    Int64.(fun x _ -> (x + 5L) / 10L)
    x
let ans = Int64.(ans * pow 10L (of_int k))

let () = printf "%Ld\n%!" ans
