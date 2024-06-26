open Core
open Scanf

let x, k = scanf "%Ld %d" Tuple2.create

let ans =
  Iter.(0 -- (k - 1))
  |> Iter.fold Int64.(fun acc i ->
    let p = pow 10L (of_int i + 1L) in
    p * (acc / p + if (acc % p) / (p / 10L) >= 5L then 1L else 0L)
  ) x

let () = printf "%Ld\n%!" ans
