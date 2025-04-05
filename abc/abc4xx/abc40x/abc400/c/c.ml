open Core
open Scanf

let isqrt n =
  let open Big_int in
  big_int_of_int64 n
  |> sqrt_big_int
  |> int64_of_big_int

let n = scanf " %Ld" Fn.id
let ans =
  Iter.(1 -- 60)
  |> Iter.fold Int64.(fun acc a ->
    let pow2 = shift_left 1L in
    acc + (isqrt (n / pow2 a) + 1L) / 2L
  ) 0L

let () = printf "%Ld\n%!" ans
