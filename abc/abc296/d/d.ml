open Core
open Scanf

let n, m = scanf "%Ld %Ld" Tuple2.create

open Int64

let ans =
  Iter.(1 -- 1_000_000)
  |> Iter.filter_map (fun a ->
      let a = of_int a in
      let b = (m + a - 1L) / a in
      Option.some_if (a <= n && b <= n) (a * b)
    )
  |> Iter.min ~lt:(<) |> Option.value ~default:(-1L)

let () = printf "%Ld\n%!" ans
