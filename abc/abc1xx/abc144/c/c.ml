open Core
open Scanf

let n = scanf "%Ld" ident

open Int64
let ans =
  Iter.(2 -- 1_000_000)
  |> Iter.map    of_int
  |> Iter.filter (fun x -> x * x <= n && n % x = 0L)
  |> Iter.map    (fun x -> x + n / x - 2L)
  |> Iter.min ~lt:(<)
  |> Option.value ~default:(n - 1L)

let () = printf "%Ld\n%!" ans
