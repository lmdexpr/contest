(* unsolved *)
let cbrt = Float.cbrt

open Core
open Scanf

let n = scanf "%Ld" Float.of_int64

let ans =
  Iter.(1 -- Float.(cbrt n |> round_down |> to_int)) |> Iter.flat_map (fun a ->
    Iter.(a -- Float.(n / float a |> sqrt |> round_down |> to_int)) |> Iter.map (fun b ->
      let a = float a and b = float b in
      Float.(round_down (n / (a * b)) - b + 1.) |> Float.to_int64
    )
  )
  |> Iter.fold Int64.(fun acc x -> acc + x) 0L

let () = printf "%Ld\n%!" ans
