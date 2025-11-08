open Core
open Scanf

let n = scanf " %d" Fn.id
let k = scanf " %d" Fn.id

let ans =
  Iter.(1 -- n) |> Iter.flat_map (fun r ->
    Iter.(1 -- n) |> Iter.filter (fun b ->
      let w = k - r - b in
      1 <= w && w <= n
    )
  )
  |> Iter.length

let () = printf "%d\n%!" ans
