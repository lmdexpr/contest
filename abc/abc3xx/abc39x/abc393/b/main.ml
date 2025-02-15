open Core
open Scanf

let s = scanf " %s" Fn.id
let n = String.length s

let ans =
  Iter.(0 -- (n - 1))
  |> Iter.filter Char.(fun i -> s.[i] = 'A')
  |> Iter.flat_map (fun i ->
    Iter.((i + 1) -- (n - 1))
    |> Iter.filter Char.(fun j -> s.[j] = 'B')
    |> Iter.map (fun j -> i, j)
  )
  |> Iter.flat_map (fun (i, j) ->
    Iter.((j + 1) -- (n - 1))
    |> Iter.filter Char.(fun k -> s.[k] = 'C')
    |> Iter.map (fun k -> i, j, k)
  )
  |> Iter.filter (fun (i, j, k) -> (j - i) = (k - j))
  |> Iter.length

let () = printf "%d\n%!" ans
