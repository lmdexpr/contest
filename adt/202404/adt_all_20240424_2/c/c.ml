open Core
open Scanf

let n = scanf "%d" Fn.id
let s = scanf " %s" Fn.id

let () =
  for i = 1 to n - 1 do
    Iter.(0 -- (n - i - 1))
    |> Iter.take_while Char.(fun j -> s.[j] <> s.[j + i])
    |> Iter.length
    |> printf "%d\n"
  done
