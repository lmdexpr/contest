open Core
open Scanf

let s = scanf  "%s" Fn.id
let t = scanf " %s" Fn.id

let n = String.length s

let rec solve i =
  if i = n then n + 1 else
  if Char.(s.[i] <> t.[i]) then i + 1 else solve (i + 1)

let ans = solve 0

let () = printf "%d\n%!" ans

