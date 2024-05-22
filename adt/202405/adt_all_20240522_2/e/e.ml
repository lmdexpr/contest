open Core
open Scanf

let s, t = scanf "%s %s" Tuple2.create

let n = String.length s

let rec solve i =
  if i = n || Char.(s.[i] <> t.[i]) then i + 1
  else 
    solve (i + 1)

let ans = solve 0

let () = printf "%d\n%!" ans
