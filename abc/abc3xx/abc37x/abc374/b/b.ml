open Core
open Scanf

let s = scanf  "%s" Fn.id
let t = scanf " %s" Fn.id

let n = String.length s
let m = String.length t

let rec solve i =
  if n = i && m = i then 0
  else if n <= i || m <= i then i + 1
  else if Char.(s.[i] <> t.[i]) then i + 1
  else 
    solve (i + 1)

let ans = solve 0

let () = printf "%d\n%!" ans
