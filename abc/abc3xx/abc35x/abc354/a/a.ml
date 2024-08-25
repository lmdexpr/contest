open Core
open Scanf

let h = scanf "%d" Fn.id

let rec solve acc i =
  let acc = acc + Int.pow 2 i in
  let i   = i + 1 in
  if acc > h then printf "%d\n" i
  else solve acc i

let () = solve 0 0
