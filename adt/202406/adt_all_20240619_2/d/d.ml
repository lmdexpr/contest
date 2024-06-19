open Core
open Scanf

let k, g, m = scanf "%d %d %d" Tuple3.create

let rec simulate i glass mug =
  if i > k then glass, mug
  else
    let i = i + 1 in
    if glass = g then simulate i 0 mug
    else if mug = 0 then simulate i glass m
    else
      if glass + mug < g then simulate i (glass + mug) 0
      else
        simulate i g (mug - (g - glass))

let g, m = simulate 1 0 0

let () = printf "%d %d\n%!" g m
