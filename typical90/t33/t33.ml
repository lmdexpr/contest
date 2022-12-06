open Core

let h, w = Scanf.scanf "%d %d" Tuple2.create

let ans =
  if h = 1 || w = 1 then h * w
  else
    ((h+1) / 2) * ((w+1) / 2)

let () = printf "%d\n%!" ans
