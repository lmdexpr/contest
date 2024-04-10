open Core
open Scanf

let a, b = scanf "%f %f" Tuple2.create

let d = sqrt (a *. a +. b *. b)

let x, y = a /. d, b /. d

let () = printf "%.12f %.12f\n%!" x y
