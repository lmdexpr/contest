open Core
open Scanf

open Float
let a, b, x = scanf "%d %d %d" @@ fun a b x -> float a, float b, float x

let ans = 
  let half = a * a * b / 2. in
  let tan_theta =
    if x < half then
      b / ( (2. * x) / (a * b) )
    else
      ( 2. * b - 2. * x / (a * a) ) / a
  in
  atan tan_theta /. pi *. 180.

let () = printf "%.10f\n%!" ans
