open Core
open Scanf

let a, b, x = scanf "%d %d %d" Tuple3.create |> Tuple3.map ~f:float

let ans = 
  let open Float in
  let tan_theta =
    if x < a * a * b / 2. then 
      b / ((2. * x) / (a * b))
    else
      (2. * b - 2. * x / (a * a)) / a
  in
  atan tan_theta /. pi *. 180.

let () = printf "%.10f\n%!" ans
