open Core
open Scanf

let n   = scanf "%d" Fn.id
let pos = Array.init n ~f:(fun _ -> scanf " %f %f" Tuple2.create)

let ans, (x, y) =
  Array.fold pos ~init:(0., (0., 0.)) ~f:(fun (acc, (a, b)) (c, d) ->
    Float.(acc + sqrt ((a - c) ** 2. + (b - d) ** 2.)), (c, d)
  )

let ans = Float.(ans + sqrt (x ** 2. + y ** 2.))

let () = printf "%f\n%!" ans
