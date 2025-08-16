open Core
open Scanf

let n = scanf " %d" Fn.id
let a = Array.init n ~f:(fun _ -> scanf " %d %d" Tuple2.create)

let min_r, max_r, min_c, max_c =
  Array.fold a ~init:(Int.max_value, Int.min_value, Int.max_value, Int.min_value)
    ~f:(fun (min_r, max_r, min_c, max_c) (r, c) ->
      (Int.min min_r r, Int.max max_r r, Int.min min_c c, Int.max max_c c)
    )

let r = (min_r + max_r) / 2
let c = (min_c + max_c) / 2

let ans =
  Array.fold a ~init:0 ~f:(fun acc (r', c') ->
    max acc @@ max (abs @@ r - r') (abs @@ c - c')
  )

let () = printf "%d\n%!" ans
