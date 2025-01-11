open Core
open Scanf

let n = scanf "%d" Fn.id
let a = Array.init n ~f:(fun _ -> scanf " %d" Fn.id)

let ans =
  Array.fold a ~init:0 ~f:(fun acc x ->
    match Array.binary_search a ~compare `Last_strictly_less_than (x * 2) with
    | None   -> acc
    | Some k -> acc + (n - k - 1)
  )

let () = printf "%d\n%!" ans
