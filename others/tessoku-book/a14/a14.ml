open Core
open Scanf

let n = scanf " %d" Fn.id
let k = scanf " %d" Fn.id

let a = Array.init n ~f:(fun _ -> scanf " %d" Fn.id)
let b = Array.init n ~f:(fun _ -> scanf " %d" Fn.id)
let c = Array.init n ~f:(fun _ -> scanf " %d" Fn.id)
let d = Array.init n ~f:(fun _ -> scanf " %d" Fn.id)

let sums x y =
  Array.concat_map x ~f:(fun xi -> Array.map y ~f:(fun yi -> xi + yi))

let ab = sums a b
let cd = sums c d

let yes =
  Array.sort ab ~compare;
  Array.exists cd ~f:(fun sum_cd ->
    Array.binary_search ab ~compare `First_equal_to (k - sum_cd) |> Option.is_some
  )

let ans = if yes then "Yes" else "No"

let () = printf "%s\n%!" ans
