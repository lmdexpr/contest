open Core
open Scanf

let n = scanf "%d" Fn.id
let a = Array.init n ~f:(fun _ -> scanf " %d" Fn.id)
let () = Array.sort a ~compare:Int.descending

let odd  = Array.filter a ~f:(fun x -> x % 2 = 1)
let even = Array.filter a ~f:(fun x -> x % 2 = 0)

let n_odd  = Array.length odd
let n_even = Array.length even

let ans =
  match 1 < n_odd, 1 < n_even with
  | false, false -> -1
  | true,  false -> odd.(0) + odd.(1)
  | false, true  -> even.(0) + even.(1)
  | true,  true  -> max (odd.(0) + odd.(1)) (even.(0) + even.(1))

let () = printf "%d\n%!" ans
