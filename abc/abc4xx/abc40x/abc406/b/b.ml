open Core
open Scanf

open Big_int

let n = scanf " %d" Fn.id
let k = scanf " %d" Fn.id
let a = Array.init n ~f:(fun _ -> scanf " %Ld" big_int_of_int64)

let m = power_int_positive_int 10 k

let one = big_int_of_int 1

let ans =
  Array.fold a ~init:one ~f:(fun acc x ->
    let acc = mult_big_int acc x in
    if le_big_int m acc then  one
    else
      acc
    )
  |> int64_of_big_int

let () = printf "%Ld\n%!" ans
