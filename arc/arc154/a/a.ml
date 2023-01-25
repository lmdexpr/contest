open Core
open Scanf

let n = scanf "%d" ident

let int64arr_of_string s =
  String.to_array s
  |> Array.map ~f:(fun c -> Char.to_int c - Char.to_int '0' |> Int64.of_int)

let a = scanf " %s" int64arr_of_string
let b = scanf " %s" int64arr_of_string

let modulo = 998244353L

let x = Array.init n ~f:(fun i -> Int64.min a.(i) b.(i))
let y = Array.init n ~f:(fun i -> Int64.max a.(i) b.(i))

open Int64

let int64_of_arr arr = Array.fold arr ~init:0L ~f:(fun acc x -> (acc * 10L + x) % modulo) % modulo

let ans = int64_of_arr x * int64_of_arr y % modulo

let () = printf "%Ld\n%!" ans
