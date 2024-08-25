open Core
open Scanf

let n = scanf "%Ld" Fn.id

open Int64

let () =
  if n = 1L then ( printf "0\n"; exit 0 ) 

let n = n - 1L

let number_of_palindrome_num k = 
  let x = of_int Int.(succ k) / 2L in
  Int64.(9L * 10L ** (x - 1L))

let n, k =
  Iter.(1 -- 35)
  |> Iter.fold_while Int64.(fun (n, _) k ->
    let num = number_of_palindrome_num k in
    if n < num then (n, k), `Stop else (n - num, k), `Continue
  ) (n, 0)

let ans = 
  let x = of_int Int.(succ k) / 2L in
  let y = 10L ** (x - 1L) + n - 1L in
  let y = to_string y in
  String.init k ~f:(fun i ->
    if of_int i < x then y.[i]
    else 
      y.[Int.(k - 1 - i)]
  )

let () =
  printf "%s\n" ans
