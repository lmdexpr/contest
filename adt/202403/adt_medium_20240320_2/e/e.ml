open Core
open Scanf

let n = scanf "%d" Fn.id
let a = Array.init n ~f:(fun _ -> scanf " %Ld" Fn.id)

let sum = Array.sum (module Int64) a ~f:Fn.id

let x = scanf " %Ld" Fn.id

let rec solve ?(i=0) y =
  if Int64.(a.(i) > y) then Int64.of_int (i + 1)
  else solve ~i:(i+1) Int64.(y - a.(i))

let ans = Int64.(of_int n * (x / sum) + solve (x % sum))

let () = printf "%Ld\n%!" ans
