open Core
open Scanf

let n = scanf " %d" Fun.id
let k = scanf " %d" Fun.id

let rec digit_sum = function
  | 0 -> 0
  | x -> x % 10 + digit_sum (x / 10)

let ans =
  Seq.init n succ
  |> Seq.filter (fun x -> digit_sum x = k)
  |> Seq.length

let () = printf "%d\n%!" ans
