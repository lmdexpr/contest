open Core
open Scanf

let n = scanf "%d" ident

let yes = Iter.(1 -- 9) |> Iter.filter (fun i -> n % i = 0) |> Iter.exists (fun i -> n / i < 10)
let ans = if yes then "Yes" else "No"

let () = printf "%s\n%!" ans
