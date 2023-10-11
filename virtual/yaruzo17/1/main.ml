open Core
open Scanf

let p = scanf "%d" Fn.id

let rec fact = function
  | 0 -> 1
  | n -> n * fact (n - 1)
let coins = Array.init 11 ~f:fact

let _, ans = 
  Iter.(10 --^ 1)
  |> Iter.fold (fun (p, n) i -> p % coins.(i), p / coins.(i) + n) (p, 0)

let () = printf "%d\n%!" ans
