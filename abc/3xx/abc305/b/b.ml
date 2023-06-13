open Core
open Scanf

let c_to_i = function
  | 'A' -> 0
  | 'B' -> 1
  | 'C' -> 2
  | 'D' -> 3
  | 'E' -> 4
  | 'F' -> 5
  | 'G' -> 6
  | _   -> assert false

let p, q = scanf "%c %c" @@ fun p q -> c_to_i p, c_to_i q
let p, q = min p q, max p q - 1

let ans =
  Iter.(p -- q)
  |> Iter.map (Array.get [| 3; 1; 4; 1; 5; 9 |])
  |> Iter.sum

let () = printf "%d\n%!" ans
