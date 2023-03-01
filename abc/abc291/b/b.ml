open Core
open Scanf

let n = scanf "%d" ident

let x = Array.init (5 * n) ~f:(fun _ -> scanf " %d" ident)
let () = Array.sort x ~compare

let ans =
  Iter.(n -- (4 * n - 1))
  |> Iter.map (Array.get x)
  |> Iter.sum
let ans =
  float ans /. 3. /. float n

let () = printf "%f\n%!" ans
