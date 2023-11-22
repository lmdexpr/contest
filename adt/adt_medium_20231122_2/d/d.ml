open Core
open Scanf

let n = scanf "%d" Fn.id
let a = Array.init n ~f:(fun _ -> scanf " %d" Fn.id)

let cs = Array.folding_map a ~init:0 ~f:(fun acc x -> acc + x, acc + x)
let cs = Array.append [|0|] cs

let ans =
  Iter.(0 -- n) |> Iter.map (fun i -> cs.(n) - cs.(i))
  |> Iter.filter_count (fun x -> x >= 4)

let () = printf "%d\n%!" ans
