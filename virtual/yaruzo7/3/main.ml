open Core
open Scanf

let s = scanf "%s" String.to_array |> Array.map ~f:(function '0' -> 0 | _ -> 1)

let n = Array.length s - 1

let zero = Iter.(0 -- n) |> Iter.map (fun i -> s.(i) =  i % 2) |> Iter.map Bool.to_int |> Iter.sum
let one  = Iter.(0 -- n) |> Iter.map (fun i -> s.(i) <> i % 2) |> Iter.map Bool.to_int |> Iter.sum
let ans  = min zero one

let () = printf "%d\n%!" ans
