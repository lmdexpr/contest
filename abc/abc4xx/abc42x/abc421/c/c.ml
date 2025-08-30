open Core
open Scanf

let n = scanf " %d" Fn.id
let s = scanf " %s" String.to_array

let a = Array.filter_mapi s ~f:(fun i -> function 'A' -> Some i | _ -> None)

let ans =
  min
    (Iter.(0 -- pred n) |> Iter.map (fun i -> abs @@ a.(i) - (2 * i + 1)) |> Iter.sum)
    (Iter.(0 -- pred n) |> Iter.map (fun i -> abs @@ a.(i) - 2 * i)       |> Iter.sum)

let () = printf "%d\n%!" ans
