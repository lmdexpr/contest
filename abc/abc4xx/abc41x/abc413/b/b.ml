open Core
open Scanf

let n = scanf " %d" Fn.id
let s = Array.init n ~f:(fun _ -> scanf " %s" Fn.id)

let ans =
  Iter.(0 -- pred n)
  |> Iter.flat_map (fun i ->
    Iter.(0 -- pred n)
    |> Iter.filter (fun j -> i <> j)
    |> Iter.map (fun j -> s.(i) ^ s.(j))
  )
  |> Iter.fold Set.add String.Set.empty
  |> Set.length

let () = printf "%d\n%!" ans
