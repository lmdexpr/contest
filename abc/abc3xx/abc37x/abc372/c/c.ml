open Core
open Scanf

let n, q = scanf "%d %d" Tuple2.create
let s    = scanf " %s" String.to_array

let contribute i c =
  match c with
  | 'A' -> i + 2 < n && Char.(s.(i + 1) = 'B' && s.(i + 2) = 'C')
  | 'B' -> 0 <= i - 1 && i + 1 < n && Char.(s.(i - 1) = 'A' && s.(i + 1) = 'C')
  | 'C' -> 0 <= i - 2 && Char.(s.(i - 2) = 'A' && s.(i - 1) = 'B')
  | _   -> false

let _ =
  Iter.(1 -- q)
  |> Iter.fold (fun count _ ->
    let x, c = scanf " %d %c" Tuple2.create in
    let x = x - 1 in
    let contributed     = contribute x s.(x) in
    let will_contribute = contribute x c in
    let count = count + Bool.to_int will_contribute - Bool.to_int contributed in
    s.(x) <- c;
    printf "%d\n%!" count;
    count
  ) @@ Array.counti s ~f:(fun i c ->
    i + 2 < n && Char.(c = 'A' && s.(i + 1) = 'B' && s.(i + 2) = 'C')
  )
