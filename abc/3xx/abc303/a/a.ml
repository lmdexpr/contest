open Core
open Scanf

let _n = scanf "%d" ident
let s = scanf " %s" String.to_array
let t = scanf " %s" String.to_array

let yes =
  Array.zip_exn s t
  |> Array.fold ~init:true ~f:(fun acc -> function
      | _ when not acc -> acc
      | '1', 'l' | 'l', '1' -> true
      | '0', 'o' | 'o', '0' -> true
      | s, t -> Char.(s = t)
    )

let ans = if yes then "Yes" else "No"

let () = printf "%s\n%!" ans
