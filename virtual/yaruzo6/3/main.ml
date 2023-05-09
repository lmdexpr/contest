open Core
open Scanf

let s = scanf "%s" ident

let yes =
  let rec loop i j =
    if i >= j then true
    else
      match s.[i], s.[j] with
      | 'a', r when Char.(r <> 'a') -> false
      | l, 'a' when Char.(l <> 'a') -> loop i (j - 1)
      | l, r ->
        Char.(l = r) && loop (i + 1) (j - 1)
  in
  loop 0 (String.length s - 1)

let ans = if yes then "Yes" else "No"

let () = printf "%s\n%!" ans
