open Core
open Scanf

let n = scanf "%s" String.to_list_rev

let n = List.drop_while n ~f:Char.(fun c -> c = '0') |> List.rev |> List.to_array

let yes =
  let rec loop i j =
    if i >= j then true
    else
      Char.(n.(i) = n.(j)) && loop (i + 1) (j - 1)
  in
  loop 0 (Array.length n - 1)

let ans = if yes then "Yes" else "No"

let () = printf "%s\n%!" ans
