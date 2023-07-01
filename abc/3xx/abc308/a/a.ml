open Core
open Scanf

let s = Array.init 8 ~f:(fun i -> scanf (if i = 0 then "%d" else " %d") ident)

let yes =
  Array.for_all s ~f:(fun s -> 100 <= s && s <= 675 && s mod 25 = 0) &&
  Array.fold s ~init:(true, 0) ~f:(fun (acc, p) s -> acc && p <= s, s) |> fst

let ans = if yes then "Yes" else "No"

let () = printf "%s\n%!" ans
