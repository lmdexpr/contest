open Core
open Scanf

let n = scanf "%d" ident

let w = Array.init n ~f:(fun _ -> scanf " %s" ident)

let ans = Array.find ~f:(List.mem ["and"; "not"; "that"; "the"; "you"] ~equal:String.(=)) w |> Option.is_some
let ans = if ans then "Yes" else  "No"

let () = printf "%s\n%!" ans
