open Core
open Scanf

let n = scanf " %d" Fn.id

let a = Array.init n ~f:(fun _ -> scanf " %d" Fn.id)

let x = scanf " %d" Fn.id

let ans = Array.find a ~f:(fun ai -> ai = x) |> Option.is_some

let ans = if ans then "Yes" else "No"

let () = printf "%s\n%!" ans
