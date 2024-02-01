open Core
open Scanf

let s = scanf "%s" Fn.id

let ans = String.split s ~on:'.' |> List.last_exn

let () = printf "%s\n%!" ans
