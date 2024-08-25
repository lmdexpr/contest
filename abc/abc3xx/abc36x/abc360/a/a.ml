open Core
open Scanf

let s = scanf "%s" Fn.id

let r = String.findi s ~f:Char.(fun _ -> (=) 'R') |> Option.value_exn |> fst
let m = String.findi s ~f:Char.(fun _ -> (=) 'M') |> Option.value_exn |> fst

let ans = if r < m then "Yes" else "No"

let () = printf "%s\n%!" ans
