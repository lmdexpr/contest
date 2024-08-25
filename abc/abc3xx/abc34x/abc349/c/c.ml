open Core
open Scanf

let s,t = scanf "%s %s" Tuple2.create

let t = String.map t ~f:Char.lowercase

let no () = printf "No\n%!"; exit 0

let s = 
  match String.findi s ~f:Char.(fun _ c -> c = t.[0]) with
  | None        -> no ()
  | Some (i, _) -> String.drop_prefix s (i + 1)

let s = 
  match String.findi s ~f:Char.(fun _ c -> c = t.[1]) with
  | None        -> no ()
  | Some (i, _) -> String.drop_prefix s (i + 1)

let yes = 
  Char.(t.[2] = 'x') ||
  String.findi s ~f:Char.(fun _ c -> c = t.[2]) |> Option.is_some

let ans = if yes then "Yes" else "No"

let () = printf "%s\n%!" ans
