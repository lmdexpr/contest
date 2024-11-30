open Core
open Scanf

let s = scanf "%s" Fn.id
let t = Hashtbl.create (module Char)
let () =
  String.iter s ~f:(fun c ->
    Hashtbl.update t c ~f:(function
      | None   -> 1
      | Some x -> x + 1
    )
  )

let ans = 
  Hashtbl.to_alist t
  |> List.find ~f:(fun (_, x) -> x = 1)
  |> Option.map ~f:fst

let () = 
  match ans with
  | None     -> printf "-1\n%!"
  | Some ans -> printf "%c\n%!" ans
