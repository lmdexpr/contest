open Core
open Scanf

let _ = scanf "%d"  Fn.id
let s = scanf " %s" Fn.id

let () =
  if
    String.for_all s ~f:Char.((=) '-') ||
    String.for_all s ~f:Char.((=) 'o')
  then
    printf "-1\n%!"
  else
    String.split_on_chars s ~on:['-']
    |> List.map ~f:(fun s -> String.length s)
    |> List.max_elt ~compare
    |> Option.value ~default:(-1)
    |> printf "%d\n%!"
