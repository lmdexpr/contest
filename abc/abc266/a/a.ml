open Core
open Scanf

let s = scanf "%s" ident

let () = printf "%c\n%!" @@ s.[String.length s / 2]
