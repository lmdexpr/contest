open Core
open Scanf

let n = scanf "%d" Fn.id
let s = Array.init n ~f:(fun _ -> scanf " %s" Fn.id)

let hashtbl = Hashtbl.create (module String) ~size:n

let () =
  Array.iter s ~f:(fun s ->
    Hashtbl.update hashtbl s ~f:(function
      | Some c -> printf "%s(%d)\n" s c; c + 1
      | None   -> printf "%s\n" s; 1
    )
  )
