open Core
open Scanf

let n = scanf "%d" Fn.id

let tbl = Hashtbl.create (module String)

let () =
  for _ = 1 to n do
    let s = scanf " %s" Fn.id in
    Hashtbl.update tbl s ~f:(function
      | None   -> printf "%s\n" s; 0
      | Some x -> printf "%s(%d)\n" s (x + 1); x + 1
    )
  done
