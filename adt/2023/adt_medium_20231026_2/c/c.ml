open Core
open Scanf

let _ = scanf "%d" Fn.id
let t = scanf " %s" Fn.id

let x, y, _, _ =
  String.fold t ~init:(0, 0, 1, 0) ~f:(fun (x, y, dx, dy) -> function
    | 'S' -> x + dx, y + dy, dx, dy
    | _   -> x, y, dy, -dx
  )

let () = printf "%d %d\n%!" x y
