open Core

let n = Scanf.scanf "%d" ident
let s = List.init n ~f:(fun _ -> Scanf.scanf " %s" ident)

let () = List.iter s ~f:(printf "%s\n")
