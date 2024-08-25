open Core

let n, x = Scanf.scanf "%d %d" @@ fun n x -> n, x

let p = Array.init n ~f:(fun _ -> Scanf.scanf " %d" ident)
let [@warning "-8"] Some (i, _) = Array.findi p ~f:(fun _ e -> e = x)

let () = Printf.printf "%d\n" @@ i + 1 
