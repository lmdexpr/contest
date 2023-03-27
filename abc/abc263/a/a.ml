open Core
open Scanf

let a = Array.init 5 ~f:(fun _ -> scanf " %d" ident)
let () = Array.sort ~compare a

let yes =
  (a.(0) = a.(1) && a.(1) = a.(2) && a.(3) = a.(4)) ||
  (a.(0) = a.(1) && a.(2) = a.(3) && a.(3) = a.(4))

let yes = if yes then "Yes" else "No"

let () = printf "%s\n%!" yes
