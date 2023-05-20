open Core
open Scanf

let n = scanf "%d" ident
let a = Array.init n ~f:(fun _ -> scanf " %d" ident)

let a = Array.filter a ~f:(fun e -> e % 2 = 0)

let () = Array.iter a ~f:(printf "%d "); printf "\n%!"
