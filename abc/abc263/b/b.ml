open Core
open Scanf

let n = scanf "%d" ident
let p = Array.init (n-1) ~f:(fun _ -> scanf " %d" ident)

let dp = Array.init (n+1) ~f:(fun _ -> 0)
let () = Array.iteri p ~f:(fun i x -> dp.(i+2) <- dp.(x) + 1)

let () = printf "%d\n%!" dp.(n)
