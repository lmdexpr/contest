open Core
open Scanf

let n = scanf "%d" Fn.id

let p = Array.init (n - 1) ~f:(fun _ -> scanf " %d" Fn.id)

let ans = Array.create ~len:(n+1) 0
let () = Array.iteri p ~f:(fun i p -> ans.(i + 2) <- ans.(p) + 1)

let () = printf "%d\n%!" ans.(n)
