open Core
open Scanf

let n = scanf "%d" ident

let a = Array.init n ~f:(fun _ -> scanf " %d" ident)

let tbl = Hashtbl.create ~size:n (module Int)

let () = Array.iter a ~f:(fun x -> Hashtbl.incr tbl x)

let ans = Hashtbl.fold tbl ~init:0 ~f:(fun ~key:_ ~data ans -> ans + data / 2)

let () = printf "%d\n%!" ans
