open Core
open Scanf

let n, k = scanf "%d %d" Tuple2.create

let s = List.init n ~f:(fun _ -> scanf " %s" Fn.id) |> List.rev
let s = List.take s k |> List.sort ~compare:String.compare

let () = List.iter s ~f:(fun s -> printf "%s\n" s)
