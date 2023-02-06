open Core
open Scanf

module SI = Set.Make(Int)
let inp = scanf "%d %d %d %d %d" @@ fun a b c d e -> [ a; b; c; d; e ]
let ans = SI.length @@ SI.of_list inp

let () = printf "%d\n%!" ans
