open Core
open Scanf

let n, m = scanf "%d %d" Tuple2.create

let s = Array.init n ~f:(fun _ -> scanf " %s" @@ fun s -> sprintf "%c%c%c" s.[3] s.[4] s.[5])
let t = Array.init m ~f:(fun _ -> scanf " %s" Fn.id) |> String.Set.of_array

let ans = Array.count s ~f:(fun s -> Set.mem t s)

let () = printf "%d\n%!" ans
