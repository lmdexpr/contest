open Core
open Scanf

let n = scanf "%d" Fn.id
let h = Array.init n ~f:(fun _ -> scanf " %d" Fn.id)

let ans =
  Array.findi h ~f:(fun _ hi -> h.(0) < hi)
  |> Option.value_map ~default:(-1) ~f:(fun (i, _) -> i + 1)

let () = printf "%d\n%!" ans
