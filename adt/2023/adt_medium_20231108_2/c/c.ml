open Core
open Scanf

let n = scanf "%d" Fn.id
let a = Array.init n ~f:(fun _ -> scanf " %d" Fn.id) |> Int.Set.of_array

let ans = Iter.(0 -- 2000) |> Iter.find_pred_exn (fun i -> not @@ Set.mem a i)

let () = printf "%d\n%!" ans
