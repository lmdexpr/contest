open Core
open Scanf

let n = scanf "%d" ident

let a = Array.init n ~f:(fun _ -> scanf " %d" ident)

let f = function
  | None   -> 1
  | Some v -> v + 1
let m = Array.fold a ~init:Int.Map.empty ~f:(Int.Map.update ~f)

let () =
  Int.Map.to_alist ~key_order:`Decreasing m 
  |> List.iter ~f:(fun (_, d) -> printf "%d\n" d);
  for _ = 1 to n - Int.Map.length m do
    printf "0\n%!"
  done
