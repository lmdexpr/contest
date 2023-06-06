open Core
open Scanf

let n = scanf "%d" ident

let sa = Array.init n ~f:(fun _ -> scanf " %s %d" Tuple2.create)

let start, _ =
  sa
  |> Array.mapi ~f:(fun i (_, a) -> i, a)
  |> Array.min_elt ~compare:(fun (_, a) (_, b) -> Int.compare a b)
  |> Option.value ~default:(0, 0)

let () = 
  for i = start to n - 1 do
    printf "%s\n" (Tuple2.get1 sa.(i))
  done;
  for i = 0 to start - 1 do
    printf "%s\n" (Tuple2.get1 sa.(i))
  done
