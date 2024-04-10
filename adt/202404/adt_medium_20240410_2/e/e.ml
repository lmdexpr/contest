open Core
open Scanf

let n, k = scanf "%d %d" Tuple2.create
let a = Array.init n ~f:(fun _ -> scanf " %d" Fn.id)

let a = Int.Set.of_array a |> Set.to_list |> Fn.flip List.take k |> Int.Set.of_list

let () =
  for i = 0 to k - 1 do
    if not @@ Set.mem a i then (
      printf "%d\n" i;
      exit 0
    )
  done;
  printf "%d\n" k
