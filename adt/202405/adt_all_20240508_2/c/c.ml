open Core
open Scanf

let n, m = scanf "%d %d\n" Tuple2.create
let a = Array.init m ~f:(fun _ -> scanf "%d " Fn.id) |> Int.Set.of_array

let rec solve i = function
  | hd :: tl as acc -> 
    if n < i then List.rev acc
    else
      if Set.mem a i then solve (i+1) ((i :: hd) :: tl)
      else
        solve (i+1) ([] :: (i :: hd) :: tl)
  | [] -> 
    failwith "unreachable"

let ans = solve 1 [[]]

let () =
  List.iter ans ~f:(List.iter ~f:(printf "%d "));
  printf "\n"
