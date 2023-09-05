open Core
open Scanf

let s = scanf "%s" String.to_list
let rec solve acc = function
  | []       -> List.rev acc
  | 'B' :: t -> solve List.(tl acc |> Option.value ~default:[]) t
  |   h :: t -> solve (h :: acc) t

let () =
  solve [] s |> List.iter ~f:(printf "%c");
  printf "\n"
