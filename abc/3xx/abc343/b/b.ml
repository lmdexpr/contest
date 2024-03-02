open Core
open Scanf

let n = scanf "%d" Fn.id

let a = Array.init n ~f:(fun _ ->
  Array.init n ~f:(fun _ -> scanf " %d" Fn.id)
)

let () =
  for i = 0 to n - 1 do
    for j = 0 to n - 1 do
      if a.(i).(j) = 1 then
        printf "%d " (j + 1);
    done;
    printf "\n";
  done
