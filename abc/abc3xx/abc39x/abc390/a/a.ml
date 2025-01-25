open Core
open Scanf

let a = Array.init 5 ~f:(fun _ -> scanf " %d" Fn.id)

let () =
  for i = 0 to 3 do
    Array.swap a i (i + 1);
    if Array.is_sorted a ~compare then (
      printf "Yes\n%!"; exit 0
    );
    Array.swap a i (i + 1);
  done;
  printf "No\n%!"
