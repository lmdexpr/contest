open Core
open Scanf

let h, w = scanf "%d %d" Tuple2.create

let a = Array.init h ~f:(fun _ -> Array.init w ~f:(fun _ -> scanf " %d" ident))

let () =
  Array.iter a ~f:(fun a ->
      Array.iter a ~f:(function
          | 0 -> printf "."
          | n -> printf "%c" Char.(of_int_exn (n + to_int 'A' - 1))
        );
      printf "\n%!";
    )
