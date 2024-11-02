open Core
open Scanf

let n = scanf "%d" Fn.id
let a = Array.init n ~f:(fun _ -> scanf " %d" Fn.id)

let map = Hashtbl.create (module Int)

let () =
  Array.iteri a ~f:(fun i x ->
    Hashtbl.update map x ~f:(function
      | None   -> printf "-1 "; i + 1
      | Some j -> printf "%d " j; i + 1
    )
  );
  printf "\n"
