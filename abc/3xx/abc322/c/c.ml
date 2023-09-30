open Core
open Scanf

let n, m = scanf "%d %d" Tuple2.create

let a = Array.init m ~f:(fun _ -> scanf " %d" Fn.id)

let rec solve i day =
  if day = n then printf "0\n"
  else
    let next = a.(i) - day in
    printf "%d\n" next;
    solve (i + Bool.to_int (next = 0)) (day + 1)

let () = solve 0 1
