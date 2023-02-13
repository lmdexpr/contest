open Core
open Scanf

let n, m = scanf "%d %d" Tuple2.create

let a = Array.init m ~f:(fun _ -> scanf " %d" ident)

let rec solve ?(k=ignore) i j =
  if i > n then ()
  else if j < m && a.(j) = i then solve ~k:(fun _ -> printf "%d " i; k ()) (i + 1) (j + 1)
  else
    (printf "%d " i; k (); solve ~k:ignore (i + 1) j)

let () = solve 1 0
