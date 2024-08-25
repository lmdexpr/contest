open Core
open Scanf

let h, w = scanf "%d %d" Tuple2.create

let c = Array.init h ~f:(fun _ -> scanf " %s" ident)

let () = 
  for j = 0 to w - 1 do
    Iter.(0 -- (h - 1))
    |> Iter.filter_count Char.(fun i -> c.(i).[j] = '#')
    |> printf "%d %!"
  done;
  printf "\n%!"
