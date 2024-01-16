open Core
open Scanf

let h, w = scanf "%d %d" Tuple2.create
let s = Array.init h ~f:(fun _ -> scanf " %s" String.to_array)

let () =
  Array.iter s ~f:(fun s ->
    for j = 0 to w - 2 do
      if Char.(s.(j) = 'T' && s.(j+1) = 'T') then begin
        s.(j)   <- 'P';
        s.(j+1) <- 'C';
      end
    done
  )

let () = 
  Array.iter s ~f:(fun s -> Array.iter s ~f:(printf "%c"); printf "\n");
