open Core
open Scanf

let h, w = scanf "%d %d" Tuple2.create

let s = Array.init h ~f:(fun _ -> scanf " %s" String.to_array)
let () =
  for i = 0 to h - 1 do
    for j = 0 to w - 2 do
      if Char.(s.(i).(j) = 'T' && s.(i).(j + 1) = 'T') then begin
        s.(i).(j) <- 'P';
        s.(i).(j+1) <- 'C'
      end
    done
  done

let () = Array.iter s ~f:(fun s -> Array.iter s ~f:(printf "%c"); printf "\n%!");
