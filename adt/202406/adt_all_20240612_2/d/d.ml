open Core
open Scanf

let h, w = scanf "%d %d" Tuple2.create

let a = Array.init h ~f:(fun _ -> Array.init w ~f:(fun _ -> scanf " %d" Fn.id))

let () = 
  for i1 = 0 to h - 1 do
    for i2 = i1 + 1 to h - 1 do
      for j1 = 0 to w - 1 do
        for j2 = j1 + 1 to w - 1 do
          if a.(i1).(j1) + a.(i2).(j2) > a.(i2).(j1) + a.(i1).(j2) then (
            printf "No\n";
            exit 0
          )
        done
      done
    done
  done;
  printf "Yes\n"
