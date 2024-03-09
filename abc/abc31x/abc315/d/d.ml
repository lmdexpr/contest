open Core
open Scanf

let h, w = scanf "%d %d" Tuple2.create

let c = Array.init h ~f:(fun _ -> scanf " %s" String.to_array)

let row = Array.make_matrix ~dimx:h ~dimy:26 0
let col = Array.make_matrix ~dimx:w ~dimy:26 0

let () =
  for i = 0 to h - 1 do
    for j = 0 to w - 1 do
      let c = Char.to_int c.(i).(j) - Char.to_int 'a' in
      row.(i).(c) <- row.(i).(c) + 1;
      col.(j).(c) <- col.(j).(c) + 1
    done
  done

let ans = 0

let () = printf "%d\n%!" ans
