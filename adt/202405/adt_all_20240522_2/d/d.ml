open Core
open Scanf

let h, w = scanf "%d %d" Tuple2.create
let s = Array.init h ~f:(fun _ -> scanf " %s" String.to_array)

let sx, sy = ref (-1), ref (-1) 
let gx, gy = ref (-1), ref (-1)
let () =
  for i = 0 to h - 1 do
    for j = 0 to w - 1 do
      if Char.(s.(i).(j) = 'o') then
        if !sx = -1 then
          (sx := i; sy := j)
        else
          (gx := i; gy := j)
    done
  done
let sx, sy, gx, gy = !sx, !sy, !gx, !gy

let ans = abs (gx - sx) + abs (gy - sy)

let () = printf "%d\n%!" ans
