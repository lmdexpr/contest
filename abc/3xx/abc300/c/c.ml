open Core
open Scanf

let h, w = scanf "%d %d" Tuple2.create
let c = Array.init h ~f:(fun _ -> scanf " %s" String.to_array)

let n = min h w
let s = Array.init (n+1) ~f:(const 0)

let rec check ?(size=1) i j =
  if
    0 <= i - size && i + size < h &&
    0 <= j - size && j + size < w &&
    Char.(c.(i + size).(j + size) = '#') &&
    Char.(c.(i + size).(j - size) = '#') &&
    Char.(c.(i - size).(j + size) = '#') &&
    Char.(c.(i - size).(j - size) = '#')
  then
    check ~size:(size + 1) i j
  else
    s.(size - 1) <- s.(size - 1) + 1

let () =
  for i = 0 to h - 1 do
    for j = 0 to w - 1 do
      if Char.(c.(i).(j) = '#') then
        check i j
    done
  done

let () =
  for i = 1 to n do
    printf "%d " s.(i);
  done;
  printf "\n%!"
